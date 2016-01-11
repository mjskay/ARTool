# Internal helper functions for the art function 
# 
# Author: mjskay
###############################################################################


### Parses and validates model formula for art. 
### Raises exception if formula does not validate (e.g. not factorial).
### Given a formula like y ~ a*b*c + (1|d) + Error(g * h) + Error(i),
### returns list with:
###		fixed.only: formula with only fixed components (y ~ a+b+c)
###		fixed.terms: additive formula with only variables from fixed terms and no response 
###							(for use with ddply) (e.g., ~ a + b + c)
###    	fixed.term.labels:	character vector of term labels (e.g., c("a", "b", "c", "a:b", "a:c", "b:c", "a:b:c"))
###     n.grouping.terms: number of grouping terms like (1|d) (e.g. 1)
###     n.error.terms: number of error terms like Error(g) (e.g. 2)
###     error.terms: formula with error terms extracted from within Error() (e.g. ~ g * h + i) 
#' @importFrom stats terms
#' @importFrom plyr laply
parse.art.formula = function(formula) {
    #extract terms from the formula
    f.terms = terms(formula)
    
    #ensure we have an independent variable and an intercept
    if (attr(f.terms, "response") != 1) {
        stop("Model must have exactly one dependent variable (got ", attr(f.terms, "response"), ")")
    }
    if (attr(f.terms, "intercept") == 0) {
        stop("Model must have an intercept (got ", attr(f.terms, "intercept"), ")")
    }
    
    #unique variables in the rhs of the formula as list of quoted variables 
    #e.g. y ~ a*b*c + (1|d) + Error(g) -> list(quote(a), quote(b), quote(c), quote((1|d)), quote(Error(g))))
    variables = as.list(attr(f.terms, "variables"))[c(-1,-2)] 
    #char vector of names of rhs terms and their interactions
    #e.g. y ~ a*b*c + (1|d) + Error(g) -> c("a","b","c","1 | d","Error(g)","a:b","b:c","a:b:c"))  
    term.labels = attr(f.terms, "term.labels")
    #vector of length(f.term.labels); value is the order of the interaction of the corresponding entry in term.labels
    #e.g. y ~ a*b*c + (1|d) + Error(g) -> c(1,1,1,1,1,2,2,3)  
    term.order = attr(f.terms, "order")
    
    #determine which variables on the rhs are grouping variables, error variables, or fixed variables
    is.grouping.variable = laply(variables, function(term) as.list(term)[[1]] == quote(`|`))
    is.error.variable = laply(variables, function(term) is.call(term) & as.list(term)[[1]] == quote(`Error`))
    #all other variables that aren't grouping or error variables must be fixed variables
    is.fixed.variable = !(is.grouping.variable | is.error.variable)

    #ensure we have at least one fixed effect and are using either grouping terms or error terms but not both
    if (sum(is.fixed.variable) == 0) {
        stop("Model must have at least one fixed effect (0 given)")
    }
    if (any(is.grouping.variable) & any(is.error.variable)) {
        stop("Model cannot contain both grouping terms, like (1|d), and error terms, like Error(d). Use one or the other.")
    }
    
    #get table with rows == rhs variables and cols == term labels, each cell == 1 if variable in term
    variables.by.terms = attr(f.terms, "factors")[-1,,drop=FALSE] #prevent reducing to vector if only one cell
    
    #make a version of the formula terms with only fixed effects
    n.rhs.variables = length(variables)
    n.rhs.terms = length(term.labels)
    n.interaction.terms = n.rhs.terms - n.rhs.variables
    if (n.interaction.terms < 0) {
        #only happens when not factorial
        stop("Model must include all combinations of interactions of fixed effects.")
    }
    is.fixed.term = c(is.fixed.variable, rep(TRUE, n.interaction.terms))
    fixed.variables.by.terms = variables.by.terms[is.fixed.variable, is.fixed.term, drop=FALSE]
    fixed.term.labels = term.labels[is.fixed.term]
    fixed.term.order = term.order[is.fixed.term]
    
    #ensure design of fixed effects portion of model has all interactions
    #first, pull out the response and the main (i.e. order-1) fixed effect terms 
    response = formula[[2]]
    #build a factorial model of all fixed effects
    #e.g. y ~ a*b*c + (1|d) + Error(g) -> y ~ a*b*c
    factorial.formula = eval(bquote(.(response) ~ .(Reduce(function(x,y) bquote(.(x) * .(y)), variables[is.fixed.variable]))))
    environment(factorial.formula) = environment(formula)
    #verify the factorial model is the same as the fixed effects in the supplied model
    factorial.factors = attr(terms(factorial.formula), "factors")[-1,,drop=FALSE]
    if (!all(dim(factorial.factors) == dim(fixed.variables.by.terms)) || !all(factorial.factors == fixed.variables.by.terms)) {
        stop("Model must include all combinations of interactions of fixed effects.")
    }

    #build a formula with only fixed variables on the right-hand-side (added to each other)
    #e.g. y ~ a*b*c + (1|d) + Error(g) ->  ~ a + b + c
    fixed.terms = eval(bquote(~ .(Reduce(function(x,y) bquote(.(x) + .(y)), variables[is.fixed.variable]))))
    environment(fixed.terms) = environment(formula)

    #build a formula with all Error terms extracted from Error() on the right-hand side (added to each other)
    #e.g. y ~ a*b*c + (1|d) + Error(g * h) + Error(i) ->  ~ g * h + i
    error.terms = eval(bquote(~ .(Reduce(function(x,y) bquote(.(x) + .(y)), Map(function (v) v[[2]], variables[is.error.variable])))))
    environment(error.terms) = environment(formula)
    
    #return validated formulas
    list(
        fixed.only = factorial.formula,
        fixed.terms = fixed.terms,
        fixed.term.labels = fixed.term.labels,
        n.grouping.terms = sum(is.grouping.variable),
        n.error.terms = sum(is.error.variable),
        error.terms = error.terms
    )
}


### Given a factorial, fixed-effects-only formula,
### calculate the cell means and estimated effects
### for all responses. Returns a list of three
### data frames all indexed in parallel: data 
### (the original data as determined by 
### model.frame(formula, data)),
### cell.means, and estimated.effects
###
### Given some formula f and an input data frame df,
### parameters to art.estimated effects are:
### formula.terms = terms(f)
### data = model.frame(f, df)
#' @importFrom plyr ddply
art.estimated.effects = function(formula.terms, data) {
    #N.B. in this method "interaction" refers to 
    #all 0 - n order interactions (i.e., grand mean, 
    #first-order/"main" effects, and 2+-order interactions)
    
    #matrix with interactions as columns
    #and the response + all first-order factors as rows, 
    #with each cell indicating if the first-order factor (row) 
    #contributes to the nth-order interaction (column)
    interaction.matrix = cbind(data.frame(.grand=FALSE), attr(formula.terms, "factors") == 1)
    interaction.names = colnames(interaction.matrix)
    term.names = row.names(interaction.matrix)
    #interaction order of each column in interaction.matrix
    #(order of grand mean (first column) is 0, main effects
    #are 1, n-way interactions are n) 
    interaction.order = c(0, attr(formula.terms, "order"))
    
    #calculate cell means for each interaction
    cell.means = data.frame(y=rep(mean(data[,1]), nrow(data)))
    colnames(cell.means) = term.names[1]
    data$.row = 1:nrow(data)	#original row indices so we can keep rows in order when we split/combine	
    for (j in 2:ncol(interaction.matrix)) {
        term.index = interaction.matrix[,j]
        #calculate cell means
        #must dervie term.formula as below (instead of just passing term.names[term.index] to ddply)
        #because otherwise expressions like "factor(a)" would be converted to ~ factor(a) (instead of ~ `factor(a)`, which
        #is what we want here because the expression has already been evaluated previously)
        #A nicer way to do all this would be good to come up with eventually  
        term.formula = eval(bquote(~ .(Reduce(function(x,y) bquote(.(x) + .(y)), Map(as.name, term.names[term.index])))))
        cell.mean.df = ddply(data, term.formula, function (df) {
            df$.cell.mean = mean(df[,1])	#mean of response for this interaction
            df
        })
        #put results into cell.means in the order of the original rows (so that they match up)
        cell.means[[interaction.names[j]]] = cell.mean.df[order(cell.mean.df$.row), ".cell.mean"]
    }
    
    #calculate estimated effects for each interaction
    estimated.effects = data.frame(y=cell.means[,1])	#estimated effect for grand mean == grand mean
    colnames(estimated.effects) = term.names[1]
    for (j in 2:ncol(interaction.matrix)) {
        #index of which cell means (columns of cell.means)
        #contribute to the estimated effect of this interaction 
        cell.means.cols = colSums(interaction.matrix[interaction.matrix[,j],]) > 0 &	#any that involve the same factors 
            interaction.order < interaction.order[j]				#and which are of lower order
        #also include grand mean and cell mean of this interaction in estimated effect
        cell.means.cols[1] = TRUE
        cell.means.cols[j] = TRUE
        #cell means contribute positively if they have the same 
        #order (mod 2) as this interaction and negatively otherwise
        cell.means.multiplier = ifelse((interaction.order - interaction.order[j]) %% 2, -1, 1)
        
        #calculate estimated effect
        estimated.effects[[interaction.names[j]]] = 
            rowSums(t(t(cell.means[,cell.means.cols]) * cell.means.multiplier[cell.means.cols]))
    }
    estimated.effects[1] = NULL #drop first column (grand mean), not needed
    
    
    list(
        cell.means=cell.means,
        estimated.effects=estimated.effects
    )
}
