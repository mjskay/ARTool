# Internal helper functions for the art function 
# 
# Author: mjskay
###############################################################################


### Parses and validates model formula for art. 
### Raises exception if formula does not validate (e.g. not factorial).
### Returns list with:
###		fixed.only: formula with only fixed components
###		fixed.terms: additive formula with only fixed terms and no response 
###							(e.g. for use with ddply)
###    	fixed.term.labels:	character vector of term labels
###     all.terms: formula with response, fixed main effects, grouping factors (not in (1|x) form), 
###                and fixed interaction terms (in that order)                
parse.art.formula = function(formula) {
    #extract terms from the formula
    f.terms = terms(formula)
    f.variables = attr(f.terms, "variables")
    f.term.labels = attr(f.terms, "term.labels")
    f.factors = attr(f.terms, "factors")
    f.order = attr(f.terms, "order")
    f.response = attr(f.terms, "response")
    
    #ensure we have 1 independent variable
    if (attr(f.terms, "response") != 1) {
        stop("Model must have exactly one independent variable (got ", f.response, ")")
    }
    
    #ensure we have an intercept
    if (attr(f.terms, "intercept") == 0) {
        stop("Model must have an intercept (got ", attr(f.terms, "intercept"), ")")
    }
    
    #determine which terms on the right hand side are grouping terms
    is.grouping.variable = laply(as.list(f.variables), function(term) as.list(term)[[1]] == quote(`|`))
    
    #make a version of the formula terms with only fixed effects
    fixed.f.variables = f.variables[!is.grouping.variable]
    term_index.2 = length(f.term.labels) - length(is.grouping.variable) + 2	#TODO: go back and determine what this is 
    if (term_index.2 < 0) {
        #only happens when not factorial? TODO: why?
        stop("Model must include all combinations of interactions of fixed effects.")
    }
    term_index = c(!is.grouping.variable[-c(1,2)], rep(TRUE, term_index.2))
    if (length(f.factors) == 0) {
        #no effects => no fixed effects
        stop("Model must have at least one fixed effect (0 given)")
    }
    fixed.f.factors = f.factors[!is.grouping.variable[-1],term_index]
    if (length(fixed.f.factors) == 0) {
        #all effects are random => no fixed effects
        stop("Model must have at least one fixed effect (0 given)")
    }
    fixed.f.term.labels = f.term.labels[term_index]
    fixed.f.order = f.order[term_index]
    
    #ensure design of fixed effects portion of model has all interactions
    #first, pull out the response and the main (i.e. order-1) fixed effect terms 
    response.label = rownames(f.factors)[1]
    main.effects.labels = fixed.f.term.labels[fixed.f.order == 1]
    #build a factorial model of all fixed effects
    factorial.formula = eval(parse(text=paste(response.label,"~",do.call(paste, c(main.effects.labels, list(sep="*"))))))
    #verify the factorial model is the same as the fixed effects in the supplied model
    factorial.factors = attr(terms(factorial.formula), "factors")
    if (!all(dim(factorial.factors) == dim(fixed.f.factors)) || !all(factorial.factors == fixed.f.factors)) {
        stop("Model must include all combinations of interactions of fixed effects.")
    }
    
    #	#ensure grouping terms are intercept-only (TODO: is this necessary?)
    grouping.f.variables = f.variables[is.grouping.variable]
    #	if (!all(laply(as.list(grouping.f.variables), function(term) term[[2]] == 1))) {
    #		stop("All grouping variables must be intercept-only (e.g. (1|some.factor))")
    #	}
    
    #generate all-terms formula
    all.terms = factorial.formula
    #add grouping variables back in to formula as standlone terms (not in (1|x) form)
    for (rv in as.list(grouping.f.variables)) {
        all.terms = eval(parse(text=paste("update(all.terms, ~ . +", as.character(rv[[3]]), ")")))
    }
    
    #return validated formulas
    list(
        fixed.only=factorial.formula,
        fixed.terms=eval(parse(text=paste("~",do.call(paste, c(main.effects.labels, list(sep="+")))))),
        fixed.term.labels=fixed.f.term.labels,
        all.terms=all.terms,
        n.grouping.variables=sum(is.grouping.variable)
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
        cell.mean.df = ddply(data, term.names[term.index], function (df) {
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
