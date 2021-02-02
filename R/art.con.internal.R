### Gets variables from spec
### Makes sure op is the only operator in spec. Throws error if not.
### Returns vector containing all variables in spec
### spec is an expression (e.g., a:b:`var with spaces and : in it`)
### op is a string representation of the allowed operation (e.g., ":")
### Note: for our current purposes, we only have one validation operator for each situation
###   we would use this function in. Obviously, it would have to be re-written to accomodate more.
get.variables = function(spec, op) {
    if (is.call(spec) && spec[[1]] == op) {
        # recursive case: get variables from children
        c(get.variables(spec[[2]], op), get.variables(spec[[3]], op))
    } else if (is.name(spec)) {
        # base case: this is a variable
        # if recursive case is never called, need to make sure we still return a vector
        # if recursive case is called, it's fine, c(c(1), c(2)) = c(1,2)
        c(spec)
    } else {
        stop(paste("Contrast term can only contain variables and ", op, ".", sep="" ))
    }
}


### Parses and validates contrast formula of form  "a:b:c"
### Raises exception if formula does not validate.
### returns list with:
### interaction.variables: list of fixed variables (of type name) in interaction (e.g., list(a, b, c)).
### interaction.term.labels: quoted interaction term label (e.g. "a:b:c")
### concat.interaction.variable: concatenation (of type name) of all variables in interaction.variables (e.g., abc)
#' @importFrom stats terms
parse.art.con.string.formula = function(f.orig){

    # make sure f.orig is a single string (as opposed to a vector of multiple strings)
    # don't think this is actually needed. seems to get caught earlier by
    # "Contrast must either be formula of form ~ X1*X2*X3 or
    # term of form \"X1:X2:X3\")" error below
    if(!is.character(f.orig) || length(f.orig) != 1){
        stop("Contrast must be string term of form \"X1:X2\")")
    }

    # parse spec into an expression
    f.orig.expr = parse(text = f.orig, keep.source = FALSE)[[1]]
    variables = get.variables(f.orig.expr, ":")

    term.labels = f.orig

    # ensure we have exactly one interaction and no other terms
    if (length(term.labels) != 1) {
        stop("Model must have exactly one interaction and no other terms (" ,
             length(term.labels), " terms given)")
    }

    # Setup return list
    # interaction.term.label: string name for interaction term label (e.g. "a:b:c")
    interaction.term.label = term.labels
    # interaction.variables: list of fixed variables (e.g., list(a, b, c))
    interaction.variables = variables
    # concat.interaction.variable: string formula with all
    # interaction variables concatenated (e.g., abc)
    concat.interaction.variable = as.name(paste(interaction.variables, collapse=""))

    list(
        interaction.term.label = interaction.term.label,
        interaction.variables = interaction.variables,
        concat.interaction.variable = concat.interaction.variable
    )
}

### parses f.orig which is formula or term (f) passed to art.con or artlm.con
### f.orig can be of form ~ a*b*c or "a:b:c"
### if f.orig is of "formula" form (i.e., ~ a*b*c),
### converts it to "string" form (i.e., "a:b:c")
### returns: result of parse.art.con.string.formula when passed "string" version of f.orig
### list with
### interaction.variables: list of fixed variables (of type name) in interaction (e.g., list(a, b, c)).
### interaction.term.labels: quoted interaction term label (e.g. "a:b:c")
### concat.interaction.variable: concatenation (of type name) of all variables in interaction.variables (e.g., abc)
#' @importFrom stats as.formula
#' @importFrom plyr is.formula
parse.art.con.formula = function(f.orig){

    # looking for ~ a*b*c
    if(is.formula(f.orig)){
        # make sure only operator on rhs of original formula is "*"
        # e.g., f.orig = ~ a*b*c -> f.orig[[1]] = ~ and f.orig[[2]] = a*b*c
        if(f.orig[[1]] != "~"){
            stop("Left hand side of formula must be ~.")
        }

        # if there is a dependent variable (i.e., variable on lhs of ~), then f.orig will have length 3
        # otherwise, f.orig will have length 2
        # e.g., f.orig = Y ~ a*b*c -> f.orig[[1]] = ~, f.orig[[2]] = Y, f.orig[[3]] = a*b*c
        # e.g., f.orig = ~ a*b*c -> f.orig[[1]] = ~, f.orig[[2]] = a*b*c
        if (length(f.orig) > 2) {
            stop("Formula must not have any variables on LHS (got ", f.orig[[2]], "). ",
                 "Did you mean ", gsub(pattern = toString(f.orig[[2]]), x = deparse(f.orig), replacement = ''), "?")
        }

        # get formula variables (i.e., individual variables on lhs), and ensure "*" is the only RHS operator.
        # e.g., f.orig = ~ a*b*c -> f.orig[[1]] = `~`, f.orig[[2]] = a*b*c
        f.orig.expr = f.orig[[2]]
        variables = get.variables(f.orig.expr, "*")
        # stitch variables back together with : operator between each variable.
        # e.g. variables = c(a,b,c) -> a:b:c
        stiched.variables = Reduce(function(x, y) call(":", x, y), variables)
        # add "~" to lhs and turn into formula
        # e.g., stiched.variables = a:b:c -> ~a:b:c
        f = as.formula(call("~", stiched.variables))

        # extract terms from formula
        f.terms = terms(f)

        # char vector of names of rhs terms and their interactions
        # e.g. ~ a:b:c -> c("a:b:c"))
        term.labels = attr(f.terms, "term.labels")

        # don't think we need this.
        # I think it gets caught by
        # "Term or formula passed to art.con or artlm.con must contain a single
        # interaction term and no other terms, and the interaction term must be in art model formula."
        if (length(term.labels) != 1) {
            stop("Model must have exactly one interaction and no other terms (" ,
                 length(term.labels), " terms given)")
        }

        return(parse.art.con.string.formula(term.labels[[1]]))
    } # end f is formula

    # looking for "a:b:c"
    else if(is.character(f.orig) & length(f.orig) == 1){
        return(parse.art.con.string.formula(f.orig))
    }

    # Error if f is not formula or string
    else{
        stop("Contrast must either be formula of form ~ X1*X2*X3 or
         term of form \"X1:X2:X3\")")
    }
}

### Parses formula from supplied model
### Validates that interaction term from f is in model formula
### Does not validate model formula itself since model formula was validated when model was created
### Given a formula like response ~ a*b*c + (1|d) + Error(g)
### input:
### m.f is the formula for the original art model
### f.parsed is the already parsed contrast formula
### returns list with:
### response: response (type name) (e.g. response)
### fixed.variables: list of named fixed variables (of type name) (e.g. list(a, b, c))
### grouping.variables: list of grouping variables (of type name) (e.g. list(1|d))
### error.variables: list of error variables (of type name) (e.g. list(Error(g)))
parse.art.model.formula = function(m.f, f.parsed){

    # get model formula terms
    m.f.terms = terms(m.f)
    # char vector of names of rhs terms and their interactions
    # e.g. Response ~ a*b + (1|d) + Error(g) -> c("a", "b", "1 | d", "Error(g)", "a:b")) not necessarily in this order
    m.f.term.labels = attr(m.f.terms, "term.labels")

    # check if interaction term from f is in model formula
    f.interaction.term.label = f.parsed$interaction.term.label # f.parsed always has exactly one interaction
    is.interaction.term = grepl(f.interaction.term.label, m.f.term.labels)

    # if interaction term from f is not in model formula, error
    if(!any(is.interaction.term)){
        stop("Term or formula passed to art.con or artlm.con must contain a single interaction term and no other terms, and the interaction term must be in art model formula.")
    }

    # response ~ a*b*c*d + (1|d) + Error(g) -> list(response, a, b, c, d, 1|d, Error(g))
    m.f.variables.all = as.list(attr(m.f.terms, "variables"))[c(-1)]
    m.f.response = m.f.variables.all[[1]]
    m.f.variables = m.f.variables.all[c(-1)]

    #determine which variables on the rhs are grouping variables, error variables, or fixed variables
    is.grouping.variable = sapply(m.f.variables, function(term) as.list(term)[[1]] == quote(`|`))
    is.error.variable = sapply(m.f.variables, function(term) as.list(term)[[1]] == quote(`Error`))
    #all other variables that aren't grouping or error variables must be fixed variables
    is.fixed.variable = !(is.grouping.variable | is.error.variable)

    m.f.grouping = m.f.variables[is.grouping.variable]
    m.f.error = m.f.variables[is.error.variable]
    m.f.fixed = m.f.variables[is.fixed.variable] # remove response

    list(
        response = m.f.response,
        fixed.variables = m.f.fixed,
        grouping.variables = m.f.grouping,
        error.variables = m.f.error
    )
}

### creates new data frame which is a copy of df except
###   adds a new column by concatenated columns of df whose names are the interaction variables in f
###   removes columns whose names are the interaction variables in f
### m.formula is the original formula used to create the ART model
### df is the data frame used to creat the ART model
### formula is the contrast formula
generate.art.concatenated.df = function(m.f.parsed, df, f.parsed){

    # concatenate columns of data frame whose columns names are the variables in f.parsed.interaction.variables
    # e.g. abc
    f.concatenated.variable = f.parsed$concat.interaction.variable
    # e.g. list(a, b, c)
    f.interaction.variables = f.parsed$interaction.variables
    # turn list of names to vector of strings. e.g, aa = as.name("a"), bb = as.name("b"), list(aa, bb) -> c("a","b")
    art.con.df = df
    # unname throws error when only one interaction variable and we don't need to concatenate in that case
    # this is easier than debugging it
    if(length(f.interaction.variables) > 1){
        f.interaction.variables.string.vec = sapply(f.interaction.variables,deparse)
        art.con.df[[f.concatenated.variable]] = do.call(paste, c(unname(art.con.df[,f.interaction.variables.string.vec]), sep = ","))
        art.con.df[,f.interaction.variables.string.vec] = NULL
    }
    # note: when m was created would have thrown error if a fixed var column in df was not a factor
    art.con.df[[f.concatenated.variable]] = factor(art.con.df[[f.concatenated.variable]])
    art.con.df
}

### removes variables from the model formula (m.f) that are in the contrast formula (f)
### replaces them with the new concatenated (the concatenation of all variables in the contrast formula)
### creates and returns art model on art.concatenated.df using the created formula
### note: this is not the same as using the full factorial model of all columns in df
###       there can be columns in df that are not used in the model.
generate.art.concatenated.model = function(m.f, m.f.parsed, art.concatenated.df, f.parsed){

    # fixed variables in model formula
    m.f.fixed.variables = m.f.parsed$fixed.variables
    # interaction variables
    f.interaction.variables = f.parsed$interaction.variables
    # concatenated interaction variable
    f.concat.interaction.variable = f.parsed$concat.interaction.variable

    # indices of interaction variables in model formula fixed variable list
    # e.g. f.interaction.variables = (a, c) and m.f.fixed.variables = (a, b, c) -> c(1, 3)
    interaction.variable.index = match(f.interaction.variables, m.f.fixed.variables)
    # remove interaction variables from model formula
    # e.g. f.interaction.variables = (a, c) and m.f.fixed.variables = (a, b, c) -> list(b)
    m.f.fixed.variables.no.interaction.vars = m.f.fixed.variables[-interaction.variable.index]
    m.f.fixed.variables.with.concat = c(m.f.fixed.variables.no.interaction.vars, f.concat.interaction.variable)

    # create formula with response m.f.response, fixed vars m.f.fixed.variables.with.concat
    # grouping variables m.f.grouping.variables, error variables m.f.error.variables
    m.f.response = m.f.parsed$response
    m.f.grouping.variables = m.f.parsed$grouping.variables
    m.f.error.variables = m.f.parsed$error.variables

    # collapse fixed variable vector into string separated by *
    # e.g. list(ac,b) -> "ac*b"
    m.f.fixed.str = paste(m.f.fixed.variables.with.concat, collapse = "*")
    # add parentheses around each grouping variable and
    # collapse grouping variables vector into string separated by +
    # e.g list(1|d, 1|e) -> "(1|d) + (1|e)"
    # note: empty list coerced to empty vector i.e list() -> character(0)
    m.f.grouping.paren = if(length(m.f.grouping.variables) == 0) character() else paste("(", m.f.grouping.variables, ")", sep="")
    m.f.grouping.str = if(length(m.f.grouping.paren) == 0) character() else paste(m.f.grouping.paren, collapse = "+")
    # collaprse error variables vector into string separated by +
    # e.g. list(Error(g), Error(h)) -> "Error(g) + Error(h)"
    # note: empty list coerced to empty vector i.e list() -> character(0)
    m.f.error.str = if(length(m.f.error.variables) == 0) character() else paste(m.f.error.variables, collapse = "+")

    # assemble concatenated art formula
    # some terms may be missing (e.g., no goruping term). remove those from vector
    # because weird things happen when pasting multiple strings together and one is empty
    strings.to.join = c(m.f.fixed.str, m.f.grouping.str, m.f.error.str)
    nonempty.strings.to.join = Filter(function(x) x!="", strings.to.join)
    art.concatenated.formula.rhs.str = paste(nonempty.strings.to.join, collapse="+")
    art.concatenated.formula = as.formula(paste(m.f.response, art.concatenated.formula.rhs.str, sep=" ~ "))
    # make art concatenated model
    m = art(art.concatenated.formula, data=art.concatenated.df)
    m
}

### called internally from artlm.con.
### aligns-and-ranks data in m with ART-C procedure
### creates linear model, linear mixed model, or aov model depending on grouping terms in m.f
### and returns resulting model
### m: art model passed into art.con
### f.parsed: parsed contrast formula
### response: "aligned" for compare aligned responses or "art" for compare aligned-and-ranked responses
### factor.contrasts: e.g. contr.sum passed to artlm.
### ...: extra parameter passed to artlm and subsequently lm or lmer
### returns: An object of class lm if m.f does not
### contain grouping or error terms, an object of class merMod
###  (i.e. a model fit by lmer) if it contains grouping terms, or
###  an object of class aovlist (i.e. a model fit by aov) if
###  it contains error terms.
### Note: only allowed from artlm.con not art.con.
artlm.con.internal = function(m, f.parsed, response, factor.contrasts, ...){
    # make sure m is an art model
    if (!inherits(m, "art")) {
        stop("Model must be an art model, got ", deparse1(class(m)), ".")
    }
    # get model formula
    m.f = m$formula
    # m$data holds data used to create m, even if data frame var name was assigned to something else.
    df = m$data
    # parse model formula
    m.f.parsed = parse.art.model.formula(m.f, f.parsed)
    # concatenarte columns in df corresponding to interaction terms in f
    art.concatenated.df = generate.art.concatenated.df(m.f.parsed, df, f.parsed)
    # concatenate terms in m.f correcsponding to interaction terms in f and create new art model
    art.concatenated.model = generate.art.concatenated.model(m.f, m.f.parsed, art.concatenated.df, f.parsed)
    # artlm with new art model
    artlm.con.internal = artlm(art.concatenated.model, toString(f.parsed$concat.interaction.variable), response = response, factor.contrasts=factor.contrasts, ...)
    artlm.con.internal
}

### called internally from art.con iff interaction = TRUE
### m: art model passed into art.con
### f.parsed: parsed contrast formula
### response: "aligned" for compare aligned responses or "art" for compare aligned-and-ranked responses
### factor.contrasts: e.g. contr.sum passed to artlm.
### method: e.g. pairwise. passed to contrast
### adjust: e.g. tukey. passed to contrast
### ...: extra parameter passed to artlm and subsequently lm or lmer
### returns: result of conducting interaction contrasts on terms specified in f.parsed
###          (object of class emmGrid)
do.art.interaction.contrast = function(m, f.parsed, response, factor.contrasts, method, adjust, ...){
    # e.g. list("a", "b", "c")
    interaction.variables = f.parsed$interaction.variables
    # e.g. list("a", "b", "c") -> "a:b:c". will be passed to artlm
    interaction.string.term = paste(interaction.variables, collapse=":")
    # e.g. list("a", "b", "c") -> "~ a*b*c". will be passed to emmeans
    interaction.string.formula = paste("~", paste(interaction.variables, collapse="*"), sep = "")
    interaction.formula = as.formula(interaction.string.formula)

    contrast(emmeans(artlm(m, interaction.string.term, response = response, factor.contrasts = factor.contrasts, ...),
                     interaction.formula), method=method, adjust=adjust, interaction=TRUE)
}

### conducts contrasts given model returned by artlm.con
### f.parsed: parsed contrast formula
### artlm.con: model returned by artlm.con given the original inputs to art.con
### method: contrast method propogated to contrast
### adjust: adjustment method propogated to contrast
### returns: result of conducting contrasts on artlm.con model (object of class emmGrid)
### syntax: m = art(Y ~ X1*X2, data = df)
###         art.con(m, "X1") or art.con(m, ~X1)
###         art.con(m, "X1:X2") or art.con(m, ~ X1*X2)
### Note: called internally from art.con iff interaction = FALSE
#' @importFrom stats p.adjust p.adjust.methods
#' @importFrom emmeans emmeans contrast
do.art.contrast = function(f.parsed, artlm.con, method, adjust){
    # e.g. f.parsed$concat.interaction.variable = X1X2 -> "~ X1X2"
    emmeans.str = paste(" ~ ", toString(f.parsed$concat.interaction.variable))
    # e.g. "~ X1X2" -> ~ X1X2
    emmeans.formula = as.formula(emmeans.str)
    art.con.emmeans = emmeans(artlm.con, emmeans.formula)
    art.con = contrast(art.con.emmeans, method, adjust=adjust)
    art.con
}
