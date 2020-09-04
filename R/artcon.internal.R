

do.art.interaction.contrast = function(m, f.parsed, response, factor.contrasts, method, adjust){
  # e.g. list("a", "b", "c")
  interaction.variables = f.parsed$interaction.variables
  # e.g. list("a", "b", "c") -> "a:b:c". will be passed to artlm
  interaction.string.term = paste(interaction.variables, collapse=":")
  # e.g. list("a", "b", "c") -> "~ a*b*c". will be passed to emmeans
  interaction.string.formula = paste("~", paste(interaction.variables, collapse="*"), sep = "")
  interaction.formula = as.formula(interaction.string.formula)
  
  contrast(emmeans(artlm(m, interaction.string.term, response = response, factor.contrasts = factor.contrasts),
                   interaction.formula), method=method, adjust=adjust, interaction=TRUE)
}

### Parses and validates model formula for art contrast. 
### Raises exception if formula does not validate.
### Given a formula like pairwise ~ a*b*c
### returns list with:
### interaction.variables: list of fixed variables (of type name) in interaction (e.g., list(a, b, c)).
### interaction.term.labels: quoted interaction term label (e.g. "a:b:c")
### concat.interaction.variable: concatenation (of type name) of all variables in interaction.variables (e.g., abc)
#' @importFrom stats terms
#' @importFrom pryr substitute_q
#' @importFrom stringr str_replace
# validate and parse formulas of the of "a:b:c"
parse.art.con.string.formula = function(f.orig){
  
  # check if f.orig is a single string (as opposed to a vector of multiple strings)
  if(!is.character(f.orig) || length(f.orig) != 1){
    stop("Contrast must be string term of form \"X1:X2\")")
  }
  
  # don't allow spaces. must be "a:b", otherwise hard to parse
  f.orig.has.space = grepl(' ', f.orig, fixed=TRUE)
  if(f.orig.has.space){
    stop("Contrast term cannot have a space in it.")
  }
  
  # e.g. "a:b:c" -> list("a", "b", "c")
  variables.str = strsplit(f.orig, ":")[[1]]
  # e.g. list("a", "b", "c") -> list (a, b, c)
  variables = lapply(variables.str, function(term) as.name(term))
  
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

# f.orig can be ~ a*b*c or "a:b:c"
#' @importFrom stats as.formula
#' @importFrom plyr is.formula
parse.art.con.formula = function(f.orig){
  
  # looking for ~ a*b*c
  if(is.formula(f.orig)){
    # make sure : not in original formula
    f.orig.str = deparse(f.orig)
    f.orig.has.colon = grepl(':', f.orig.str, fixed=TRUE)
    
    if(f.orig.has.colon){
      stop("Formula cannot contain : Did you mean ", as.formula(str_replace(f.orig.str, ':', ' * ')), 
           " or ", str_replace(f.orig.str, '~', ''),"?")
    }
    
    # Replace * with :
    # e.g.  ~ a*b*c ->  ~ a:b:c
    f = as.formula(pryr::substitute_q(f.orig, list('*' = as.name(':'))))
    
    # extract terms from formula
    f.terms = terms(f)
    
    # unique variables in the formula as list of names
    #e.g. ~ a:b:c -> list(a, b, c)
    variables = as.list(attr(f.terms, "variables"))[c(-1)]
    
    # ensure no dependent variable
    # the index of the variable (in variables) of the response. Zero if there is no response.
    # e.g. ~ a:b:c -> 0
    # if there was lhs: e.g. pairwise ~ a:b:c -> 1
    f.response = attr(f.terms, "response")
    if (f.response != 0) {
      stop("Formula must not have any variables on LHS (got ", variables[[1]], ").\n",
           "Did you mean ", str_replace(f.orig.str, toString((variables[[1]])), ''))
    }
    
    # char vector of names of rhs terms and their interactions
    # e.g. ~ a:b:c -> c("a:b:c"))
    term.labels = attr(f.terms, "term.labels")
    
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
### returns list with:
### response: response (type name) (e.g. response)
### fixed.variables: list of named fixed variables (of type name) (e.g. list(a, b, c)
### grouping.variables: list of grouping variables (of type name) (e.g. list(1|d))
### error.variables: list of error variables (of type name) (e.g. list(Error(g)))
#' @importFrom plyr laply
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
  # TODO FIX
  if(!any(is.interaction.term)){
    stop("Term or formula passed to artcon or artlm.con must contain a single interaction term and no other terms, and the interaction term must be in art model formula.")
  }
  
  # response ~ a*b*c*d + (1|d) + Error(g) -> list(response, a, b, c, d, 1|d, Error(g))
  m.f.variables.all = as.list(attr(m.f.terms, "variables"))[c(-1)]
  m.f.response = m.f.variables.all[[1]]
  m.f.variables = m.f.variables.all[c(-1)]
  
  #determine which variables on the rhs are grouping variables, error variables, or fixed variables
  is.grouping.variable = laply(m.f.variables, function(term) as.list(term)[[1]] == quote(`|`))
  is.error.variable = laply(m.f.variables, function(term) as.list(term)[[1]] == quote(`Error`))
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

# creates new data frame which is a copy of df except
#   adds a new column by concatenated columns of df whose names are the interaction variables in f
#   removes columns whose names are the interaction variables in f
# m.formula is the original formula used to create the ART model
# df is the data frame used to creat the ART model
# formula is the contrast formula
#' @importFrom tidyr unite_
generate.art.concatenated.df = function(m.f.parsed, df, f.parsed){
  
  # concatenate columns of data frame whose columns names are the variables in f.parsed.interaction.variables
  # e.g. abc
  f.concatenated.variable = f.parsed$concat.interaction.variable
  # e.g. list(a, b, c)
  f.interaction.variables = f.parsed$interaction.variables
  art.con.df = unite_(df, f.concatenated.variable, f.interaction.variables, sep = ",", remove = TRUE) # concat columns and remove originals
  # note: when m was created would have thrown error if a fixed var column in df was not a factor
  art.con.df[[f.concatenated.variable]] = factor(art.con.df[[f.concatenated.variable]])
  art.con.df
}

# removes variables from the model formula (m.f) that are in the contrast formula (f)
# replaces them with the new concatenated (the concatenation of all variables in the contrast formula)
# note: this is not the same as using the full factorial model of all columns in df
#       there can be columns in df that are not used in the model.
#' @importFrom stringi stri_join
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
  art.concatenated.formula.rhs.str = stri_join(m.f.fixed.str, m.f.grouping.str, m.f.error.str, sep="+", ignore_null= TRUE)
  art.concatenated.formula = as.formula(paste(m.f.response, art.concatenated.formula.rhs.str, sep=" ~ "))
  # make art concatenated model
  m = art(art.concatenated.formula, data=art.concatenated.df)
  m
}

# TODO COMMENTS
# ... only allowed from artlm.con not artcon.
artlm.con.internal = function(m, f.parsed, response, factor.contrasts, ...){
  # make sure m is an art model
  if(class(m) != "art"){
    stop("Model must be an art model, got ", class(m), ".")
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

# TODO COMMENTS
# contrast(emmeans(artlm(m.art, "X1:X2"), ~ X1:X2), method="pairwise")
# We currently only use contrast "contr.sum" for this
#' @importFrom stats p.adjust p.adjust.methods
#' @importFrom emmeans emmeans contrast
do.art.contrast = function(f.parsed, artlm.con, method, adjust){
  # TODO add example comment
  emmeans.str = paste(" ~ ", toString(f.parsed$concat.interaction.variable))
  emmeans.formula = as.formula(emmeans.str)
  art.con.emmeans = emmeans(artlm.con, emmeans.formula)
  art.con = contrast(art.con.emmeans, method, adjust=adjust)
  art.con
}