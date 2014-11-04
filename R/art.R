# art function and some basic generic function implementations for art objects,
# such as print.art and summary.art
# 
# Author: mjskay
###############################################################################


art = function(formula, data,
    #number of digits to round aligned responses to before ranking (to ensure ties are computed consistently)
    rank.comparison.digits = -floor(log10(.Machine$double.eps ^ 0.5)) 
) {
    #parse and validate formula
    f = parse.art.formula(formula)
    
    #generate base model data frame (fixed effects only)
    if (missing(data)) {
        data = environment(formula)
    }
    #get data frame from input formula and data
    #first col will be response, followed by fixed effects
    df = model.frame(f$fixed.only, data, na.action=function(object) {
        #verify that all cases are complete (no NAs) 
        #TODO: add na.rm or na.action support
        if (!all(complete.cases(object))) {
            stop("Aligned Rank Transform cannot be performed when fixed effects have missing data (NAs).")
        }
        object
    })
    
    #verify that the reponse is numeric or ordinal and translate to ordinal
    if (!is.numeric(df[,1]) && !is.ordered(df[,1]) && !is.logical(df[,1])) {
        stop("Reponse term must be numeric, ordered factor, or logical (it was ", do.call(paste, as.list(class(df[,1]))), ")")
    }
    #coerce response to numeric for processing
    df[,1] = as.numeric(df[,1]) 
    
    #verify that all fixed effects are factors #TODO: can these be ordered factors?
    if (!all(laply(df[,-1], function(col) is.factor(col) || is.logical(col)))) {
        stop("All fixed effect terms must be factors or logical (e.g. not numeric).")
    }
    #coerce fixed effects to numeric for processing
    for (j in 2:ncol(df)) {
        df[,j] = as.numeric(df[,j])
    }
    
    #calculate cell means and estimated effects
    m = art.estimated.effects(terms(f$fixed.only), df)
    
    #calculate residuals (response - cell mean of highest-order interaction)
    m$residuals = df[,1] - m$cell.means[,ncol(m$cell.means)]
    
    #calculate aligned responses
    m$aligned = m$residuals + m$estimated.effects
    
    #compute aligned and ranked responses
    m$aligned.ranks = data.frame(llply(round(m$aligned, rank.comparison.digits), rank), check.names=FALSE)
    
    class(m) = "art"
    m$formula = formula
    m$call = match.call()
    m$data = data
    m$n.grouping.variables = f$n.grouping.variables
    m
}

summary.art = function(object, ...) {
    #sensible names for generic parameters
    m = object
    
    #verify that aligned responses sum to 0 (using fuzzy compare)
    m$aligned.col.sums = colSums(m$aligned) 
    if (!isTRUE(all.equal(as.vector(m$aligned.col.sums), rep(0, length(m$aligned.col.sums))))) {
        stop("Aligned responses do not sum to ~0. ART may not be appropriate.")
    }
    
    #verify that F values of ANOVA are all ~0 (using fuzzy compare)
    m$aligned.anova = anova(m, response="aligned")
    if (!isTRUE(all.equal(m$aligned.anova$F, rep(0, length(m$aligned.anova$F))))) {
        warning("F values of ANOVAs on aligned responses not of interest are not all ~0. ART may not be appropriate.")
    }
    
    class(m) = c("summary.art", class(m))
    m
}

print.art = function(x, ...) print(summary(x), ...)

print.summary.art = function(x, 
    #number of digits to display (based on tolerance used for fuzzy compare in all.equals)	
    display.digits = -floor(log10(.Machine$double.eps ^ 0.5)),
    ...
) {
    #sensible names for generic parameters
    m = x
    
    cat("Aligned Rank Transform of Factorial Model\n\nCall:\n", paste(deparse(m$call), sep="\n", collapse="\n"), "\n\n", sep="")
    cat("Column sums of aligned responses (should all be ~0):\n")
    print(round(m$aligned.col.sums, display.digits), ...)
    cat("\nF values of ANOVAs on aligned responses not of interest (should all be ~0):\n")
    print(round(summary(m$aligned.anova$F), display.digits), ...)
}
