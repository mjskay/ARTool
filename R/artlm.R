# art function and some basic generic function implementations for art objects
#
# Author: mjskay
###############################################################################


#' Per-Term Linear Model from Aligned Rank Transformed Data
#'
#' Build a linear model for ART data with response aligned or aligned and
#' ranked by the specified term from the model.
#'
#' This function is used primarily for post-hoc tests. To run an ANOVA, it does
#' not need to be called directly; instead, use \code{\link{anova.art}}, which
#' calls this function as needed.
#'
#' @param m An object of class \code{\link{art}}.
#' @param term An object of type \code{"character"} indicating the effect term
#' in the transformed data in \code{m} to use as the aligned or art response.
#' @param response Which response to use: the aligned response
#' (\code{"aligned"}) or the aligned and ranked (\code{"art"}) response.
#' @param factor.contrasts The name of the contrast-generating function to be
#' applied by default to fixed effect factors. Sets the the first element of
#' \code{\link{options}("contrasts")} for the duration of this function. The
#' default is to use \code{"contr.sum"}, i.e. sum-to-zero contrasts, which is
#' appropriate for Type III ANOVAs (the default ANOVA type for
#' \code{\link{anova.art}}).
#' @param \dots Additional arguments passed to \code{\link{lm}} or
#' \code{\link{lmer}}.
#' @return An object of class \code{\link{lm}} if \code{formula(m)} does not
#' contain grouping or error terms, an object of class \code{\link{merMod}}
#' (i.e. a model fit by \code{\link{lmer}}) if it contains grouping terms, or
#' an object of class \code{aovlist} (i.e. a model fit by \code{\link{aov}}) if
#' it contains error terms.
#' @author Matthew Kay
#' @seealso See \code{\link{art}} for an example. See also
#' \code{\link{anova.art}}, which makes use of this function.
#' @keywords nonparametric
#'
#' @importFrom stats lm update aov
#' @importFrom lme4 lmer
#' @export
artlm = function(m, term,
    response=c("art", "aligned"),
    factor.contrasts="contr.sum",
	...
) {
	#match enum arguments
    response = match.arg(response)

    #for the duration of this function, switch to the supplied contrast types
    original.contrasts = getOption("contrasts")
    tryCatch({
        options(contrasts=c(factor.contrasts, original.contrasts[-1]))

        #place the transformed (aligned or aligned and ranked) version of y
        #into the data frame as the dummy response ".y"
        df = m$data
        df$.y = switch(response,
            aligned=m$aligned[[term]],
            art=m$aligned.ranks[[term]])

        #modify formula to use dummy response name ".y"
        f = update(m$formula, .y ~ .)

        #reassign the environment of the model formula to this frame so that the data can be correctly recreated
        #by other functions (e.g. emmeans:::recover.data) that use the function call and environment
        environment(f) = sys.frame(sys.nframe())

        #run linear model
        if (m$n.grouping.terms > 0) {       #grouping terms => REML
            m = lmer(f, data=df, ...)
        } else if (m$n.error.terms > 0) {   #error terms => repeated measures ANOVA
            m = aov(f, data=df, ...)
        } else {	                        #no grouping or error terms => OLS
            m = lm(f, data=df, ...)
        }

        attr(m, "term") = term
        attr(m, "response") = response
        m
    }, finally = {
        options(contrasts=original.contrasts)
    })
}
