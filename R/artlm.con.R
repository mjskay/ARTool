# artlm function to align-and-rank with ART-C and then create linear model
#
# Author: lelkin
###############################################################################

#' Per-Term Linear Model on Data Aligned-and-Ranked with ART-C
#'
#' Given an \code{\link{art}} model, build a linear model from data aligned or
#' aligned-and-ranked with ART-C alignment procedure by the specified term in
#' the model.
#'
#' This function is used primarily to conduct contrasts on data
#' aligned-and-ranked using the ART-C procedure using a different contrast test
#' than provided by \code{\link{artcon}}, which uses (\code{\link{contrast}}).
#' It is not necessary to use this function directly to conduct contrasts using
#' the ART-C procedure.
#'
#' @param m An object of class \code{\link{art}}.
#' @param f An object of type \code{"character"} indicating the effect term in
#'   the transformed data in \code{m} to use as the aligned or art response.
#'   this the same as the \code{"term"} in \code{\link{artlm}}
#' @param response Which response to use: the aligned (with ART-C) response
#'   (\code{"aligned"}) or the aligned and ranked (with ART-C) response
#'   (\code{"art"}).
#' @param factor.contrasts The name of the contrast-generating function to be
#'   applied by default to fixed effect factors. Sets the the first element of
#'   \code{\link{options}("contrasts")} for the duration of this function. The
#'   default is to use \code{"contr.sum"}, i.e. sum-to-zero contrasts, which is
#'   appropriate for Type III ANOVAs (the default ANOVA type for
#'   \code{\link{anova.art}}).
#' @param \dots Additional arguments passed to \code{\link{lm}} or
#'   \code{\link{lmer}}.
#' @return An object of class \code{\link{lm}} if \code{formula(m)} does not
#'   contain grouping or error terms, an object of class \code{\link{merMod}}
#'   (i.e. a model fit by \code{\link{lmer}}) if it contains grouping terms, or
#'   an object of class \code{aovlist} (i.e. a model fit by \code{\link{aov}})
#'   if it contains error terms.
#' @details Internally, the ART-C procedure concatenates the variables specified
#'   in \code{f}, and then removes the originals. When specifying the effect
#'   terms on which to conduct contrasts, use the concatenation of the effects
#'   specified in \code{f} instead of the original variables. This is demonstrated
#'   in the examples below.
#' @seealso See also \code{\link{artcon}}, which makes use of this function.

#' 
#' @author Lisa A. Elkin
#' @references Lisa A. Elkin, Matthew Kay, James J. Higgins, Jacob O. Wobbrock.
#' Under review (CHI 2021).
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' }
#' 
artlm.con = function(m, f, response="art", factor.contrasts="contr.sum", ...)
{
  f.parsed = parse.art.con.string.formula(f)
  
  artlm.con = artlm.con.internal(m, f.parsed, response, factor.contrasts,...)
  artlm.con
}

# syntax: contrast(emmeans(artlm.con(m, "X1:X2"), ~ X1X2), method="pairwise")