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
#' This function is used internally by \code{\link{art.con}} to construct
#' linear models for contrasts using the ART-C procedure (Elkin et al. 2021).
#' It is typically not necessary to use this function directly to conduct contrasts using
#' the ART-C procedure, you can use \code{\link{art.con}} instead, which will
#' ensure that the correct model and contrast test is run. However, should you
#' wish to use the ART-C procedure with a different contrast test
#' than provided by \code{\link{art.con}}, you may with to use this function.
#'
#' @param m An object of class \code{\link{art}}.
#' @param term A character vector indicating the effect term in
#'   the transformed data in \code{m} to use as the aligned or art response.
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
#'   (i.e. a model fit by \code{\link{lmer}}) if it does contain grouping terms, or
#'   an object of class \code{aovlist} (i.e. a model fit by \code{\link{aov}})
#'   if it contains error terms.
#' @details Internally, the ART-C procedure concatenates the variables specified
#'   in \code{term}, and then removes the originals. When specifying the effect
#'   terms on which to conduct contrasts, use the concatenation of the effects
#'   specified in \code{term} instead of the original variables. This is demonstrated
#'   in the example below.
#' @seealso See also \code{\link{art.con}}, which makes use of this function.

#'
#' @author Lisa A. Elkin
#' @references Lisa A. Elkin, Matthew Kay, James J. Higgins, Jacob O. Wobbrock.
#' Under review (CHI 2021).
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ## create an art model
#' m <- art(DryMatter ~ Moisture*Fertilizer + (1|Tray), data=Higgins1990Table5)
#'
#' ## use emmeans to conduct pairwise contrasts on "Moisture"
#' contrast(emmeans(artlm.con(m, "Moisture"), pairwise ~ Moisture))
#'
#' ## use emmeans to conduct pairwise contrasts on "Moisture:Fertilizer"
#' ## N.B. internally, artlm.con concatenates the factors Moisture and Fertilizer
#' ## to create MoistureFertilizer. If you try to use any of Moisture, Fertilizer,
#' ## Moisture:Fertilizer, or Moisture*Fertilizer in the RHS of the formula
#' ## passed to emmeans, you will get an error because the factors Moisture and Fertilizer
#' ## do not exist in the model returned by artlm.con.
#' contrast(emmeans(artlm.con(m, "Moisture:Fertilizer"), pairwise ~ MoistureFertilizer))
#'
#' ## Note: art.con uses emmeans internally, and the above examples are equivalent to
#' ## the following calls to art.con, which is the recommended approach as it will
#' ## ensure the model selected and the contrasts extracted from emmeans match.
#' art.con(m, "Moisture")
#' art.con(m, "Moisture:Fertilizer")
#'
#' }
#'
artlm.con = function(m, term, response = "art", factor.contrasts = "contr.sum", ...) {
    f.parsed = parse.art.con.string.formula(term)

    artlm.con = artlm.con.internal(m, f.parsed, response, factor.contrasts, ...)
    artlm.con
}

