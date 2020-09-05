# artcon function for both "normal" contrasts and interaction contrasts
#
# Author: lelkin
###############################################################################

#' Aligned Ranked Transform Contrast Procedure.
#'
#' A nonparametric method to conduct contrasts on data aligned-and-ranked using
#' the ART-C procedure. This function is primarily used to conduct post hoc
#' tests following a significant omnibus effect found with
#' \code{\link{anova.art}}.
#'
#' An \code{\link{art}} model \code{m} stores the \code{formula} and \code{data}
#' that were passed to \code{\link{art}} when \code{m} was created. This
#' function extracts the original formula and data from \code{m}, and
#' aligns-and-ranks that data, using the ART-C procedure, and conducts contrasts
#' specified in parameter f.
#'
#' Internally, this function uses \code{\link{artlm.con}} to create a linear
#' model on data aligned-and-ranked with ART-C, computes estimated marginal
#' means on the linear model using \code{\link{emmeans}}, and conducts contrasts
#' using \code{\link{contrast}}.
#'
#' The work describing the transformation itself is currently under review.
#'
#' @param m An object of class \code{\link{art}}.
#' @param f Either an object of type \code{"character"} or type
#'   \code{"formula"}, specifying the fixed effects whose levels will be
#'   compared. See "Formula" section below.
#' @param response Which response to use: the aligned (with ART-C) response
#'   (\code{"aligned"}) or the aligned-and-ranked (with ART-C) response
#'   (\code{"art"}). Default is "art". This argument is passed to artlm.con.
#' @param factor.contrasts The name of the contrast-generating function to be
#'   applied by default to fixed effect factors. Sets the the first element of
#'   \code{\link{options}("contrasts")} for the duration of this function. The
#'   default is to use \code{"contr.sum"}, i.e. sum-to-zero contrasts, which is
#'   appropriate for Type III ANOVAs (the default ANOVA type for
#'   \code{\link{anova.art}}). This argument is passed to artlm.con.
#' @param method Contrast method argument passed to \code{\link{contrast}}.
#'   Note: default is \code{"pairwise"} but \code{\link{contrast}} default is
#'   \code{"eff"}.
#' @param interaction Logical value. Default is FALSE. If TRUE, conducts
#'   interactions contrasts directly on \code{\link{art}} model m (i.e., does
#'   not align and rank with ART-C procedure). See “Interaction contrasts”
#'   section in \code{\link{contrast}}.
#' @param adjust Character: adjustment method (e.g., "bonferroni") – passed to
#'   \code{\link{contrast}}. If not provided, \code{\link{contrast}} will use
#'   its default ("tukey" at the time of publication).
#' @param \dots Additional arguments passed to \code{\link{lm}} or
#'   \code{\link{lmer}}.
#' @return An object of class \code{emmGrid}. See \code{\link{contrast}}
#'   for details.
#' @author Lisa A. Elkin
#'
#' @section Formula: Contrasts compare combinations of levels from multiple
#'   factors. The \code{f} parameter indicates which factors are involved. Two
#'   formats are accepted: a \code{"character"} term as used in
#'   \code{\link{artlm}} and \code{\link{artlm.con}}, or a \code{"formula"} as
#'   used in \code{\link{emmeans}}. For example, contrasts comparing
#'   combinations of levels of factors \emph{X1} and \emph{X2} can be expressed
#'   as "X1:X2" (\code{"character"} term) or as ~ X1*X2 (\code{"formula"}).
#'
#' @references Lisa A. Elkin, Matthew Kay, James J. Higgins, Jacob O. Wobbrock.
#'   Under review.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(Higgins1990Table5, package="ARTool")
#'
#' ## Perform aligned rank transform
#' m <- art(DryMatter ~ Moisture*Fertilizer + (1|Tray), data=Higgins1990Table5)
#'
#' ## In a typical workflow, contrast tests using ART-C would follow a
#' ## significant omnibus effect found by running an anova on the ART responses
#' ## (equivalent to anova(m, response="art")).
#' ## If conducting planned contrasts, this step can be skipped.
#' anova(m)
#'
#' ## We can conduct contrasts comparing levels of Moisture using the ART-C procedure.
#' ## If conducting contrasts as a post hoc test, this would follow a significant effect
#' ## of Moisture on DryMatter.
#'
#' ## Using the "character" format for f
#' artcon(m, "Moisture")
#' ## Or using the "formula" format for f
#' artcon(m, ~ Moisture)
#'
#' ## Note: Since the ART-C procedure is mathematically equivalent to the ART procedure
#' ## in the single-factor case, this is the same as
#' ## emmeans(artlm(m, "Moisture"), pairwise ~ Moisture)
#'
#' ## We can conduct contrasts comparing combinations of levels
#' ## of Moisture and Fertilizer using the ART-C procedure.
#' ## If conducting contrasts as a post hoc test, this would follow
#' ## a significant Moisture:Fertlizer interaction effect on Drymatter.
#'
#' ## Using the "character" format for f
#' artcon(m, "Moisture:Fertilizer")
#' ## Using the "formula" format for f
#' artcon(m, ~ Moisture*Fertilizer)
#'
#' ## We can also conduct interaction contrasts (comparing differences of differences)
#' artcon(m, "Moisture:Fertilizer", interaction = TRUE)
#'
#' }


artcon = function(m, f, response = "art", factor.contrasts="contr.sum", method = "pairwise", 
                  interaction = FALSE, adjust, ...)
{
  f.parsed = parse.art.con.formula(f)
  # syntax handled differently for interaction contrasts.
  if(interaction){
    art.interaction.contrast = do.art.interaction.contrast(m, f.parsed, response, factor.contrasts, method, adjust, ...)
    art.interaction.contrast
  }
  else{
    artlm.con = artlm.con.internal(m, f.parsed, response, factor.contrasts, ...)
    art.contrast = do.art.contrast(f.parsed, artlm.con, method, adjust)
    art.contrast
  }
}