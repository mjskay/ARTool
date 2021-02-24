# art.con function for both "normal" contrasts and interaction contrasts
#
# Author: lelkin
###############################################################################

#' Aligned Ranked Transform Contrasts
#'
#' Conduct contrast tests following an Aligned Ranked Transform (ART) ANOVA
#' (\code{\link{anova.art}}). Conducts contrasts on \code{\link{art}} models
#' using aligned-and-ranked linear models using the ART (Wobbrock et al. 2011)
#' or ART-C (Elkin et al. 2021) alignment procedure, as appropriate to the requested contrast.
#'
#' An \code{\link{art}} model \code{m} stores the \code{formula} and \code{data}
#' that were passed to \code{\link{art}} when \code{m} was created. Depending on the
#' requested contrast type, this function either extracts the linear model from \code{m}
#' needed to perform that contrast or creates a new linear model on data
#' aligned-and-ranked using the ART-C
#' procedure, then conducts the contrasts specified in parameter \code{formula}.
#'
#' Internally, this function uses \code{\link{artlm.con}} (when \code{interaction = FALSE})
#' or \code{\link{artlm}} (when \code{interaction = TRUE}) to get the linear
#' model necessary for the requested contrast, computes estimated marginal
#' means on the linear model using \code{\link{emmeans}}, and conducts contrasts
#' using \code{\link{contrast}}.
#'
#' @param m An object of class \code{\link{art}}.
#' @param formula Either a character vector or a formula specifying the fixed
#'   effects whose levels will be compared. See "Formula" section below.
#' @param response Which response to use: the aligned response
#'   (\code{"aligned"}) or the aligned-and-ranked response
#'   (\code{"art"}). Default is "art". This argument is passed to \code{\link{artlm.con}}
#'   (when \code{interaction = FALSE}) or \code{\link{artlm}} (when \code{interaction = TRUE}).
#' @param factor.contrasts The name of the contrast-generating function to be
#'   applied by default to fixed effect factors. Sets the the first element of
#'   \code{\link{options}("contrasts")} for the duration of this function. The
#'   default is to use \code{"contr.sum"}, i.e. sum-to-zero contrasts, which is
#'   appropriate for Type III ANOVAs (the default ANOVA type for
#'   \code{\link{anova.art}}). This argument is passed to \code{\link{artlm.con}} /
#'   \code{\link{artlm}}.
#' @param method Contrast method argument passed to \code{\link{contrast}}.
#'   Note: the default is \code{"pairwise"} even though the default for the
#'   \code{\link{contrast}} function is \code{"eff"}.
#' @param interaction Logical value. If \code{FALSE} (the default), conducts contrasts using
#'   the ART-C procedure and \code{\link{artlm.con}}. If \code{TRUE}, conducts
#'   difference-of-difference contrasts using a model returned by \code{\link{artlm}}.
#'   See the "Interaction Contrasts" section in \code{\link{contrast}}.
#' @param adjust Character: adjustment method (e.g., "bonferroni") passed to
#'   \code{\link{contrast}}. If not provided, \code{\link{contrast}} will use
#'   its default ("tukey" at the time of publication). All available options are listed
#'   in \code{\link{summary.emmGrid}} in the "P-value adjustments" section.
#' @param \dots Additional arguments passed to \code{\link{lm}} or
#'   \code{\link{lmer}}.
#' @return An object of class \code{emmGrid}. See \code{\link{contrast}}
#'   for details.
#' @author Lisa A. Elkin, Matthew Kay, Jacob O. Wobbrock
#'
#' @section Formula: Contrasts compare combinations of levels from multiple
#'   factors. The \code{formula} parameter indicates which factors are involved. Two
#'   formats are accepted: (1) a character vector as used in
#'   \code{\link{artlm}} and \code{\link{artlm.con}}, with factors separated by \code{":"};
#'   or (2) a formula as used in \code{\link{emmeans}}, with factors separated by \code{*}.
#'   For example, contrasts comparing
#'   combinations of levels of factors \emph{X1} and \emph{X2} can be expressed
#'   as \code{"X1:X2"} (character vector) or as \code{~ X1*X2} (formula).
#'
#' @references Elkin, L. A., Kay, M, Higgins, J. J., and Wobbrock, J. O.
#'   (2021). An Aligned Rank Transform Procedure for Multifactor Contrast Tests.
#'   \href{arXiv eprint: 2102.11824}{https://arxiv.org/abs/2102.11824}.
#'
#'   Wobbrock, J. O., Findlater, L., Gergle, D., and Higgins, J. J.
#'   (2011). The Aligned Rank Transform for nonparametric factorial analyses
#'   using only ANOVA procedures. \emph{Proceedings of the ACM Conference on
#'   Human Factors in Computing Systems (CHI '11)}.  Vancouver, British Columbia
#'   (May 7-12, 2011). New York: ACM Press, pp. 143-146.
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(Higgins1990Table5, package = "ARTool")
#'
#' library(dplyr)
#'
#' ## Perform aligned rank transform
#' m <- art(DryMatter ~ Moisture*Fertilizer + (1|Tray), data=Higgins1990Table5)
#'
#' ## In a some workflows, contrast tests using ART-C would follow a
#' ## significant omnibus effect found by running an anova on the ART responses
#' ## (equivalent to anova(m, response="art")).
#' ## If conducting planned contrasts, this step can be skipped.
#' anova(m)
#'
#' ## We can conduct contrasts comparing levels of Moisture using the ART-C procedure.
#' ## If conducting contrasts as a post hoc test, this would follow a significant effect
#' ## of Moisture on DryMatter.
#'
#' ## Using a character vector
#' art.con(m, "Moisture")
#' ## Or using a formula
#' art.con(m, ~ Moisture)
#'
#' ## Note: Since the ART-C procedure is mathematically equivalent to the ART procedure
#' ## in the single-factor case, this is the same as
#' ## emmeans(artlm(m, "Moisture"), pairwise ~ Moisture)
#'
#' ## art.con() returns an emmGrid object, which does not print asterisks
#' ## beside "significant" tests (p < 0.05). If you wish to add stars beside
#' ## tests of a particular significant level, you can always do that to the
#' ## data frame returned by the summary() method of emmGrid. For example:
#' art.con(m, ~ Moisture) %>%
#'   summary() %>%
#'   mutate(sig = ifelse(p.value < 0.05, "*", ""))
#'
#' ## Or a more complex example:
#' art.con(m, ~ Moisture) %>%
#'   summary() %>%
#'   mutate(sig = symnum(p.value, corr = FALSE, na = FALSE,
#'     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
#'     symbols = c("***", "**", "*", ".", " ")
#'   ))
#'
#' ## We can conduct contrasts comparing combinations of levels
#' ## of Moisture and Fertilizer using the ART-C procedure.
#' ## If conducting contrasts as a post hoc test, this would follow
#' ## a significant Moisture:Fertlizer interaction effect on Drymatter.
#'
#' ## Using a character vector for formula
#' art.con(m, "Moisture:Fertilizer")
#' ## Using a formula
#' art.con(m, ~ Moisture*Fertilizer)
#'
#' ## We can also conduct interaction contrasts (comparing differences of differences)
#' art.con(m, "Moisture:Fertilizer", interaction = TRUE)
#'
#' ## For more examples, see vignette("art-contrasts")
#'
#' }
art.con = function(
  m, formula, response = "art", factor.contrasts="contr.sum", method = "pairwise",
  interaction = FALSE, adjust, ...
) {
  f.parsed = parse.art.con.formula(formula)

  # syntax handled differently for interaction contrasts.
  if (interaction) {
    art.interaction.contrast = do.art.interaction.contrast(m, f.parsed, response, factor.contrasts, method, adjust, ...)
    art.interaction.contrast
  }
  else {
    artlm.con = artlm.con.internal(m, f.parsed, response, factor.contrasts, ...)
    art.contrast = do.art.contrast(f.parsed, artlm.con, method, adjust)
    art.contrast
  }
}
