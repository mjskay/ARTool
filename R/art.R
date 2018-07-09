# art function and some basic generic function implementations for art objects,
# such as print.art and summary.art
#
# Author: mjskay
###############################################################################


#' Aligned Rank Transform
#'
#' Apply the aligned rank transform to a factorial model (with optional
#' grouping terms). Usually done in preparation for a nonparametric analyses of
#' variance on models with numeric or ordinal responses, which can be done by
#' following up with \code{anova.art}.
#'
#' The aligned rank transform allows a nonparametric analysis of variance to be
#' conducted on factorial models with fixed and random effects (or repeated
#' measures) and numeric or ordinal responses. This is done by first aligning
#' and ranking the fixed effects using this function, then conducting an
#' analysis of variance on linear models built from the transformed data using
#' \code{\link{anova.art}} (see 'Examples'). The model specified using this
#' function \emph{must} include all interactions of fixed effects.
#'
#' The \code{formula} should contain a single response variable (left-hand
#' side) that can be numeric, an ordered factor, or logical. The right-hand
#' side of the formula should contain one or more fixed effect factors, zero or
#' more grouping terms, and zero or more error terms. Error terms and grouping
#' terms cannot be used simultaneously. All possible interactions of the fixed
#' effect terms must be included. For example, \code{y ~ x} and \code{y ~
#' a*b*c} and \code{y ~ a + b + b:c} are legal, but \code{y ~ a + b} is not, as
#' it omits the interaction \code{a:b}. Grouping terms are specified as in
#' \code{\link{lmer}}, e.g. \code{y ~ a*b*c + (1|d)} includes the random
#' intercept term \code{(1|d)}. Error terms are specified as in
#' \code{\link{aov}}, e.g. \code{y ~ a*b*c + Error(d)}. Grouping terms and
#' error terms are not involved in the transformation, but are included in the
#' model when ANOVAs are conducted, see \code{\link{anova.art}}.
#'
#' For details on the transformation itself, see Wobbrock \emph{et al.} (2011)
#' or the ARTool website: \url{http://depts.washington.edu/aimgroup/proj/art/}.
#'
#' @param formula A factorial formula with optional grouping terms or error
#' terms (but not both). Should be a formula with a single response variable
#' (left-hand side) and one or more terms with all interactions on the
#' right-hand side, e.g. \code{y ~ x} or \code{y ~ a*b*c} or \code{y ~ a + b +
#' b:c}. If you want to run a mixed effects ANOVA on the transformed data using
#' \code{\link{lmer}}, you can include grouping terms, as in \code{y ~ a*b*c +
#' (1|d)}.  If you want to run a repeated measures ANOVA using
#' \code{\link{aov}}, you can include error terms, as in \code{y ~ a*b*c +
#' Error(d)}. See 'Details'.
#' @param data An optional data frame containing the variables in the model.
#' @param rank.comparison.digits The number of digits to round aligned
#' responses to before ranking (to ensure ties are computed consistently).  See
#' the \code{digits} argument of \code{\link{round}}. The default value is
#' based on the default \code{tolerance} used for fuzzy comparison in
#' \code{all.equal}.
#' @param check.errors.are.factors Should we check to ensure \code{Error()}
#' terms are all factors? A common mistake involves coding a categorical variable
#' as numeric and passing it to \code{Error()}, yielding incorrect results
#' from \code{\link{aov}}. Disabling this check is not recommended unless you
#' know what you are doing; the most common uses of \code{Error()} (e.g.
#' in repeated measures designs) involve categorical variables (factors).
#' @return An object of class \code{"art"}:
#'
#' \item{call}{ The call used to generate the transformed data. }
#' \item{formula}{ The formula used to generate the transformed data. }
#' \item{cell.means}{ A data frame of cell means for each fixed term and
#' interaction on the right-hand side of formula. } \item{estimated.effects}{ A
#' data frame of estimated effects for each fixed term and interaction on the
#' right-hand side of formula. } \item{residuals}{ A vector of residuals
#' (response - cell mean of highest-order interaction). } \item{aligned}{ A
#' data frame of aligned responses for each fixed term and interaction on the
#' right-hand side of formula. } \item{aligned.ranks}{ A data frame of aligned
#' and ranked responses for each fixed term and interaction on the right-hand
#' side of formula. } \item{data}{ The input data frame }
#' \item{n.grouping.terms}{ The number of grouping variables in the input
#' formula. }
#'
#' For a complete description of cell means, estimated effects, aligned ranks,
#' etc., in the above output, see Wobbrock \emph{et al.} (2011).
#' @author Matthew Kay
#' @seealso \code{\link{summary.art}}, \code{\link{anova.art}},
#' \code{\link{artlm}}.
#' @references Wobbrock, J. O., Findlater, L., Gergle, D., and Higgins, J. J.
#' \emph{ARTool}. \url{http://depts.washington.edu/aimgroup/proj/art/}.
#'
#' Wobbrock, J. O., Findlater, L., Gergle, D., and Higgins, J. J. (2011). The
#' Aligned Rank Transform for nonparametric factorial analyses using only ANOVA
#' procedures. \emph{Proceedings of the ACM Conference on Human Factors in
#' Computing Systems (CHI '11)}.  Vancouver, British Columbia (May 7-12, 2011).
#' New York: ACM Press, pp. 143-146.
#' @keywords nonparametric
#' @examples
#' \dontrun{
#' data(Higgins1990Table5, package="ARTool")
#'
#' ## perform aligned rank transform
#' m <- art(DryMatter ~ Moisture*Fertilizer + (1|Tray), data=Higgins1990Table5)
#'
#' ## see summary data to ensure aligned rank transform is appropriate for this data
#' summary(m)
#' ## looks good (aligned effects sum to 0 and F values on aligned responses
#' ## not of interest are all ~0)
#'
#' ## we can always look at the anova of aligned data if we want more detail
#' ## to assess the appropriateness of ART.  F values in this anova should all
#' ## be approx 0.
#' anova(m, response="aligned")
#'
#' ## then we can run an anova on the ART responses (equivalent to anova(m, response="art"))
#' anova(m)
#'
#' ## if we want post-hoc tests, artlm(m, term) returns the linear model for the
#' ## given term
#' ## which we can then examine using our preferred method (emmeans, glht, etc)
#' ## e.g., pairwise contrasts on Moisture:
#' library(emmeans)
#' emmeans(artlm(m, "Moisture"), pairwise ~ Moisture)
#'
#' ## pairwise contrasts on Fertilizer:
#' emmeans(artlm(m, "Fertilizer"), pairwise ~ Fertilizer)
#'
#' ## N.B. The above types of contrasts ARE NOT valid for interactions.
#' ## Instead, use testInteractions from the phia package. For example:
#' library(phia)
#' testInteractions(artlm(m, "Moisture:Fertilizer"), pairwise=c("Moisture", "Fertilizer"))
#' ## For a more in-depth explanation and example, see this vignette:
#' vignette("art-contrasts")
#'
#' }
#'
#' @importFrom stats complete.cases model.frame terms
#' @importFrom plyr laply llply
#' @export
art = function(formula, data,
    #number of digits to round aligned responses to before ranking (to ensure ties are computed consistently)
    rank.comparison.digits = -floor(log10(.Machine$double.eps ^ 0.5)),
    check.errors.are.factors = TRUE
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
    non.factor.terms = Filter(function (col) !is.factor(df[,col]) && !is.logical(df[,col]), 2:ncol(df))
    if (any(non.factor.terms)) {
        stop(paste0(
            "All fixed effect terms must be factors or logical (e.g. not numeric).\n",
            "  The following terms are not factors or logical:\n    ",
            paste0(names(df)[non.factor.terms], collapse = "\n    "),
            "\n  If these terms are intended to represent categorical data, you may\n  ",
            "want to convert them into factors using factor()."
            ))

    }
    #coerce fixed effects to numeric for processing
    for (j in 2:ncol(df)) {
        df[,j] = as.numeric(df[,j])
    }

    #for error terms, issue error if any terms aren't factors
    if (check.errors.are.factors && f$n.error.terms > 0) {
        error.term.df = model.frame(f$error.terms, data)
        non.factor.error.terms = Filter(function (col) !is.factor(error.term.df[,col]), 1:ncol(error.term.df))
        if (any(non.factor.error.terms)) {
            stop(paste0(
                "The following Error terms are not factors:\n    ",
                paste0(names(error.term.df)[non.factor.error.terms], collapse = "\n    "),
                "\n  If these terms are intended to represent categorical data, such as subjects in a \n",
                "  repeated measures design, you should convert them into factors using factor().\n",
                "  \n",
                "  If you know what you are doing and still want Error terms that are not factors, use\n",
                "  check.errors.are.factors = FALSE."
                ))
        }
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
    m$n.grouping.terms = f$n.grouping.terms
    m$n.error.terms = f$n.error.terms
    m
}


#' Aligned Rank Transform Summary
#'
#' Summary and diagnostics for aligned rank transformed data
#'
#' This function gives diagnostic output to help evaluate whether the ART
#' procedure is appropriate for an analysis. It tests that column sums of
#' aligned responses are ~0 and that F values of ANOVAs on aligned responses
#' not of interest are ~0. For more details on these diagnostics see Wobbrock
#' \emph{et al.} (2011).
#'
#' @param object An object of class \code{\link{art}}.
#' @param \dots Potentially further arguments passed from other methods.
#' @return An object of class \code{"summary.art"}, which usually is printed.
#' @author Matthew Kay
#' @seealso See \code{\link{art}} for an example. See also
#' \code{\link{anova.art}}.
#' @references Wobbrock, J. O., Findlater, L., Gergle, D., and Higgins, J. J.
#' (2011). The Aligned Rank Transform for nonparametric factorial analyses
#' using only ANOVA procedures. \emph{Proceedings of the ACM Conference on
#' Human Factors in Computing Systems (CHI '11)}.  Vancouver, British Columbia
#' (May 7-12, 2011). New York: ACM Press, pp. 143-146.
#' @keywords nonparametric
#'
#' @importFrom stats anova
#' @export
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

#' @export
print.art = function(x, ...) print(summary(x), ...)

#' @export
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
