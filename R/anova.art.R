# ANOVAs for art objects
#
# Author: mjskay
###############################################################################


#' Aligned Rank Transform Analysis of Variance
#'
#' Conduct analyses of variance on aligned rank transformed data.
#'
#' This function runs several ANOVAs: one for each fixed effect term in the
#' model \code{object}. In each ANOVA, the independent variables are the same,
#' but the response is aligned by a different fixed effect term (if response is
#' "aligned") or aligned and ranked by that fixed effect term (if response is
#' "art"). These models are generated using \code{\link{artlm}}.
#'
#' From each model, only the relevant output rows are kept (unless
#' \code{all.rows} is \code{TRUE}, in which case all rows are kept).
#'
#' When \code{response} is \code{"art"} (the default), only one row is kept
#' from each ANOVA: the row corresponding to fixed effect term the response was
#' aligned and ranked by. These results represent nonparametric tests of
#' significance for the effect of each term on the original response variable.
#'
#' When \code{response} is \code{"aligned"}, all rows \emph{except} the row
#' corresponding to the fixed effect term the response was aligned by are kept.
#' If the ART procedure is appropriate for this data, these tests should have
#' all effects "stripped out", and have an F value of ~0. If that is not the
#' case, another analysis should be considered. This diagnostic is tested by
#' \code{\link{summary.art}} and a warning generated if the F values are not
#' all approximately 0.
#'
#' @name anova.art
#' @rdname anova.art
#' @aliases anova.art print.anova.art
#' @param object An object of class \code{\link{art}}.
#' @param response Which response to run the ANOVA on: the aligned responses
#' (\code{"aligned"}) or the aligned and ranked responses (\code{"art"}). This
#' argument is passed to \code{\link{artlm}}. See 'Details'.
#' @param type Type of ANOVAs to conduct. If \code{type} is \code{1} or
#' \code{"I"}, then conducts Type I ANOVAs using \code{\link{anova}}.
#' Otherwise, conducts Type II or Type III ANOVAs using \code{\link{Anova}}.
#' The default is Type III \emph{if} the underlying model supports it. Models
#' fit with \code{Error} terms are fit using \code{\link{aov}}, which only
#' supports Type I ANOVAs.
#' @param factor.contrasts The name of the contrast-generating function to be
#' applied by default to fixed effect factors. See the first element of
#' \code{\link{options}("contrasts")}. The default is to use
#' \code{"contr.sum"}, i.e. sum-to-zero contrasts, which is appropriate for
#' Type III ANOVAs (also the default). This argument is passed to
#' \code{\link{artlm}}.
#' @param test Test statistic to use. Default \code{"F"}. Note that some models
#' and ANOVA types may not support \code{"Chisq"}.
#' @param all.rows Show all rows of the resulting ANOVA tables? By default
#' (\code{FALSE}), shows only the rows that are relevant depending on the type
#' of \code{response}.
#' @param x An object of class \code{\link{art}}.
#' @param verbose When \code{TRUE}, sums of squares and residual sum of squares
#' in addition to degrees of freedom are printed in some ANOVA types (e.g.
#' repeated measures ANOVAs). Default \code{FALSE}, for brevity.
#' @param digits Digits of output in printed table; see \code{\link{print}}.
#' @param \dots Additional arguments passed to \code{\link{Anova}} or
#' \code{\link{anova}} by \code{anova.art} or to \code{\link{print}} by
#' \code{print.anova.art}.
#' @return An object of class \code{"anova"}, which usually is printed.
#' @author Matthew Kay
#' @seealso See \code{\link{art}} for an example. See also
#' \code{\link{summary.art}}, \code{\link{artlm}}.
#' @references Wobbrock, J. O., Findlater, L., Gergle, D., and Higgins, J. J.
#' (2011). The Aligned Rank Transform for nonparametric factorial analyses
#' using only ANOVA procedures. \emph{Proceedings of the ACM Conference on
#' Human Factors in Computing Systems (CHI '11)}.  Vancouver, British Columbia
#' (May 7-12, 2011). New York: ACM Press, pp. 143-146.
#' @keywords nonparametric
#'
#' @export
anova.art = function(object,
    response=c("art", "aligned"),
    type=c("III", "II", "I", 3, 2, 1),
    factor.contrasts="contr.sum",
    test=c("F", "Chisq"),
    all.rows=FALSE,
    ...
) {
    #sensible names for generic parameters
    m = object

    #match enum arguments
    response = match.arg(response)
    type = as.character(type)
    type = match.arg(type)
    test = match.arg(test)

    #get transformed responses based on response type requested
    responses = switch(response,
        aligned=m$aligned,
        art=m$aligned.ranks)

    #determine anova type to use
    type = if (type %in% c(1,"I")) "I"
    else if (type %in% c(2,"II")) "II"
    else if (type %in% c(3,"III")) "III"

    #are we going to need to show the term we aligned by
    #for each row of the output?
    show.aligned.by.term = response == "aligned" || all.rows

    #run linear models and anovas
    df = m$data
    anovas = NULL
    table.description = ""
    for (j in 1:ncol(responses)) {	#can't use ldply here because it appears to drop row names when binding rows
        aligned.by.term = colnames(responses)[[j]]

        #get linear model
        m.l = artlm(m, aligned.by.term, response=response, factor.contrasts=factor.contrasts)

        #run anova and extract desired results
        anova.j = flat.anova(m.l, type=type, test=test, ...)
        if (j == 1) table.description = attr(anova.j, "description")

        #extract desired result rows from anova
        #for art, this is the one row correponding to the effect we aligned and ranked by
        #for aligned, this is every effect *except* the one we aligned and ranked by
        if (!all.rows) {
            include.row = anova.j$Term == aligned.by.term
            if (response == "aligned") include.row = !include.row
            anova.j = anova.j[include.row,]
        }

        #Add "Aligned By" column when needed to disambiguate
        if (nrow(anova.j) > 0) {
            #if only one fixed effect we can get no rows here, e.g. if response="aligned"
            #and all.rows=FALSE, so the above guard is necessary
            if (show.aligned.by.term) {
                anova.j = cbind(anova.j[,1,drop=FALSE], `Aligned By`=aligned.by.term, anova.j[,-1,drop=FALSE])
            }
        }

        anovas = rbind(anovas, anova.j)
    }

    #fill in the rest of the anova table metadata and return
    class(anovas) = c("anova.art", "anova", "data.frame")
    attr(anovas, "model") =
        if (m$n.grouping.terms > 0) "lmer"
        else if (m$n.error.terms > 0) "aov"
        else "lm"
    attr(anovas, "table.description") = table.description
    attr(anovas, "response") = response
    attr(anovas, "response.term") = colnames(m$cell.means)[1]
    anovas
}

### Generate p stars for a vector of p values
#' @importFrom stats symnum
p.stars = function(p.values) {
    unclass(symnum(p.values, corr = FALSE, na = FALSE, cutpoints = c(0,
                            0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**",
                            "*", ".", " ")))
}

#' @rdname anova.art
#' @importFrom magrittr %<>%
#' @export
print.anova.art = function(x, verbose=FALSE, digits=5, ...) {
    #print heading and metadata
    cat("Analysis of Variance of Aligned Rank Transformed Data\n\n")
    cat("Table Type:", attr(x, "table.description"), "\n")
    cat("Model:", switch(attr(x, "model"),
        lm = "No Repeated Measures (lm)\n",
        aov = "Repeated Measures (aov)\n",
        lmer = "Mixed Effects (lmer)\n",
    ))
    cat(sep="", "Response: ", attr(x, "response"), "(", attr(x, "response.term"), ")\n\n")

    #format p values
    p.col = last(which(grepl("^(P|Pr)\\(", names(x))))
    stars.legend = if (!is.na(p.col)) {
        #add column for p stars
        stars = p.stars(x[[p.col]])
        x = cbind(x, ` ` = stars)
        #reformat p values for printing
        x[[p.col]] %<>% format.pval()
        #return stars legend
        attr(stars, "legend")
    } else NULL

    #generate row names from Terms
    rownames(x) = paste(1:nrow(x), x$Term)
    x$Term = NULL

    #abbreviate columns
    if (!is.null(x$Error)) x$Error %<>% abbreviate(5)
    if (!is.null(x$`Aligned By`)) x$`Aligned By` %<>% abbreviate(10)

    #drop "Sum Sq" (etc) columns when not doing verbose output
    if (!verbose) x %<>% select(everything(), -contains("Sum Sq"), -contains("Mean Sq"))

    #print table
    print.data.frame(x, digits=digits, ...)

    #print legend
    if (!is.null(stars.legend)) {
        cat("---\nSignif. codes:  ", stars.legend, "\n")
    }
}
