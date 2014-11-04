\name{anova.art}
\alias{anova.art}
\title{
Aligned Rank Transform Analysis of Variance
}
\description{
Conduct analyses of variance on aligned rank transformed data.
}
\usage{
\method{anova}{art}(object, 
    response = c("art", "aligned"), 
    type=c("III", "II", "I", 3, 2, 1), 
    factor.contrasts="contr.sum",
    test = c("F", "Chisq"),
    all.rows = FALSE, 
    ...)
}
\arguments{
  \item{object}{
An object of class \code{\link{art}}.
}
  \item{response}{
Which response to run the ANOVA on: the aligned responses (\code{"aligned"}) or the 
aligned and ranked responses (\code{"art"}). This argument is passed to \code{\link{artlm}}. See 'Details'.
}
  \item{type}{
Type of ANOVAs to conduct. If \code{type} is \code{1} or \code{"I"}, then conducts Type I ANOVAs 
using \code{\link{anova}}. Otherwise, conducts Type II or Type III ANOVAs using \code{\link{Anova}}.
}
  \item{factor.contrasts}{
The name of the contrast-generating function to be applied by default to fixed effect factors. See
the first element of \code{\link{options}("contrasts")}. The default is to use \code{"contr.sum"}, 
i.e. sum-to-zero contrasts, which is appropriate for Type III ANOVAs (also the default). This
argument is passed to \code{\link{artlm}}.
}
  \item{test}{
Test statistic to use. Default \code{"F"}. Note that some models and ANOVA types may not support \code{"Chisq"}. 
}
  \item{all.rows}{
Show all rows of the resulting ANOVA tables? By default (\code{FALSE}), shows only the
rows that are relevant depending on the type of \code{response}.
}
  \item{\dots}{
Additional arguments passed to \code{\link{Anova}} or \code{\link{anova}}.
}
}
\details{
This function runs several ANOVAs: one for each fixed effect term in the model \code{object}.
In each ANOVA, the independent variables are the same, but the response is aligned by a different 
fixed effect term (if response is "aligned") or aligned and ranked by that fixed effect term 
(if response is "art"). These models are generated using \code{\link{artlm}}. 

From each model, only the relevant output rows are kept (unless \code{all.rows} is \code{TRUE}, 
in which case all rows are kept). 

When \code{response} is \code{"art"} (the default), only one
row is kept from each ANOVA: the row corresponding to fixed effect term the response was aligned
and ranked by. These results represent nonparametric tests of significance for the effect of
each term on the original response variable.

When \code{response} is \code{"aligned"}, all rows \emph{except} the row corresponding to the fixed effect 
term the response was aligned by are kept. If the ART procedure is appropriate for this
data, these tests should have all effects "stripped out", and have an F value of ~0. If that
is not the case, another analysis should be considered. This diagnostic is tested by \code{\link{summary.art}}
and a warning generated if the F values are not all approximately 0.
}
\value{
An object of class \code{"anova"}, which usually is printed.  
}
\references{
Wobbrock, J. O., Findlater, L., Gergle, D., and Higgins, J. J. (2011). The Aligned Rank Transform for nonparametric factorial 
analyses using only ANOVA procedures. \emph{Proceedings of the ACM Conference on Human Factors in Computing Systems (CHI '11)}. 
Vancouver, British Columbia (May 7-12, 2011). New York: ACM Press, pp. 143-146. 
}
\author{
Matthew Kay
}
\seealso{
See \code{\link{art}} for an example. See also \code{\link{summary.art}}, \code{\link{artlm}}.
}
\keyword{nonparametric}
