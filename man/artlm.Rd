\name{artlm}
\alias{artlm}
\title{
Per-Term Linear Model from Aligned Rank Transformed Data
}
\description{
Build a linear model for ART data with response aligned or aligned and ranked 
by the specified term from the model. 
}
\usage{
artlm(m, 
    term, 
    response = c("art", "aligned"),
    factor.contrasts="contr.sum",
    ...)
}
\arguments{
  \item{m}{
An object of class \code{\link{art}}.
}
  \item{term}{
An object of type \code{"character"} indicating the effect term in the
transformed data in \code{m} to use as the aligned or art response.
}
  \item{response}{
Which response to use: the aligned response (\code{"aligned"}) or the 
aligned and ranked (\code{"art"}) response.
}
  \item{factor.contrasts}{
The name of the contrast-generating function to be applied by default to fixed effect factors. Sets the
the first element of \code{\link{options}("contrasts")} for the duration of this function. The default 
is to use \code{"contr.sum"}, i.e. sum-to-zero contrasts, which is appropriate for Type III ANOVAs 
(the default in \code{\link{anova.art}}).
}
  \item{\dots}{
Additional arguments passed to \code{\link{lm}} or \code{\link{lmer}}.
}
}
\details{
This function is used primarily for post-hoc tests. To run an ANOVA, it
does not need to be called directly; instead, use \code{\link{anova.art}},
which calls this function as needed.
}
\value{
An object of class \code{\link{lm}} if \code{formula(m)} does not contain
grouping terms, and an object of class \code{\link{merMod}} (i.e. a model
fit by \code{\link{lmer}}) if it does.
}
\author{
Matthew Kay
}
\seealso{
See \code{\link{art}} for an example. See also \code{\link{anova.art}}, which makes use of this function.
}
\keyword{nonparametric}
