\name{Higgins1990Table1}
\alias{Higgins1990Table1}
\docType{data}
\title{
Synthetic 3x3 Factorial Randomized Experiment
}
\description{
Synthetic data from a balanced 3x3 factorial experiment with main effects, no interaction, 
and independent and identically distributed (i.i.d.) Normal errors.
}
\usage{data(Higgins1990Table1)}
\format{
  A data frame with 36 observations on the following 4 variables.
  \describe{
    \item{\code{Subject}}{a numeric vector}
    \item{\code{Row}}{a factor with levels \code{1} \code{2} \code{3}}
    \item{\code{Column}}{a factor with levels \code{1} \code{2} \code{3}}
    \item{\code{Response}}{a numeric vector}
  }
}
\source{
Higgins, J. J., Blair, R. C. and Tashtoush, S. (1990). The aligned rank transform procedure. 
\emph{Proceedings of the Conference on Applied Statistics in Agriculture}. Manhattan, Kansas: Kansas State University, pp. 185-195.
}
\seealso{
\code{\link{art}}, \code{\link{anova.art}}. 
}
\examples{
data(Higgins1990Table1)

## run aligned-rank transform and ANOVA on the data
## Note: because there is only one observation per Subject 
## in this dataset, we do not need to include Subject as 
## a grouping term in this formula. Indeed, if we did, 
## lmer would complain when we attempt the ANOVA.
m <- art(Response ~ Row*Column, data=Higgins1990Table1)
anova(m)
}
\keyword{datasets}
