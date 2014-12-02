\name{HigginsABC}
\alias{HigginsABC}
\docType{data}
\title{
Synthetic 2x2x2 Mixed Design Experiment
}
\description{
Synthetic data from an experiment with two between-\code{Subject}s factors (\code{A} and \code{B}) 
having two levels each and one within-\code{Subject}s factor (\code{C}) with two levels.
}
\usage{data(HigginsABC)}
\format{
  A data frame with 16 observations on the following 5 variables.
  \describe{
    \item{\code{Subject}}{a numeric vector}
    \item{\code{A}}{a factor with levels \code{1} \code{2}}
    \item{\code{B}}{a factor with levels \code{1} \code{2}}
    \item{\code{C}}{a factor with levels \code{1} \code{2}}
    \item{\code{Y}}{a numeric vector}
  }
}
\source{
Wobbrock, J. O., Findlater, L., Gergle, D., and Higgins, J. J. \emph{ARTool}. \url{http://depts.washington.edu/aimgroup/proj/art/}.
}
\seealso{
\code{\link{art}}, \code{\link{anova.art}}. 
}
\examples{\dontrun{
data(HigginsABC, HigginsABC.art, package="ARTool")

## run aligned-rank transform and ANOVA on the data
m <- art(Y ~ A*B*C + (1|Subject), data=HigginsABC)
anova(m)
}}
\keyword{datasets}
