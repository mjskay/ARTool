\name{HigginsABC.art}
\alias{HigginsABC.art}
\docType{data}
\title{
Aligned Rank Transformed Version of HigginsABC
}
\description{
The ART version of \code{\link{HigginsABC}} as produced by the original ARTool, used to test
the correctness of \code{\link{art}} output.
}
\usage{data(HigginsABC.art)}
\format{
  A data frame with 16 observations on the following 19 variables.
  \describe{
    \item{\code{Subject}}{a numeric vector}
    \item{\code{A}}{a numeric vector}
    \item{\code{B}}{a numeric vector}
    \item{\code{C}}{a numeric vector}
    \item{\code{Y}}{a numeric vector}
    \item{\code{aligned.Y..for.A}}{a numeric vector}
    \item{\code{aligned.Y..for.B}}{a numeric vector}
    \item{\code{aligned.Y..for.A.B}}{a numeric vector}
    \item{\code{aligned.Y..for.C}}{a numeric vector}
    \item{\code{aligned.Y..for.A.C}}{a numeric vector}
    \item{\code{aligned.Y..for.B.C}}{a numeric vector}
    \item{\code{aligned.Y..for.A.B.C}}{a numeric vector}
    \item{\code{ART.Y..for.A}}{a numeric vector}
    \item{\code{ART.Y..for.B}}{a numeric vector}
    \item{\code{ART.Y..for.A.B}}{a numeric vector}
    \item{\code{ART.Y..for.C}}{a numeric vector}
    \item{\code{ART.Y..for.A.C}}{a numeric vector}
    \item{\code{ART.Y..for.B.C}}{a numeric vector}
    \item{\code{ART.Y..for.A.B.C}}{a numeric vector}
  }
}
\source{
Wobbrock, J. O., Findlater, L., Gergle, D., and Higgins, J. J. \emph{ARTool}. \url{http://depts.washington.edu/aimgroup/proj/art/}.
}
\seealso{
\code{\link{HigginsABC}}, \code{\link{art}}.
}
\keyword{datasets}
\keyword{internal}
