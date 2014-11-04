\name{Higgins1990Table5}
\alias{Higgins1990Table5}
\docType{data}
\title{
Split-plot Experiment Examining Effect of Moisture and Fertilizer on Dry Matter in Peat Pots
}
\description{
This dataset comes from a split-plot experiment examining \code{Tray}s of 4 peat pots each. \code{Moisture} was
varied between \code{Tray}s (i.e. it was the whole-plot treatment) and \code{Fertilizer} was varied within \code{Tray}s
(i.e. it was the sub-plot treatment). The outcome measure was \code{DryMatter}.

This dataset, originally from Milliken & Johnson (1984), is reproduced here from Higgins \emph{et al.} (1990).
}
\usage{data(Higgins1990Table5)}
\format{
  A data frame with 48 observations on the following 4 variables.
  \describe{
    \item{\code{Tray}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7} \code{8} \code{9} \code{10} \code{11} \code{12}}
    \item{\code{Moisture}}{a factor with levels \code{1} \code{2} \code{3} \code{4}}
    \item{\code{Fertilizer}}{a factor with levels \code{1} \code{2} \code{3} \code{4}}
    \item{\code{DryMatter}}{a numeric vector}
  }
}
\source{
Milliken, G.A., Johnson, D.E. (1984). \emph{Analysis of Messy Data Vol I: Designed Experiments}.
Van Nostrand Reinhold Company, New York.
}
\references{
Higgins, J. J., Blair, R. C. and Tashtoush, S. (1990). The aligned rank transform procedure. 
\emph{Proceedings of the Conference on Applied Statistics in Agriculture}. Manhattan, Kansas: Kansas State University, pp. 185-195.
}
\seealso{
See \code{\link{art}} for a more complete example. See also \code{\link{anova.art}}. 
}
\examples{
data(Higgins1990Table5)
		
## run aligned-rank transform and ANOVA on the data
m <- art(DryMatter ~ Moisture*Fertilizer + (1|Tray), data=Higgins1990Table5)
anova(m)
}
\keyword{datasets}
