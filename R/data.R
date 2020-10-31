# documentation of datasets
#
# Author: mjskay
###############################################################################


#' Aligned Rank Transformed Version of Higgins1990Table1
#'
#' The ART version of \code{\link{Higgins1990Table1}} as produced by the
#' original ARTool, used to test the correctness of \code{\link{art}} output.
#'
#'
#' @name Higgins1990Table1.art
#' @docType data
#' @format A data frame with 36 observations on the following 10 variables.
#' \describe{
#'      \item{Subject}{a factor with levels \code{"s1"} .. \code{"s36"}}
#'      \item{Row}{a factor with levels \code{"r1"} .. \code{"r3"}}
#'      \item{Column}{a factor with levels \code{"c1"} .. \code{"c3"}}
#'      \item{Response}{a numeric vector}
#'      \item{aligned.Response..for.Row}{a numeric vector}
#'      \item{aligned.Response..for.Column}{a numeric vector}
#'      \item{aligned.Response..for.Row.Column}{a numeric vector}
#'      \item{ART.Response..for.Row}{a numeric vector}
#'      \item{ART.Response..for.Column}{a numeric vector}
#'      \item{ART.Response..for.Row.Column}{a numeric vector}
#' }
#' @seealso \code{\link{Higgins1990Table1}}, \code{\link{art}}.
#' @source Wobbrock, J. O., Findlater, L., Gergle, D., and Higgins, J. J.
#' \emph{ARTool}. \url{https://depts.washington.edu/acelab/proj/art/}.
#' @keywords datasets internal
NULL


#' Synthetic 3x3 Factorial Randomized Experiment
#'
#' Synthetic data from a balanced 3x3 factorial experiment with main effects,
#' no interaction, and independent and identically distributed (i.i.d.) Normal
#' errors.
#'
#'
#' @name Higgins1990Table1
#' @docType data
#' @format A data frame with 36 observations on the following 4 variables.
#' \describe{
#'      \item{Subject}{a factor with levels \code{"s1"} .. \code{"s36"}}
#'      \item{Row}{a factor with levels \code{"r1"} .. \code{"r3"}}
#'      \item{Column}{a factor with levels \code{"c1"} .. \code{"c3"}}
#'      \item{Response}{a numeric vector}
#' }
#' @seealso \code{\link{art}}, \code{\link{anova.art}}.
#' @source Higgins, J. J., Blair, R. C. and Tashtoush, S. (1990). The aligned
#' rank transform procedure.  \emph{Proceedings of the Conference on Applied
#' Statistics in Agriculture}. Manhattan, Kansas: Kansas State University, pp.
#' 185-195.
#' @keywords datasets
#' @examples
#'
#' data(Higgins1990Table1)
#'
#' ## run aligned-rank transform and ANOVA on the data
#' ## Note: because there is only one observation per Subject
#' ## in this dataset, we do not need to include Subject as
#' ## a grouping term in this formula. Indeed, if we did,
#' ## lmer would complain when we attempt the ANOVA.
#' m <- art(Response ~ Row*Column, data=Higgins1990Table1)
#' anova(m)
#'
NULL


#' Aligned Rank Transformed Version of Higgins1990Table5
#'
#' The ART version of \code{\link{Higgins1990Table5}} as produced by the
#' original ARTool, used to test the correctness of \code{\link{art}} output.
#'
#'
#' @name Higgins1990Table5.art
#' @docType data
#' @format A data frame with 48 observations on the following 10 variables.
#' \describe{
#'      \item{Tray}{a factor with levels \code{"t1"} .. \code{"t12"}}
#'      \item{Moisture}{a factor with levels \code{"m1"} .. \code{"m4"}}
#'      \item{Fertilizer}{a factor with levels \code{"f1"} .. \code{"f4"}}
#'      \item{DryMatter}{a numeric vector}
#'      \item{aligned.DryMatter..for.Moisture}{a numeric vector}
#'      \item{aligned.DryMatter..for.Fertilizer}{a numeric vector}
#'      \item{aligned.DryMatter..for.Moisture.Fertilizer}{a numeric vector}
#'      \item{ART.DryMatter..for.Moisture}{a numeric vector}
#'      \item{ART.DryMatter..for.Fertilizer}{a numeric vector}
#'      \item{ART.DryMatter..for.Moisture.Fertilizer}{a numeric vector}
#' }
#' @seealso \code{\link{Higgins1990Table5}}, \code{\link{art}}.
#' @source Wobbrock, J. O., Findlater, L., Gergle, D., and Higgins, J. J.
#' \emph{ARTool}. \url{https://depts.washington.edu/acelab/proj/art/}.
#' @keywords datasets internal
NULL


#' Split-plot Experiment Examining Effect of Moisture and Fertilizer on Dry
#' Matter in Peat Pots
#'
#' This dataset comes from a split-plot experiment examining \code{Tray}s of 4
#' peat pots each. \code{Moisture} was varied between \code{Tray}s (i.e. it was
#' the whole-plot treatment) and \code{Fertilizer} was varied within
#' \code{Tray}s (i.e. it was the sub-plot treatment). The outcome measure was
#' \code{DryMatter}.
#'
#' This dataset, originally from Milliken & Johnson (1984), is reproduced here
#' from Higgins \emph{et al.} (1990).
#'
#'
#' @name Higgins1990Table5
#' @docType data
#' @format A data frame with 48 observations on the following 4 variables.
#' \describe{
#'      \item{Tray}{a factor with levels \code{"t1"} .. \code{"t12"}}
#'      \item{Moisture}{a factor with levels \code{"m1"} .. \code{"m4"}}
#'      \item{Fertilizer}{a factor with levels \code{"f1"} .. \code{"f4"}}
#'      \item{DryMatter}{a numeric vector}
#' }
#' @seealso See \code{\link{art}} for a more complete example. See also
#' \code{\link{anova.art}}.
#' @references Higgins, J. J., Blair, R. C. and Tashtoush, S. (1990). The
#' aligned rank transform procedure.  \emph{Proceedings of the Conference on
#' Applied Statistics in Agriculture}. Manhattan, Kansas: Kansas State
#' University, pp. 185-195.
#' @source Milliken, G.A., Johnson, D.E. (1984). \emph{Analysis of Messy Data
#' Vol I: Designed Experiments}. Van Nostrand Reinhold Company, New York.
#' @keywords datasets
#' @examples
#'
#' data(Higgins1990Table5)
#'
#' ## run aligned-rank transform and ANOVA on the data
#' m <- art(DryMatter ~ Moisture*Fertilizer + (1|Tray), data=Higgins1990Table5)
#' anova(m)
#'
NULL


#' Aligned Rank Transformed Version of HigginsABC
#'
#' The ART version of \code{\link{HigginsABC}} as produced by the original
#' ARTool, used to test the correctness of \code{\link{art}} output.
#'
#'
#' @name HigginsABC.art
#' @docType data
#' @format A data frame with 16 observations on the following 19 variables.
#' \describe{
#'      \item{Subject}{a factor with levels \code{"s1"} .. \code{"s8"}}
#'      \item{A}{a factor with levels \code{"a1"} \code{"a2"}}
#'      \item{B}{a factor with levels \code{"b1"} \code{"b2"}}
#'      \item{C}{a factor with levels \code{"c1"} \code{"c2"}}
#'      \item{Y}{a numeric vector}
#'      \item{aligned.Y..for.A}{a numeric vector}
#'      \item{aligned.Y..for.B}{a numeric vector}
#'      \item{aligned.Y..for.A.B}{a numeric vector}
#'      \item{aligned.Y..for.C}{a numeric vector}
#'      \item{aligned.Y..for.A.C}{a numeric vector}
#'      \item{aligned.Y..for.B.C}{a numeric vector}
#'      \item{aligned.Y..for.A.B.C}{a numeric vector}
#'      \item{ART.Y..for.A}{a numeric vector}
#'      \item{ART.Y..for.B}{a numeric vector}
#'      \item{ART.Y..for.A.B}{a numeric vector}
#'      \item{ART.Y..for.C}{a numeric vector}
#'      \item{ART.Y..for.A.C}{a numeric vector}
#'      \item{ART.Y..for.B.C}{a numeric vector}
#'      \item{ART.Y..for.A.B.C}{a numeric vector}
#' }
#' @seealso \code{\link{HigginsABC}}, \code{\link{art}}.
#' @source Wobbrock, J. O., Findlater, L., Gergle, D., and Higgins, J. J.
#' \emph{ARTool}. \url{https://depts.washington.edu/acelab/proj/art/}.
#' @keywords datasets internal
NULL


#' Synthetic 2x2x2 Mixed Design Experiment
#'
#' Synthetic data from an experiment with two between-\code{Subject}s factors
#' (\code{A} and \code{B}) having two levels each and one
#' within-\code{Subject}s factor (\code{C}) with two levels.
#'
#'
#' @name HigginsABC
#' @docType data
#' @format A data frame with 16 observations on the following 5 variables.
#' \describe{
#'      \item{Subject}{a factor with levels \code{"s1"} .. \code{"s8"}}
#'      \item{A}{a factor with levels \code{"a1"} \code{"a2"}}
#'      \item{B}{a factor with levels \code{"b1"} \code{"b2"}}
#'      \item{C}{a factor with levels \code{"c1"} \code{"c2"}}
#'      \item{Y}{a numeric vector}
#' }
#' @seealso \code{\link{art}}, \code{\link{anova.art}}.
#' @source Wobbrock, J. O., Findlater, L., Gergle, D., and Higgins, J. J.
#' \emph{ARTool}. \url{https://depts.washington.edu/acelab/proj/art/}.
#' @keywords datasets
#' @examples
#' \dontrun{
#' data(HigginsABC, HigginsABC.art, package="ARTool")
#'
#' ## run aligned-rank transform and ANOVA on the data
#' m <- art(Y ~ A*B*C + Error(Subject), data=HigginsABC)
#' anova(m)
#' }
NULL


#' Synthetic Data Used in the Contrast Test Vignette
#'
#' See (\code{vignette("art-contrasts")} for a description of this data.
#'
#'
#' @name InteractionTestData
#' @docType data
#' @seealso \code{\link{art}}, \code{\link{anova.art}}.
#' @keywords datasets
#' @examples
#' \dontrun{
#' ## see this vignette
#' vignette("art-contrasts")
#' }
NULL
