\name{art}
\alias{art}
\title{Aligned Rank Transform}
\description{
Apply the aligned rank transform to a factorial model (with optional grouping terms) in order to
conduct nonparametric analyses of variance on models with numeric or ordinal responses.
}
\usage{
art(formula, data, 
    rank.comparison.digits = -floor(log10(.Machine$double.eps^0.5)))
}
\arguments{
  \item{formula}{
A factorial formula with optional grouping terms. Should be a formula with a single
response variable (left-hand side) and one or more terms with all interactions on the right-hand side, e.g.
\code{y ~ x} or \code{y ~ a*b*c} or \code{y ~ a + b + b:c}. May also include grouping terms, e.g. 
\code{y ~ a*b*c + (1|d)}. See 'Details'.
}
  \item{data}{
An optional data frame containing the variables in the model. 
}
  \item{rank.comparison.digits}{
The number of digits to round aligned responses to before ranking (to ensure ties are computed consistently). 
See the \code{digits} argument of \code{\link{round}}. The default value is based on the default \code{tolerance} used for 
fuzzy comparison in \code{all.equal}. 
}
}
\details{
The aligned rank transform allows a nonparametric analysis of variance to be conducted 
on factorial models with fixed and random effects and numeric or ordinal responses. This is done by first aligning 
and ranking the fixed effects using this function, then conducting an analysis of variance 
on linear models built from the transformed data using \code{\link{anova.art}} (see 'Examples'). 
The model specified using this function \emph{must} include all interactions of fixed effects. 

The \code{formula} should contain a single response variable (left-hand side) that can be numeric,
an ordered factor, or logical. The right-hand side of the formula should contain one or more
fixed effect factors and zero or more grouping terms. All possible interactions of the fixed effect terms 
must be included. For example, \code{y ~ x} and \code{y ~ a*b*c} and \code{y ~ a + b + b:c} are legal, 
but \code{y ~ a + b} is not, as it omits the interaction \code{a:b}. Grouping terms are specified as in 
\code{\link{lmer}}, e.g. \code{y ~ a*b*c + (1|d)} includes the random intercept term \code{(1|d)}.
Grouping terms are not transformed, but are included in the model when ANOVAs are conducted,
see \code{\link{anova.art}}.

For details on the transformation itself, see Wobbrock \emph{et al.} (2011) or the ARTool
website: \url{http://depts.washington.edu/aimgroup/proj/art/}.
}
\value{
An object of class \code{"art"}: 

\item{call}{ The call used to generate the transformed data. }
\item{formula}{ The formula used to generate the transformed data. }
\item{cell.means}{ A data frame of cell means for each fixed term and interaction on the right-hand side of formula. }
\item{estimated.effects}{ A data frame of estimated effects for each fixed term and interaction on the right-hand side of formula. }
\item{residuals}{ A vector of residuals (response - cell mean of highest-order interaction). }
\item{aligned}{ A data frame of aligned responses for each fixed term and interaction on the right-hand side of formula. }
\item{aligned.ranks}{ A data frame of aligned and ranked responses for each fixed term and interaction on the right-hand side of formula. }
\item{data}{ The input data frame }
\item{n.grouping.variables}{ The number of grouping variables in the input formula. }

For a complete description of cell means, estimated effects,
aligned ranks, etc., in the above output, see Wobbrock \emph{et al.} (2011).
}
\references{
Wobbrock, J. O., Findlater, L., Gergle, D., and Higgins, J. J. \emph{ARTool}. \url{http://depts.washington.edu/aimgroup/proj/art/}.

Wobbrock, J. O., Findlater, L., Gergle, D., and Higgins, J. J. (2011). The Aligned Rank Transform for nonparametric factorial 
analyses using only ANOVA procedures. \emph{Proceedings of the ACM Conference on Human Factors in Computing Systems (CHI '11)}. 
Vancouver, British Columbia (May 7-12, 2011). New York: ACM Press, pp. 143-146. 
}
\author{
Matthew Kay
}
\seealso{
\code{\link{summary.art}}, \code{\link{anova.art}}, \code{\link{artlm}}.
}
\examples{\dontrun{
data(Higgins1990Table5, package="ARTool")

## perform aligned rank transform
m <- art(DryMatter ~ Moisture*Fertilizer + (1|Tray), data=Higgins1990Table5)

## see summary data to ensure aligned rank transform is appropriate for this data
summary(m)
## looks good (aligned effects sum to 0 and F values on aligned responses 
## not of interest are all ~0)

## we can always look at the anova of aligned data if we want more detail 
## to assess the appropriateness of ART.  F values in this anova should all
## be approx 0.
anova(m, response="aligned")

## then we can run an anova on the ART responses (equivalent to anova(m, response="art"))
anova(m)

## if we want post-hoc tests, artlm(m, term) returns the linear model for the
## given term
## which we can then examine using our preferred method (lsmeans, glht, etc)
## e.g., pairwise contrasts on Moisture:
library(lsmeans)
lsmeans(artlm(m, "Moisture"), pairwise ~ Moisture)

## pairwise contrasts on Fertilizer:
lsmeans(artlm(m, "Fertilizer"), pairwise ~ Fertilizer)
}}
\keyword{nonparametric}
