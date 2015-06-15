# ARTool: R Package for the Aligned Rank Transform for Nonparametric Factorial ANOVAs 

[![Build Status](https://travis-ci.org/mjskay/ARTool.png?branch=master)](https://travis-ci.org/mjskay/ARTool)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ARTool)](http://cran.r-project.org/web/packages/ARTool)

_Matthew Kay, University of Washington <mjskay@uw.edu>_

_Jacob O. Wobbrock, University of Washington <wobbrock@uw.edu>_

ARTool is an R package implementing the Aligned Rank Transform for conducting nonparametric analyses of variance on factorial models. This implementation is based on the ART procedure as used in the original implementation of [ARTool](http://depts.washington.edu/aimgroup/proj/art/) by Wobbrock et al. 

__Note__: The documentation of this package assumes some level of familiarity with when and why you may want to use the aligned rank transform; the [ARTool page](http://depts.washington.edu/aimgroup/proj/art/) provides a more in-depth (and highly approachable) introduction to the aligned rank transform and the motivation for its use.

## Installation

You can install the latest released version from CRAN with this R command:

```R
> install.packages("ARTool")
```

__Or__, you can install the latest development version from GitHub with these R commands: 

```R
> install.packages("devtools")
> devtools::install_github("mjskay/ARTool")
```

## Example

The general approach to using ART is to transform your data using `art`, verify the ART procedure is appropriate to the dataset using `summary`, and then run an anova on the transformed data using `anova`.

First, let us load some example data:

```R
> library(ARTool)
> data(Higgins1990Table5)
```

`Higgins1990Table5` is a data frame from an experiment in which the effects of `Moisture` and `Fertilizer` on `DryMatter` in peat pots was tested. Four pots were placed on each `Tray`, with `Moisture` varied between `Tray`s and `Fertilizer` varied within `Tray`s. We can see the basic structure of the data:

```R
> str(Higgins1990Table5)
```
```
'data.frame':   48 obs. of  4 variables:
 $ Tray      : Factor w/ 12 levels "1","2","3","4",..: 1 1 1 1 2 2 2 2 3 3 ...
 $ Moisture  : Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 1 1 1 1 1 ...
 $ Fertilizer: Factor w/ 4 levels "1","2","3","4": 1 2 3 4 1 2 3 4 1 2 ...
 $ DryMatter : num  3.3 4.3 4.5 5.8 4 4.1 6.5 7.3 1.9 3.8 ...
```

```R
> head(Higgins1990Table5, n=8)
```
```
   Tray Moisture Fertilizer DryMatter
1     1        1          1       3.3
2     1        1          2       4.3
3     1        1          3       4.5
4     1        1          4       5.8
5     2        1          1       4.0
6     2        1          2       4.1
7     2        1          3       6.5
8     2        1          4       7.3
```

To analyze this data using the aligned rank transform, we first transform the data using `art`. We specify the response variable (`DryMatter`), the fixed effects and all of their interactions (`Moisture*Fertilizer`, or equivalently `Moisture + Fertilizer + Moisture:Fertilizer`), and any grouping terms if present (here, `(1|Tray)`):

```R
> m <- art(DryMatter ~ Moisture*Fertilizer + (1|Tray), data=Higgins1990Table5)
```

To verify that the ART procedure was correctly applied and is appropriate for this dataset, we can look at the output of `summary`:

```R
> summary(m)
```
```
Aligned Rank Transform of Factorial Model

Call:
art(formula = DryMatter ~ Moisture * Fertilizer + (1 | Tray), 
    data = Higgins1990Table5)

Column sums of aligned responses (should all be ~0):
           Moisture          Fertilizer Moisture:Fertilizer 
                  0                   0                   0 

F values of ANOVAs on aligned responses not of interest (should all be ~0):
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      0       0       0       0       0       0 
```

We see that the columns sums of aligned responses and the F values of ANOVAs on aligned responses not of interest are all ~0, indicating that the alignment correctly "stripped out" effects not of interest. Thus, we can apply the ANOVA on the transformed data:

```R
> anova(m)
```
```
Aligned Rank Transform Anova Table (Type III tests)

Response: art(DryMatter)
                          F Df Df.res    Pr(>F)    
Moisture             23.833  3      8 0.0002420 ***
Fertilizer          122.402  3     24 1.112e-14 ***
Moisture:Fertilizer   5.118  9     24 0.0006466 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## Problems

Should you encounter any issues with this package, contact Matthew Kay (<mjskay@uw.edu>). If you have found a bug, please file it [here](https://github.com/mjskay/ARTool/issues/new) with minimal code to reproduce the issue.

## Citations

Kay M and Wobbrock J (2014). _ARTool: Aligned Rank Transform for
Nonparametric Factorial ANOVAs_. R package version 0.9.3, <https://github.com/mjskay/ARTool>.

Wobbrock J, Findlater L, Gergle D and Higgins J (2011). "The Aligned
Rank Transform for Nonparametric Factorial Analyses Using Only ANOVA
Procedures." In _Proceedings of the ACM Conference on Human Factors in
Computing Systems (CHI '11)_, Vancouver, British Columbia (May 7-12, 2011). New York: ACM Press, pp. 143-146. <http://depts.washington.edu/aimgroup/proj/art/>.
