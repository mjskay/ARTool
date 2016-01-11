# ARTool: R Package for the Aligned Rank Transform for Nonparametric Factorial ANOVAs 

[![Build Status](https://travis-ci.org/mjskay/ARTool.png?branch=master)](https://travis-ci.org/mjskay/ARTool)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ARTool)](http://cran.r-project.org/web/packages/ARTool)
[![GPL >= 2](https://img.shields.io/badge/GPL-%E2%89%A52-brightgreen.svg)](https://cran.r-project.org/web/licenses/GPL-3)

_Matthew Kay, University of Washington <mjskay@uw.edu>_<br>
_Jacob O. Wobbrock, University of Washington <wobbrock@uw.edu>_

ARTool is an R package implementing the Aligned Rank Transform for conducting
nonparametric analyses of variance on factorial models. This implementation is
based on the ART procedure as used in the original implementation of 
[ARTool](http://depts.washington.edu/aimgroup/proj/art/) by Wobbrock et al.

The package automates the Aligning-and-Ranking process using the `art` function.
It also automates the process of running a series of ANOVAs on the transformed
data and extracting the results of interest. It supports traditional ANOVA
models (fit using `lm`), repeated measures ANOVAs (fit using `aov`), and 
mixed effects models (fit using `lmer`); the model used is determined by the
formula passed to `art`.

__Note__: The documentation of this package assumes some level of familiarity
with when and why you may want to use the aligned rank transform; the 
[ARTool page](http://depts.washington.edu/aimgroup/proj/art/) provides a more in-depth (and
highly approachable) introduction to the aligned rank transform and the
motivation for its use.

## Installation

You can install the latest released version from CRAN with this R command:


```r
install.packages("ARTool")
```

__Or__, you can install the latest development version from GitHub with these R
commands:


```r
install.packages("devtools")
devtools::install_github("mjskay/ARTool")
```

## Example

The general approach to using ART is to transform your data using `art` , verify
the ART procedure is appropriate to the dataset using `summary` , and then run an
anova on the transformed data using `anova` .

First, let us load some example data:


```r
library(ARTool)
data(Higgins1990Table5)
```

`Higgins1990Table5`  is a data frame from an experiment in which the effects of `Moisture`
and `Fertilizer` on `DryMatter` in peat pots was tested. Four pots were placed on
each `Tray` , with `Moisture` varied between `Tray` s and `Fertilizer` varied
within `Tray` s. We can see the basic structure of the data:


```r
str(Higgins1990Table5)
```

```
## 'data.frame':	48 obs. of  4 variables:
##  $ Tray      : Factor w/ 12 levels "t1","t2","t3",..: 1 1 1 1 2 2 2 2 3 3 ...
##  $ Moisture  : Factor w/ 4 levels "m1","m2","m3",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ Fertilizer: Factor w/ 4 levels "f1","f2","f3",..: 1 2 3 4 1 2 3 4 1 2 ...
##  $ DryMatter : num  3.3 4.3 4.5 5.8 4 4.1 6.5 7.3 1.9 3.8 ...
```

```r
head(Higgins1990Table5, n=8)
```

```
##   Tray Moisture Fertilizer DryMatter
## 1   t1       m1         f1       3.3
## 2   t1       m1         f2       4.3
## 3   t1       m1         f3       4.5
## 4   t1       m1         f4       5.8
## 5   t2       m1         f1       4.0
## 6   t2       m1         f2       4.1
## 7   t2       m1         f3       6.5
## 8   t2       m1         f4       7.3
```

### Step 1: Transform the data

To analyze this data using the aligned rank transform, we first transform the
data using `art` . We specify the response variable (`DryMatter` ), the fixed
effects and all of their interactions (`Moisture*Fertilizer`, or equivalently 
`Moisture + Fertilizer + Moisture:Fertilizer`), and any grouping terms if 
present (here, `(1|Tray)` ). 

While `(1|Tray)` has no effect on the results of 
the aligned rank transformation, it will be used by `anova` to determine the 
type of model to run: when grouping terms are present, mixed effects models
are run using `lmer`. If you wish to use a repeated measures ANOVA instead of
a mixed effects model, you can use an `Error` term instead (see below for an
example of this). If you do not having repeated measures, do not include
any grouping terms or error terms. 


```r
m <- art(DryMatter ~ Moisture*Fertilizer + (1|Tray), data=Higgins1990Table5)
```

### Step 2: Verify appropriateness of ART

To verify that the ART procedure was correctly applied and is appropriate for
this dataset, we can look at the output of `summary` :


```r
summary(m)
```

```
## Aligned Rank Transform of Factorial Model
## 
## Call:
## art(formula = DryMatter ~ Moisture * Fertilizer + (1 | Tray), 
##     data = Higgins1990Table5)
## 
## Column sums of aligned responses (should all be ~0):
##            Moisture          Fertilizer Moisture:Fertilizer 
##                   0                   0                   0 
## 
## F values of ANOVAs on aligned responses not of interest (should all be ~0):
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0       0       0       0       0       0
```

We see that the columns sums of aligned responses and the F values of ANOVAs on
aligned responses not of interest are all ~0, indicating that the alignment
correctly "stripped out" effects not of interest. Thus, we can apply the ANOVA
on the transformed data. 

### Step 3: Run the ANOVA

ARTool automatically selects the model to be used
for the ANOVA. Because we have included a grouping term, `(1|Tray)`, ARTool
will fit mixed effects models using `lmer` and run the ANOVAs on them:


```r
anova(m)
```

```
## Analysis of Variance of Aligned Rank Transformed Data
## 
## Table Type: Analysis of Deviance Table (Type III Wald F tests with Kenward-Roger df) 
## Model: Mixed Effects (lmer)
## Response: art(DryMatter)
## 
##                             F Df Df.res     Pr(>F)    
## 1 Moisture             23.833  3      8 0.00024199 ***
## 2 Fertilizer          122.402  3     24 1.1124e-14 ***
## 3 Moisture:Fertilizer   5.118  9     24 0.00064665 ***
## ---
## Signif. codes:   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

### Alternative model: Repeated Measures ANOVA

This particular study could also be analyzed using a repeated measures ANOVA, 
yielding the same results (note that repeated measures ANOVAs and mixed
effects models will not always yield the same results). To instead run
a repeated measures ANOVA, add an `Error` term to the model as you
might for a call to `aov`:


```r
m <- art(DryMatter ~ Moisture*Fertilizer + Error(Tray), data=Higgins1990Table5)
anova(m)
```

```
## Analysis of Variance of Aligned Rank Transformed Data
## 
## Table Type: Repeated Measures Analysis of Variance Table (Type I) 
## Model: Repeated Measures (aov)
## Response: art(DryMatter)
## 
##                       Error Df Df.res F value     Pr(>F)    
## 1 Moisture             Tray  3      8  23.833 0.00024199 ***
## 2 Fertilizer          Withn  3     24 122.402 1.1124e-14 ***
## 3 Moisture:Fertilizer Withn  9     24   5.118 0.00064665 ***
## ---
## Signif. codes:   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## Contrast tests

For an example of how to run contrast tests on an `art` model, see this vignette:


```r
vignette("art-contrasts")
```

Also available [here](art-contrasts.md).

## Problems

Should you encounter any issues with this package, contact Matthew Kay
(<mjskay@uw.edu>). If you have found a bug, please file it 
[here](https://github.com/mjskay/ARTool/issues/new) with minimal code to 
reproduce the issue.

## Citations

Kay M and Wobbrock J (2014). _ARTool: Aligned Rank Transform for
Nonparametric Factorial ANOVAs_. R package version 0.10.0, <https://github.com/mjskay/ARTool>.

Wobbrock J, Findlater L, Gergle D and Higgins J (2011). "The Aligned
Rank Transform for Nonparametric Factorial Analyses Using Only ANOVA
Procedures." In _Proceedings of the ACM Conference on Human Factors in
Computing Systems (CHI '11)_, Vancouver, British Columbia (May 7-12, 2011). 
New York: ACM Press, pp. 143-146. <http://depts.washington.edu/aimgroup/proj/art/>.
