# Tests for anova.art
#
# Author: mjskay
###############################################################################

library(testthat)
library(ARTool)

context("anova.art")


comparable.anova = function(m, include.error=FALSE) {
    #get an anova from a model in a form that is easy to compare
    #(known column subset and rounded numeric results)
    a = suppressWarnings(as.data.frame(anova(m)))
    #extract character cols and convert to factors for consistency in comparison
    character_cols = lapply(a[,c("Term", if (include.error) "Error"), drop=FALSE], factor)
    #some anovas use "F value" instead of just "F", standardize
    if (!is.null(a$`F value`)) a$F = a$`F value`
    #extract numeric cols and round for consistency
    numeric_cols = round(a[,c("Df","Df.res","F","Pr(>F)")], digits=4)
    #stick it all back together and standardize the rownames
    a = cbind(character_cols, numeric_cols)
    rownames(a) = 1:nrow(a)
    a
}

test_that("anova.art of Higgins1990Table5 matches results of the original ARTool", {
    data(Higgins1990Table5, package="ARTool")

    #reference result
    ref = data.frame(
        Term = factor(c("Moisture", "Fertilizer", "Moisture:Fertilizer")),
        Error = factor(c("Tray", "Within", "Within")),
        Df = c(3, 3, 9),
        Df.res = c(8, 24, 24),
        "F" = c(23.8326,  122.4017, 5.1180),
        "Pr(>F)" = c(0.0002, 0, 0.0006),
        check.names = FALSE
    )

    #run art using repeated measures anova
    m = art(DryMatter ~ Moisture*Fertilizer + Error(Tray), data=Higgins1990Table5)
    a = comparable.anova(m, include.error=TRUE)
    expect_equal(a, ref, tolerance = 0.0001)

    #run art using mixed effects model
    m = art(DryMatter ~ Moisture*Fertilizer + (1|Tray), data=Higgins1990Table5)
    a = comparable.anova(m)
    expect_equal(a, ref[-2], tolerance = 0.0001) #no Error col
})

test_that("anova.art of Higgins1990Table1 matches results of the original ARTool", {
    data(Higgins1990Table1, package="ARTool")

    #reference result
    ref = data.frame(
        Term = factor(c("Row", "Column", "Row:Column")),
        Df = c(2,  2, 4),
        Df.res = c(27, 27, 27),
        "F" = c(29.9925, 77.8668, 0.6417),
        "Pr(>F)" = c(0, 0, 0.6374),
        check.names = FALSE
    )

    #run art using linear model
    m = art(Response ~ Row*Column, data=Higgins1990Table1)
    a = comparable.anova(m)
    expect_equal(a, ref)
})

test_that("anova.art of HigginsABC matches results of the original ARTool", {
    data(HigginsABC, package="ARTool")

    #reference result
    ref = data.frame(
        Term = factor(c("A", "B", "C", "A:B", "A:C", "B:C", "A:B:C")),
        Error = factor(c("Subject", "Subject", "Within", "Subject", "Within",  "Within", "Within")),
        Df = c(1, 1, 1, 1, 1, 1, 1),
        Df.res = c(4,  4, 4, 4, 4, 4, 4),
        F = c(120.4706, 120.4706, 14.3217, 81.92,  0.1259, 0.2319, 0.9715),
        "Pr(>F)" = c(4e-04, 4e-04, 0.0194, 8e-04,  0.7407, 0.6553, 0.3801),
        check.names = FALSE
    )

    #run art using linear model
    m = art(Y ~ A*B*C + Error(Subject), data=HigginsABC)
    a = comparable.anova(m, include.error=TRUE)
    expect_equal(a, ref)
})
