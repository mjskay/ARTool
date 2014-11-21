# Tests for summary.art
# 
# Author: mjskay
###############################################################################

library(testthat)
library(ARTool)


test_that("summary(art) gives a warning on models with approx nonzero F on aligned responses not of interest", {
    df = data.frame(y=1:20, a=factor(rep(c(1,2),10)))
    df$c = df$a
    df$c[[2]] = "1"
    df$c[[3]] = "2"
    m = art(y ~ a*c, data=df)
    
    expect_warning(summary(m), "F values of ANOVAs on aligned responses not of interest are not all ~0. ART may not be appropriate.")
})
