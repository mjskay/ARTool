# Tests for artlm
# 
# Author: mjskay
###############################################################################

library(testthat)
library(ARTool)

context("artlm")

test_that("artlm returns models whose data can be recovered by lsmeans", {
    skip_if_not_installed("lsmeans")
    require(lsmeans)

    data(HigginsABC, HigginsABC.art, package="ARTool")
    
    #run art with grouping term to force lmer
    m = art(Y ~ A*B*C + (1|Subject), data=HigginsABC)
    
    #should be able to run lsmeans on the linear model
    #this will only work if lsmeans can recover the data using
    #lsmeans:::recover.data, which will only work if artlm
    #correctly sets the environment of the returned model
    expect_equal(round(summary(lsmeans(artlm(m, "A"), pairwise ~ A)$contrasts)$p.value, 8), 0.00702493)
    
    #run art without grouping term to use lm
    m = art(Y ~ A*B*C, data=HigginsABC)
    
    #will only work if lsmeans can recover data (see comment above)
    expect_equal(round(summary(lsmeans(artlm(m, "A"), pairwise ~ A)$contrasts)$p.value, 8), 0.00093959)
})
