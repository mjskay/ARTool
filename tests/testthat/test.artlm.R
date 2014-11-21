# Tests for artlm
# 
# Author: mjskay
###############################################################################

library(testthat)
library(ARTool)
library(lsmeans)


test_that("artlm returns models that can be used by lsmeans", {
    ### verify that art on HigginsABC is correct
    data(HigginsABC, HigginsABC.art, package="ARTool")
    
    #run art on original data (with grouping term to force lmer)
    m = art(Y ~ A*B*C + (1|Subject), data=HigginsABC)
    
    #should be able to run lsmeans on the linear model
    #this will only work if lsmeans can recover the data using
    #lsmeans:::recover.data, which will only work if artlm
    #correctly sets the environment of the returned model
    expect_equal(round(summary(lsmeans(artlm(m, "A"), pairwise ~ A)$contrasts)$p.value, 8), 0.00702493)
    
    #run art on original data (without grouping term to use lm)
    m = art(Y ~ A*B*C, data=HigginsABC)
    
    #should be able to run lsmeans on the linear model
    #this will only work if lsmeans can recover the data using
    #lsmeans:::recover.data, which will only work if artlm
    #correctly sets the environment of the returned model
    expect_equal(round(summary(lsmeans(artlm(m, "A"), pairwise ~ A)$contrasts)$p.value, 8), 0.00093959)
})
