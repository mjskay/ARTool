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

    data(Higgins1990Table5, package="ARTool")
    
    
    #run art without grouping term to use lm
    m = art(DryMatter ~ Moisture*Fertilizer, data=Higgins1990Table5)
    
    #should be able to run lsmeans on the linear model
    #this will only work if lsmeans can recover the data using
    #lsmeans:::recover.data, which will only work if artlm
    #correctly sets the environment of the returned model
    expect_equal(
        round(summary(pairs(lsmeans(artlm(m, "Moisture"), "Moisture")))$t, 5),
        c(-7.40049, -10.82022, -4.86242, -3.41972, 2.53808, 5.95780)
    )
    
    
    #run art with grouping term to force lmer
    m = art(DryMatter ~ Moisture*Fertilizer + (1|Tray), data=Higgins1990Table5)
    
    #will only work if lsmeans can recover data (see comment above)
    expect_equal(
        round(summary(pairs(lsmeans(artlm(m, "Moisture"), "Moisture")))$t, 5),
        c(-5.60684, -8.19773, -3.68392, -2.59089, 1.92292, 4.51381)
    )
    

    #run art with Error term to force aov
    m = art(DryMatter ~ Moisture*Fertilizer + Error(Tray), data=Higgins1990Table5)
    
    #will only work if lsmeans can recover data (see comment above)
    expect_equal(
        round(summary(pairs(lsmeans(artlm(m, "Moisture"), "Moisture")))$t, 5),
        c(-5.60684, -8.19773, -3.68392, -2.59089, 1.92292, 4.51381)
    )
})
