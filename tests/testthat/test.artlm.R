# Tests for artlm
#
# Author: mjskay
###############################################################################

context("artlm")

test_that("artlm returns models whose data can be recovered by emmeans", {
    skip_if_not_installed("emmeans")

    data(Higgins1990Table5, package="ARTool")


    #run art without grouping term to use lm
    m = art(DryMatter ~ Moisture*Fertilizer, data=Higgins1990Table5)

    #should be able to run emmeans on the linear model
    #this will only work if emmeans can recover the data using
    #emmeans:::recover.data, which will only work if artlm
    #correctly sets the environment of the returned model
    expect_equal(
      summary(pairs(emmeans::emmeans(artlm(m, "Moisture"), "Moisture")))$t,
      c(-7.40049, -10.82022, -4.86242, -3.41972, 2.53808, 5.95780),
      tolerance = 0.0001
    )


    #run art with grouping term to force lmer
    m = art(DryMatter ~ Moisture*Fertilizer + (1|Tray), data=Higgins1990Table5)

    #will only work if emmeans can recover data (see comment above)
    expect_equal(
      summary(pairs(emmeans::emmeans(artlm(m, "Moisture"), "Moisture")))$t,
      c(-5.60684, -8.19773, -3.68392, -2.59089, 1.92292, 4.51381),
      tolerance = 0.0001
    )


    #run art with Error term to force aov
    m = art(DryMatter ~ Moisture*Fertilizer + Error(Tray), data=Higgins1990Table5)

    #will only work if emmeans can recover data (see comment above)
    expect_equal(
      summary(pairs(emmeans::emmeans(artlm(m, "Moisture"), "Moisture")))$t,
      c(-5.60684, -8.19773, -3.68392, -2.59089, 1.92292, 4.51381),
      tolerance = 0.0001
    )
})
