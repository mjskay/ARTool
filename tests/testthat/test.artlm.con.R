# Tests for artlm.con
#
# Author: lelkin
###############################################################################

library(testthat)
library(ARTool)

context("artlm.con")

test_that("artlm.con matches with art.con",{
  data(Higgins1990Table5, package = "ARTool")

  # run art without grouping term to use lm
  m = art(DryMatter ~ Moisture*Fertilizer, data=Higgins1990Table5)

  expect_equal(
    summary(art.con(m, "Moisture")),
    summary(contrast(emmeans(artlm.con(m, "Moisture"), ~ Moisture), method="pairwise"))
  )

  expect_equal(
    summary(art.con(m, "Moisture:Fertilizer")),
    summary(contrast(emmeans(artlm.con(m, "Moisture:Fertilizer"), ~ MoistureFertilizer), method="pairwise"))
  )

  # grouping term to force lmer
  m = art(DryMatter ~ Moisture*Fertilizer + (1|Tray), data=Higgins1990Table5)

  expect_equal(
    summary(art.con(m, "Moisture")),
    summary(contrast(emmeans(artlm.con(m, "Moisture"), ~ Moisture), method="pairwise"))
  )

  expect_equal(
    summary(art.con(m, "Moisture:Fertilizer")),
    summary(contrast(emmeans(artlm.con(m, "Moisture:Fertilizer"), ~ MoistureFertilizer), method="pairwise"))
  )

  # error term
  # can't use Higgins Table 5 because it's unbalanced with respect to Moisture:Fertilizer
  data(ElkinAB, package = "ARTool")
  m = art(Y ~ A*B + Error(S), data=ElkinAB)

  expect_equal(
    summary(art.con(m, "A")),
    summary(contrast(emmeans(artlm.con(m, "A"), ~ A), method="pairwise"))
  )

  expect_equal(
    summary(art.con(m, "A:B")),
    summary(contrast(emmeans(artlm.con(m, "A:B"), ~ AB), method="pairwise"))
  )
})

test_that("artlm.con matches with artlm in single-factor case",{

  # run art without grouping term to use lm
  m = art(DryMatter ~ Moisture*Fertilizer, data=Higgins1990Table5)

  expect_equal(
    summary(contrast(emmeans(artlm.con(m, "Moisture"), ~ Moisture), method="pairwise")),
    summary(contrast(emmeans(artlm(m, "Moisture"), ~ Moisture), method="pairwise"))
  )

  # run art with grouping term to use lmer
  m = art(DryMatter ~ Moisture*Fertilizer + (1|Tray), data=Higgins1990Table5)

  expect_equal(
    summary(contrast(emmeans(artlm.con(m, "Moisture"), ~ Moisture), method="pairwise")),
    summary(contrast(emmeans(artlm(m, "Moisture"), ~ Moisture), method="pairwise"))
  )

  # run art with error term to force aov
  m = art(DryMatter ~ Moisture*Fertilizer + Error(Tray), data=Higgins1990Table5)

  expect_equal(
    summary(contrast(emmeans(artlm.con(m, "Moisture"), ~ Moisture), method="pairwise")),
    summary(contrast(emmeans(artlm(m, "Moisture"), ~ Moisture), method="pairwise"))
  )

})
