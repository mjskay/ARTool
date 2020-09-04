context("artcon")

# artlm.con tests if artlm.con and artcon match and if artlm.con and artlm are the 
# same in the single-factor case (which of course by transitivity tests if artcon and artlm 
# are the same in the single-factor case)

# Note: adjust method doesn't matter since t ratios don't get adjusted

test_that("artcon returns same result as ARTool.exe + JMP: no grouping term, two-factor model", {
  data(Higgins1990Table5, package="ARTool")
  
  # run art without grouping term to use lm
  m = art(DryMatter ~ Moisture*Fertilizer, data=Higgins1990Table5)
  
  # single-factor contrasts
  expect_equal(
    summary(artcon(m, "Moisture"))$t,
    c(-7.400493839,-10.82021662,-4.862418335,-3.419722785,2.5380755044,5.9577982894),
    tolerance = 0.0001
  )
 
  # two-factor contrasts
  # compare "m1,f1 - m1,f2", "m1,f1 - m2,f1", and "m1,f1, m2,f2" to results from JMP.
  expect_equal(
    (summary(artcon(m, "Moisture:Fertilizer")) %>% filter(contrast %in% c("m1,f1 - m1,f2", "m1,f1 - m2,f1", "m1,f1 - m2,f2")) %>% select("t.ratio"))[[1]],
    c(-0.7886463535, -3.0362884611, -5.0079043449),
    tolerance = 0.0001
  )
})

test_that("artcon returns same result as ARTool.exe + JMP: no grouping term, three-factor model", {
  data("ElkinABC")
  
  m = art(Y ~ A*B*C, data=ElkinABC)
  
  # single-factor contrasts
  expect_equal(
    summary(artcon(m, "A"))$t,
    c(-5.772),
    tolerance = 0.0001
  )
  
  # two-factor contrasts
  # compare "A1,B1 - A1,B2", "A1,B1 - A2,B1", and "A1,B1, A2,B2" to results from JMP.
  expect_equal(
    (summary(artcon(m, "A:B")) %>% filter(contrast %in% c("A1,B1 - A1,B2", "A1,B1 - A2,B1", "A1,B1 - A2,B2")) %>% select("t.ratio"))[[1]],
    c(-0.022614705, -4.6247071, -3.177366003),
    tolerance = 0.0001
  )
  
  # three-factor contrasts
  # compare "A1,B1,C1 - A1,B1,C2", "A1,B1,C1 - A1,B2,C2", and "A1,B1,C1 - A2,B2,C2" to results from JMP.
  expect_equal(
    (summary(artcon(m, "A:B:C")) %>% filter(contrast %in% c("A1,B1,C1 - A1,B1,C2", "A1,B1,C1 - A1,B2,C2", "A1,B1,C1 - A2,B2,C2")) %>% select("t.ratio"))[[1]],
    c(-2.060574615, -2.060574615, -2.007739368),
    tolerance = 0.0001
  )
})
  
test_that("artcon returns same result as ARTool.exe + JMP: with grouping term, two-factor model", {
  data(Higgins1990Table5, package="ARTool")
  
  #run with grouping term to force lmer
  m = art(DryMatter ~ Moisture*Fertilizer + (1|Tray), data=Higgins1990Table5)
  
  # single-factor contrasts
  expect_equal(
    summary(artcon(m, "Moisture"))$t,
    c(-5.606839446, -8.197725544, -3.68391617, -2.590886098, 1.9229232756, 4.5138093733),
    tolerance = 0.0001
  )

  # two-factor contrasts
  # compare "m1,f1 - m1,f2", "m1,f1 - m2,f1", and "m1,f1, m2,f2" to results from JMP.
  expect_equal(
    (summary(artcon(m, "Moisture:Fertilizer")) %>% filter(contrast %in% c("m1,f1 - m1,f2", "m1,f1 - m2,f1", "m1,f1 - m2,f2")) %>% select("t.ratio"))[[1]],
    c(-1.107980814, -3.036288461, -5.007904345),
    tolerance = 0.0001
  )
})

test_that("artcon returns same result as ARTool.exe + JMP: with grouping term, three-factor model", {
  data("ElkinABC")
  
  m = art(Y ~ A*B*C + (1|S), data=ElkinABC)
  
  # single-factor contrasts
  expect_equal(
    summary(artcon(m, "A"))$t,
    c(-16.98),
    tolerance = 0.001
  )
  
  # two-factor contrasts
  # compare "A1,B1 - A1,B2", "A1,B1 - A2,B1", and "A1,B1, A2,B2" to results from JMP.
  expect_equal(
    (summary(artcon(m, "A:B")) %>% filter(contrast %in% c("A1,B1 - A1,B2", "A1,B1 - A2,B1", "A1,B1 - A2,B2")) %>% select("t.ratio"))[[1]],
    c(-0.072115146, -14.74754746, -10.13217808),
    tolerance = 0.0001
  )
  
  # three-factor contrasts
  # compare "A1,B1,C1 - A1,B1,C2", "A1,B1,C1 - A1,B2,C2", and "A1,B1,C1 - A2,B2,C2" to results from JMP.
  expect_equal(
    (summary(artcon(m, "A:B:C")) %>% filter(contrast %in% c("A1,B1,C1 - A1,B1,C2", "A1,B1,C1 - A1,B2,C2", "A1,B1,C1 - A2,B2,C2")) %>% select("t.ratio"))[[1]],
    c(-8.041788515, -8.041788515, -7.835588809),
    tolerance = 0.0001
  )
})

test_that("artcon returns same result as ARTool.exe + JMP: with error term, two-factor model", {
  data(Higgins1990Table5, package="ARTool")

  #run art with Error term to force aov
  m = art(DryMatter ~ Moisture*Fertilizer + Error(Tray), data=Higgins1990Table5)
  
  # single-factor contrasts
  expect_equal(
    summary(artcon(m, "Moisture"))$t,
    c(-5.606839446, -8.197725544, -3.68391617, -2.590886098, 1.9229232756, 4.5138093733),
    tolerance = 0.0001
  )
  
  # two-factor contrasts
  # can't use Higgins Table 5 because it's unbalanced with respect to Moisture:Fertilizer
  data(ElkinAB)
  m = art(Y ~ A*B + Error(S), data=ElkinAB)
  
  # compare "A1,B1 - A1,B2", "A1,B1 - A2,B1", and "A1,B1, A2,B2" to results from JMP.
  expect_equal(
    (summary(artcon(m, "A:B")) %>% filter(contrast %in% c("A1,B1 - A1,B2", "A1,B1 - A2,B1", "A1,B1 - A2,B2")) %>% select("t.ratio"))[[1]],
    c(-0.487275267, -16.40493399, -20.7904114),
    tolerance = 0.0001
  )
  
})

test_that("artcon returns same result as ARTool.exe + JMP: with error term, two-factor model", {
  data("ElkinABC")
  
  m = art(Y ~ A*B*C + Error(S), data=ElkinABC)
  
  # single-factor contrasts
  expect_equal(
    summary(artcon(m, "A"))$t,
    c(-16.98),
    tolerance = 0.001
  )
  
  # two-factor contrasts
  # compare "A1,B1 - A1,B2", "A1,B1 - A2,B1", and "A1,B1, A2,B2" to results from JMP.
  expect_equal(
    (summary(artcon(m, "A:B")) %>% filter(contrast %in% c("A1,B1 - A1,B2", "A1,B1 - A2,B1", "A1,B1 - A2,B2")) %>% select("t.ratio"))[[1]],
    c(-0.072115146, -14.74754746, -10.13217808),
    tolerance = 0.0001
  )
  
  # three-factor contrasts
  # compare "A1,B1,C1 - A1,B1,C2", "A1,B1,C1 - A1,B2,C2", and "A1,B1,C1 - A2,B2,C2" to results from JMP.
  expect_equal(
    (summary(artcon(m, "A:B:C")) %>% filter(contrast %in% c("A1,B1,C1 - A1,B1,C2", "A1,B1,C1 - A1,B2,C2", "A1,B1,C1 - A2,B2,C2")) %>% select("t.ratio"))[[1]],
    c(-8.041788515, -8.041788515, -7.835588809),
    tolerance = 0.0001
  )
})
