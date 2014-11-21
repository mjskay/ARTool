# Tests for parse.art.formula
# 
# Author: mjskay
###############################################################################

library(testthat)
library(ARTool)


test_that("art formula must have 1 response variable", {
    expect_error(ARTool:::parse.art.formula(~ a*b*c), 'Model must have exactly one independent variable \\(got 0\\)')
    expect_error(ARTool:::parse.art.formula(~ 0),     'Model must have exactly one independent variable \\(got 0\\)')
    expect_error(ARTool:::parse.art.formula(~ (1|c)), 'Model must have exactly one independent variable \\(got 0\\)')
})

test_that("art formula must have an intercept", {
    expect_error(ARTool:::parse.art.formula(y ~ a*b*c + 0), 'Model must have an intercept \\(got 0\\)')
    expect_error(ARTool:::parse.art.formula(y ~ a*b*c - 1), 'Model must have an intercept \\(got 0\\)')
    expect_error(ARTool:::parse.art.formula(y ~ 0),         'Model must have an intercept \\(got 0\\)')
    expect_error(ARTool:::parse.art.formula(y ~ 0 + (1|c)), 'Model must have an intercept \\(got 0\\)')
})

test_that("art formula must have at least 1 fixed effect", {
    expect_error(ARTool:::parse.art.formula(y ~ 1),         'Model must have at least one fixed effect \\(0 given\\)')
    expect_error(ARTool:::parse.art.formula(y ~ 1 + (1|c)), 'Model must have at least one fixed effect \\(0 given\\)')
})

test_that("art formula must have all interactions of fixed effects", {
    expect_error(ARTool:::parse.art.formula(y ~ a + b),         'Model must include all combinations of interactions of fixed effects.')
    expect_error(ARTool:::parse.art.formula(y ~ a + b),         'Model must include all combinations of interactions of fixed effects.')
    expect_error(ARTool:::parse.art.formula(y ~ a + b + c:d),   'Model must include all combinations of interactions of fixed effects.')	
    expect_error(ARTool:::parse.art.formula(y ~ a * b + c:d),   'Model must include all combinations of interactions of fixed effects.')
    expect_error(ARTool:::parse.art.formula(y ~ a * b + c),     'Model must include all combinations of interactions of fixed effects.')
    expect_error(ARTool:::parse.art.formula(y ~ a + b + (1|c)), 'Model must include all combinations of interactions of fixed effects.')
})

test_that("different variations on factorial model specifications are accepted by art", {
    expect_equal(ARTool:::parse.art.formula(y ~ a)$fixed.only, y ~ a)
    expect_equal(ARTool:::parse.art.formula(y ~ a)$fixed.terms, ~ a)
    expect_equal(ARTool:::parse.art.formula(y ~ a + (1|c))$fixed.only, y ~ a)
    expect_equal(ARTool:::parse.art.formula(y ~ a + (1|c))$fixed.terms, ~ a)
    expect_equal(ARTool:::parse.art.formula(y ~ a * b)$fixed.only, y ~ a * b)
    expect_equal(ARTool:::parse.art.formula(y ~ a * b)$fixed.terms, ~ a + b)
    expect_equal(ARTool:::parse.art.formula(y ~ a * b + (1|c))$fixed.only, y ~ a * b)
    expect_equal(ARTool:::parse.art.formula(y ~ a * b + (1|c))$fixed.terms, ~ a + b)
    expect_equal(ARTool:::parse.art.formula(y ~ a:b + a + b)$fixed.only, y ~ a*b)
    expect_equal(ARTool:::parse.art.formula(y ~ a:b + a + b)$fixed.terms, ~ a + b)
    expect_equal(ARTool:::parse.art.formula(y ~ a:b + a + b + (1|c))$fixed.only, y ~ a*b)
    expect_equal(ARTool:::parse.art.formula(y ~ a:b + a + b + (1|c))$fixed.terms, ~ a + b)
})
