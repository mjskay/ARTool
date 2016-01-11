# Tests for parse.art.formula
# 
# Author: mjskay
###############################################################################

library(testthat)
library(ARTool)

context("art formula parsing")

test_that("art formula must have 1 response variable", {
    expect_error(ARTool:::parse.art.formula(~ a*b*c), 'Model must have exactly one dependent variable \\(got 0\\)')
    expect_error(ARTool:::parse.art.formula(~ 0),     'Model must have exactly one dependent variable \\(got 0\\)')
    expect_error(ARTool:::parse.art.formula(~ (1|c)), 'Model must have exactly one dependent variable \\(got 0\\)')
    expect_error(ARTool:::parse.art.formula(~ Error(c)), 'Model must have exactly one dependent variable \\(got 0\\)')
})

test_that("art formula must have an intercept", {
    expect_error(ARTool:::parse.art.formula(y ~ a*b*c + 0), 'Model must have an intercept \\(got 0\\)')
    expect_error(ARTool:::parse.art.formula(y ~ a*b*c - 1), 'Model must have an intercept \\(got 0\\)')
    expect_error(ARTool:::parse.art.formula(y ~ 0),         'Model must have an intercept \\(got 0\\)')
    expect_error(ARTool:::parse.art.formula(y ~ 0 + (1|c)), 'Model must have an intercept \\(got 0\\)')
    expect_error(ARTool:::parse.art.formula(y ~ 0 + Error(c)), 'Model must have an intercept \\(got 0\\)')
})

test_that("art formula must have at least 1 fixed effect", {
    expect_error(ARTool:::parse.art.formula(y ~ 1),         'Model must have at least one fixed effect \\(0 given\\)')
    expect_error(ARTool:::parse.art.formula(y ~ 1 + (1|c)), 'Model must have at least one fixed effect \\(0 given\\)')
    expect_error(ARTool:::parse.art.formula(y ~ 1 + Error(c)), 'Model must have at least one fixed effect \\(0 given\\)')
})

test_that("art formula must have all interactions of fixed effects", {
    expect_error(ARTool:::parse.art.formula(y ~ a + b),         'Model must include all combinations of interactions of fixed effects.')
    expect_error(ARTool:::parse.art.formula(y ~ a + b),         'Model must include all combinations of interactions of fixed effects.')
    expect_error(ARTool:::parse.art.formula(y ~ a + b + c:d),   'Model must include all combinations of interactions of fixed effects.')	
    expect_error(ARTool:::parse.art.formula(y ~ a * b + c:d),   'Model must include all combinations of interactions of fixed effects.')
    expect_error(ARTool:::parse.art.formula(y ~ a * b + c),     'Model must include all combinations of interactions of fixed effects.')
    expect_error(ARTool:::parse.art.formula(y ~ a + b + (1|c)), 'Model must include all combinations of interactions of fixed effects.')
    expect_error(ARTool:::parse.art.formula(y ~ a + b + Error(c)), 'Model must include all combinations of interactions of fixed effects.')
})

test_that("different variations on factorial model specifications are accepted by art", {
    expect_equal(ARTool:::parse.art.formula(y ~ a)$fixed.only, y ~ a)
    expect_equal(ARTool:::parse.art.formula(y ~ a)$fixed.terms, ~ a)
    expect_equal(ARTool:::parse.art.formula(y ~ a + (1|c))$fixed.only, y ~ a)
    expect_equal(ARTool:::parse.art.formula(y ~ a + (1|c))$fixed.terms, ~ a)
    expect_equal(ARTool:::parse.art.formula(y ~ a + Error(c))$fixed.only, y ~ a)
    expect_equal(ARTool:::parse.art.formula(y ~ a + Error(c))$fixed.terms, ~ a)
    expect_equal(ARTool:::parse.art.formula(y ~ a * b)$fixed.only, y ~ a * b)
    expect_equal(ARTool:::parse.art.formula(y ~ a * b)$fixed.terms, ~ a + b)
    expect_equal(ARTool:::parse.art.formula(y ~ a * b + (1|c))$fixed.only, y ~ a * b)
    expect_equal(ARTool:::parse.art.formula(y ~ a * b + (1|c))$fixed.terms, ~ a + b)
    expect_equal(ARTool:::parse.art.formula(y ~ a * b + Error(c))$fixed.only, y ~ a * b)
    expect_equal(ARTool:::parse.art.formula(y ~ a * b + Error(c))$fixed.terms, ~ a + b)
    expect_equal(ARTool:::parse.art.formula(y ~ a:b + a + b)$fixed.only, y ~ a*b)
    expect_equal(ARTool:::parse.art.formula(y ~ a:b + a + b)$fixed.terms, ~ a + b)
    expect_equal(ARTool:::parse.art.formula(y ~ a:b + a + b + (1|c))$fixed.only, y ~ a*b)
    expect_equal(ARTool:::parse.art.formula(y ~ a:b + a + b + (1|c))$fixed.terms, ~ a + b)
    expect_equal(ARTool:::parse.art.formula(y ~ a*b*c)$fixed.terms, ~ a + b + c)
    expect_equal(ARTool:::parse.art.formula(y ~ a*b*c*d)$fixed.terms, ~ a + b + c + d)
})

test_that("grouping terms and error terms are counted correctly", {
    expect_equal(ARTool:::parse.art.formula(y ~ a + (1|d))$n.grouping.terms, 1)
    expect_equal(ARTool:::parse.art.formula(y ~ a + (1|d) + (1|g))$n.grouping.terms, 2)
    expect_equal(ARTool:::parse.art.formula(y ~ a + Error(d))$n.error.terms, 1)
    expect_equal(ARTool:::parse.art.formula(y ~ a + Error(d) + Error(g))$n.error.terms, 2)
})

test_that("grouping terms and error terms are counted correctly", {
    expect_error(ARTool:::parse.art.formula(y ~ a + (1|d) + Error(g)), "Model cannot contain both grouping terms, like (1|d), and error terms, like Error(d). Use one or the other.", fixed=TRUE)
})

test_that("formulas with expressions for terms are parsed correctly", {
    expect_equal(ARTool:::parse.art.formula(y ~ factor(a) + b + Error(g) + factor(a):b)$fixed.only, y ~ factor(a) * b)
    expect_equal(ARTool:::parse.art.formula(y ~ factor(a) + b + Error(g) + factor(a):b)$fixed.terms, ~ factor(a) + b)
    expect_equal(ARTool:::parse.art.formula(y ~ factor(a) + b + Error(g) + factor(a):b)$fixed.term.labels, c("factor(a)", "b", "factor(a):b"))
})

test_that("formulas with expressions for responses are parsed correctly", {
    expect_equal(ARTool:::parse.art.formula(as.numeric(y) ~ a)$fixed.only, as.numeric(y) ~ a)
})

test_that("formulas with error terms are parsed correctly", {
    expect_equal(ARTool:::parse.art.formula(y ~ a)$error.terms, ~ NULL)
    expect_equal(ARTool:::parse.art.formula(y ~ a + Error(g))$error.terms, ~ g)
    expect_equal(ARTool:::parse.art.formula(y ~ a + Error(g*h))$error.terms, ~ g*h)
    expect_equal(ARTool:::parse.art.formula(y ~ a + Error(g*h) + Error(i))$error.terms, ~ g * h + i)
})

test_that("formulas generated from parsing have their environments set to that of the original formula", {
    f = y ~ a
    expect_equal(environment(ARTool:::parse.art.formula(f)$fixed.only), environment(f))
    expect_equal(environment(ARTool:::parse.art.formula(f)$fixed.terms), environment(f))
    expect_equal(environment(ARTool:::parse.art.formula(f)$error.terms), environment(f))
})
