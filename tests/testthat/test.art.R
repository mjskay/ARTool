# Tests for art
# 
# Author: mjskay
###############################################################################

library(testthat)
library(ARTool)


test_that("art does not allow factors as responses", {
    expect_error(art(y ~ a, data=data.frame(y=factor(c(1,2,3)), a=factor(c(1,2,3)))), 'Reponse term must be numeric, ordered factor, or logical \\(it was factor\\)')
})

test_that("art allows numeric responses", {
    m = art(y ~ a, data=data.frame(y=c(rep(1, 10), rep(2, 10)), a=factor(rep(1:2, 10))))
    
    expect_equal(anova(m)$F, 0) 
    expect_equal(nrow(anova(m, response="aligned")), 0)
})

test_that("art allows logical responses", {
    m = art(y ~ a, data=data.frame(y=c(rep(TRUE, 10), rep(FALSE, 10)), a=factor(rep(1:2, 10))))
    
    expect_equal(anova(m)$F, 0) 
    expect_equal(nrow(anova(m, response="aligned")), 0)
})

test_that("art allows ordinal responses", {
    m = art(y ~ a, data=data.frame(y=ordered(c(rep(1, 10), rep(2, 10))), a=factor(rep(1:2, 10))))
    
    expect_equal(anova(m)$F, 0) 
    expect_equal(nrow(anova(m, response="aligned")), 0)
})

test_that("art does not allow fixed effects that are numeric", {
    expect_error(art(y ~ a, data=data.frame(y=c(1,2,3), a=c(1,2,3))), 'All fixed effect terms must be factors or logical \\(e.g. not numeric\\).')
})

test_that("art allows logical fixed effect terms", {
    m = art(y ~ a, data=data.frame(y=c(rep(1, 10), rep(2, 10)), a=rep(c(FALSE,TRUE), 10)))
    
    expect_equal(anova(m)$F, 0) 
    expect_equal(nrow(anova(m, response="aligned")), 0)
})

test_that("art does not allow incomplete cases in fixed effects", {
    expect_error(art(y ~ a, data=data.frame(y=c(NA,2,3), a=factor(c(1,2,3)))), "Aligned Rank Transform cannot be performed when fixed effects have missing data \\(NAs\\).")
    expect_error(art(y ~ a, data=data.frame(y=c(1,2,3), a=factor(c(1,NA,3)))), "Aligned Rank Transform cannot be performed when fixed effects have missing data \\(NAs\\).")
})

test_that("art allows models with only one fixed effect", {
    df = data.frame(y=1:20, a=factor(rep(c(1,2),10)))
    m = art(y ~ a, data=df)
    
    #neither of the following anovas should throw an error
    expect_equal(anova(m)$F, 0.13636363636363636) 
    expect_equal(nrow(anova(m, response="aligned")), 0)	#only one fixed effect => aligned anova table has no rows
})

test_that("art allows models with missing data in the grouping terms", {
    df = data.frame(y=1:20, a=factor(rep(c(1,2),10)), b=factor(rep(c(1,2,3,NA,5),4)))
    m = art(y ~ a + (a|b), data=df)
    
    #neither of the following anovas should throw an error
    expect_equal(anova(m)$F, 0.6862745)
    expect_equal(nrow(anova(m, response="aligned")), 0) 
})

test_that("art of Higgins1990Table5 matches results of the original ARTool", {
    ### verify that art on Higgins1990Table5 is correct
    data(Higgins1990Table5, Higgins1990Table5.art, package="ARTool")
    
    #run art on original data
    m = art(DryMatter ~ Moisture*Fertilizer + (1|Tray), data=Higgins1990Table5)
    
    #verify column sums on aligned columns and F scores on aligned columns not of interest are all 0
    expect_equal(colSums(m$aligned), rep(0, ncol(m$aligned)), check.names=FALSE)
    aligned.anova = anova(m, response="aligned")
    expect_equal(aligned.anova$F, rep(0, nrow(aligned.anova)), check.names=FALSE)
    
    #verify that aligned responses were all calculated correctly
    expect_equal(m$aligned$Moisture, Higgins1990Table5.art$aligned.DryMatter..for.Moisture)
    expect_equal(m$aligned$Fertilizer, Higgins1990Table5.art$aligned.DryMatter..for.Fertilizer)
    expect_equal(m$aligned$`Moisture:Fertilizer`, Higgins1990Table5.art$aligned.DryMatter..for.Moisture.Fertilizer)
    
    #verify that ART responses were all calculated correctly
    expect_equal(m$aligned.ranks$Moisture, Higgins1990Table5.art$ART.DryMatter..for.Moisture)
    expect_equal(m$aligned.ranks$Fertilizer, Higgins1990Table5.art$ART.DryMatter..for.Fertilizer)
    expect_equal(m$aligned.ranks$`Moisture:Fertilizer`, Higgins1990Table5.art$ART.DryMatter..for.Moisture.Fertilizer)
})

test_that("art of Higgins1990Table1 matches results of the original ARTool", {
    ### verify that art on Higgins1990Table5 is correct
    data(Higgins1990Table1, Higgins1990Table1.art, package="ARTool")
    
    #run art on original data
    m = art(Response ~ Row*Column, data=Higgins1990Table1)
    
    #verify column sums on aligned columns and F scores on aligned columns not of interest are all 0
    expect_equal(colSums(m$aligned), rep(0, ncol(m$aligned)), check.names=FALSE)
    aligned.anova = anova(m, response="aligned")
    expect_equal(aligned.anova$F, rep(0, nrow(aligned.anova)), check.names=FALSE)
    
    #verify that aligned responses were all calculated correctly
    expect_equal(m$aligned$Row, Higgins1990Table1.art$aligned.Response..for.Row)
    expect_equal(m$aligned$Column, Higgins1990Table1.art$aligned.Response..for.Column)
    expect_equal(m$aligned$`Row:Column`, Higgins1990Table1.art$aligned.Response..for.Row.Column)
    
    #verify that ART responses were all calculated correctly
    expect_equal(m$aligned.ranks$Row, Higgins1990Table1.art$ART.Response..for.Row)
    expect_equal(m$aligned.ranks$Column, Higgins1990Table1.art$ART.Response..for.Column)
    expect_equal(m$aligned.ranks$`Row:Column`, Higgins1990Table1.art$ART.Response..for.Row.Column)
})

test_that("art of HigginsABC matches results of the original ARTool", {
    ### verify that art on HigginsABC is correct
    data(HigginsABC, HigginsABC.art, package="ARTool")
    
    #run art on original data
    m = art(Y ~ A*B*C + (1|Subject), data=HigginsABC)
    
    #verify column sums on aligned columns and F scores on aligned columns not of interest are all 0
    expect_equal(colSums(m$aligned), rep(0, ncol(m$aligned)), check.names=FALSE)
    aligned.anova = anova(m, response="aligned")
    expect_equal(aligned.anova$F, rep(0, nrow(aligned.anova)), check.names=FALSE)
    
    #verify that aligned responses were all calculated correctly
    expect_equal(m$aligned$A, HigginsABC.art$aligned.Y..for.A)
    expect_equal(m$aligned$B, HigginsABC.art$aligned.Y..for.B)
    expect_equal(m$aligned$C, HigginsABC.art$aligned.Y..for.C)
    expect_equal(m$aligned$`A:B`, HigginsABC.art$aligned.Y..for.A.B)
    expect_equal(m$aligned$`A:C`, HigginsABC.art$aligned.Y..for.A.C)
    expect_equal(m$aligned$`B:C`, HigginsABC.art$aligned.Y..for.B.C)
    expect_equal(m$aligned$`A:B:C`, HigginsABC.art$aligned.Y..for.A.B.C)
    
    #verify that ART responses were all calculated correctly
    expect_equal(m$aligned.ranks$A, HigginsABC.art$ART.Y..for.A)
    expect_equal(m$aligned.ranks$B, HigginsABC.art$ART.Y..for.B)
    expect_equal(m$aligned.ranks$C, HigginsABC.art$ART.Y..for.C)
    expect_equal(m$aligned.ranks$`A:B`, HigginsABC.art$ART.Y..for.A.B)
    expect_equal(m$aligned.ranks$`A:C`, HigginsABC.art$ART.Y..for.A.C)
    expect_equal(m$aligned.ranks$`B:C`, HigginsABC.art$ART.Y..for.B.C)
    expect_equal(m$aligned.ranks$`A:B:C`, HigginsABC.art$ART.Y..for.A.B.C)	
})
