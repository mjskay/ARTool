# Tests for anova.art
# 
# Author: mjskay
###############################################################################

library(testthat)
library(ARTool)

context("anova.art")


comparable.anova = function(m, include.error=FALSE) {
    #get an anova from a model in a form that is easy to compare 
    #(known column subset and rounded numeric results)
    a = as.data.frame(anova(m))
    #extract character cols and convert to factors for consistency in comparison
    character_cols = lapply(a[,c("Term", if (include.error) "Error"), drop=FALSE], factor)
    #some anovas use "F value" instead of just "F", standardize
    if (!is.null(a$`F value`)) a$F = a$`F value`
    #extract numeric cols and round for consistency
    numeric_cols = round(a[,c("Df","Df.res","F","Pr(>F)")], digits=4)
    #stick it all back together and standardize the rownames
    a = cbind(character_cols, numeric_cols)
    rownames(a) = 1:nrow(a)
    a
}

test_that("anova.art of Higgins1990Table5 matches results of the original ARTool", {
    data(Higgins1990Table5, package="ARTool")
    
    #reference result
    ref = data.frame(
        Term = c("Moisture", "Fertilizer", "Moisture:Fertilizer"), 
        Error = c("Tray", "Within", "Within"), 
        Df = c(3, 3, 9), 
        Df.res = c(8, 24, 24), 
        "F" = c(23.8326,  122.4017, 5.1180), 
        "Pr(>F)" = c(0.0002, 0, 0.0006),
        check.names = FALSE
    ) 

    #run art using repeated measures anova
    m = art(DryMatter ~ Moisture*Fertilizer + Error(Tray), data=Higgins1990Table5)
    a = comparable.anova(m, include.error=TRUE)
    expect_equal(a, ref)

    #run art using mixed effects model
    m = art(DryMatter ~ Moisture*Fertilizer + (1|Tray), data=Higgins1990Table5)
    a = comparable.anova(m)
    expect_equal(a, ref[-2]) #no Error col
})

test_that("anova.art of Higgins1990Table1 matches results of the original ARTool", {
    ### verify that art on Higgins1990Table5 is correct
    data(Higgins1990Table1, package="ARTool")
    
    #reference result
    ref = data.frame(
        Term = c("Row", "Column", "Row:Column"),
        Df = c(2,  2, 4), 
        Df.res = c(27, 27, 27), 
        "F" = c(29.9925, 77.8668, 0.6417), 
        "Pr(>F)" = c(0, 0, 0.6374),
        check.names = FALSE
    )

    #run art using linear model
    m = art(Response ~ Row*Column, data=Higgins1990Table1)
    a = comparable.anova(m)
    expect_equal(a, ref)        
})

#test_that("art of HigginsABC matches results of the original ARTool", {
#            ### verify that art on HigginsABC is correct
#            data(HigginsABC, HigginsABC.art, package="ARTool")
#            
#            #run art on original data
#            m = art(Y ~ A*B*C + (1|Subject), data=HigginsABC)
#            
#            #verify column sums on aligned columns and F scores on aligned columns not of interest are all 0
#            expect_equal(colSums(m$aligned), rep(0, ncol(m$aligned)), check.names=FALSE)
#            aligned.anova = anova(m, response="aligned")
#            expect_equal(aligned.anova$F, rep(0, nrow(aligned.anova)), check.names=FALSE)
#            
#            #verify that aligned responses were all calculated correctly
#            expect_equal(m$aligned$A, HigginsABC.art$aligned.Y..for.A)
#            expect_equal(m$aligned$B, HigginsABC.art$aligned.Y..for.B)
#            expect_equal(m$aligned$C, HigginsABC.art$aligned.Y..for.C)
#            expect_equal(m$aligned$`A:B`, HigginsABC.art$aligned.Y..for.A.B)
#            expect_equal(m$aligned$`A:C`, HigginsABC.art$aligned.Y..for.A.C)
#            expect_equal(m$aligned$`B:C`, HigginsABC.art$aligned.Y..for.B.C)
#            expect_equal(m$aligned$`A:B:C`, HigginsABC.art$aligned.Y..for.A.B.C)
#            
#            #verify that ART responses were all calculated correctly
#            expect_equal(m$aligned.ranks$A, HigginsABC.art$ART.Y..for.A)
#            expect_equal(m$aligned.ranks$B, HigginsABC.art$ART.Y..for.B)
#            expect_equal(m$aligned.ranks$C, HigginsABC.art$ART.Y..for.C)
#            expect_equal(m$aligned.ranks$`A:B`, HigginsABC.art$ART.Y..for.A.B)
#            expect_equal(m$aligned.ranks$`A:C`, HigginsABC.art$ART.Y..for.A.C)
#            expect_equal(m$aligned.ranks$`B:C`, HigginsABC.art$ART.Y..for.B.C)
#            expect_equal(m$aligned.ranks$`A:B:C`, HigginsABC.art$ART.Y..for.A.B.C)	
#    })
