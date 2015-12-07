# Internal function: Standardizsed ANOVA tables for lm, lmer, and aov objects
# with descriptions of the methods used and a single table for all results
# (instead of multiple tables as returned by aovlist objects --- hence "flat")
# 
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in global.variables.R
globalVariables(c("Df", "Df.res", "Sum Sq", "Sum Sq.res", "Term", "Error"))


flat.anova = function(m, ...) {
    UseMethod("flat.anova", m)
}

#' @importFrom stats anova
#' @importFrom car Anova
#' @importFrom magrittr %<>%
flat.anova.default = function(m, type="III", test="F", ...) {
    #get ANOVA table
    a = switch(type,
        I = anova(m, test=test, ...),
        II = Anova(m, type="II", test=test, ...),
        III = Anova(m, type="III", test=test, ...)[-1,]	#first row is intercept => ignore
    )

    #get the anova description from the heading
    description = strsplit(attr(a, "heading"),"\n")[[1]]
    if (type == "I") description %<>% paste("(Type I)") #Type I ANOVAs don't include the label themselves
    
    #add a column to keep track of the term name (rather than as row names because
    #in some instances this will not be unique, and row names must be unique)
    a = cbind(Term = rownames(a), a)

    attr(a, "description") = description
    a
}

#' @importFrom magrittr %<>%
flat.anova.lm = function(m, type="III", test="F", ...) {
    a = flat.anova.default(m, type, test, ...)
    description = attr(a, "description")
    
    #for lm (no grouping variables), the anova table will have an extra row with residual df; 
    #add the residual df from that row as a column and drop the unneeded row
    a %<>% columnify.anova.residuals()
 
    attr(a, "description") = description
    a
}

#' @importFrom magrittr %<>%
#' @import dplyr
columnify.anova.residuals = function (a.table) {
    #given a flat anova table with "Term", "Df", and "Sum Sq" columns
    #and the last row containing residual Df and Sum Sq, move the 
    #residual Df and Sum Sq into columns in the other rows
    
    k = nrow(a.table)
    df.res = a.table[k,"Df"]  
    sumsq.res = a.table[k,"Sum Sq"]
    a.table %<>% 
        .[-k,,drop=FALSE] %>% #drop last row (residuals)
        mutate(
            Df.res = df.res,
            `Sum Sq.res` = sumsq.res
        )
    
    #reorder columns
    cbind(
        select(a.table, Term, Df, Df.res, `Sum Sq`, `Sum Sq.res`),
        select(a.table, -Term, -Df, -Df.res, -`Sum Sq`, -`Sum Sq.res`)
    )
}

### Flat version of an anova from an aov model
#' @importFrom plyr ldply
#' @import dplyr
flat.anova.aovlist = function(m, 
        type="I", test="F", #type and test are ignored: they are always "I" and "F" for aov objects 
        ...
    ) {
    
    #construct flat anova table
    a = ldply(seq_along(m), function(i) {
        error = names(m)[[i]]
        ldply(summary(m[[i]]), function(anova.j) {
            if (nrow(anova.j) > 1) {
                #last row just has residual df and sum of squares, 
                #extract residual df to move into its own column
                anova.j %>%
                    mutate(
                        Term = gsub("\\s+$", "", rownames(.)),
                        Error = error
                    ) %>%
                    columnify.anova.residuals()
            }
        })
    })

    #reorder columns
    a = cbind(
        select(a, Term, Error),
        select(a, -Term, -Error)
    )

    attr(a, "description") = "Repeated Measures Analysis of Variance Table (Type I)"
    a
}

