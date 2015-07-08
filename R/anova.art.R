# ANOVAs for art objects
# 
# Author: mjskay
###############################################################################


anova.art = function(object, 
    response=c("art", "aligned"),
    type=c("III", "II", "I", 3, 2, 1), 
    factor.contrasts="contr.sum",
    test=c("F", "Chisq"),  
    all.rows=FALSE,
    ...
) {
    #sensible names for generic parameters
    m = object
    
    #match enum arguments
    response = match.arg(response)
    type = as.character(type)
    type = match.arg(type)
    test = match.arg(test)
    
    #get transformed responses based on response type requested
    responses = switch(response, 
        aligned=m$aligned, 
        art=m$aligned.ranks)
    
    #determine anova type to use
    type = if (type %in% c(1,"I")) "I"
    else if (type %in% c(2,"II")) "II"
    else if (type %in% c(3,"III")) "III"
        
    #are we going to need to show the term we aligned by
    #for each row of the output?
    show.aligned.by.term = response == "aligned" || all.rows
    
    #run linear models and anovas
    df = m$data
    anovas = NULL
    table.description = "" 
    for (j in 1:ncol(responses)) {	#can't use ldply here because it appears to drop row names when binding rows
        aligned.by.term = colnames(responses)[[j]]
        
        #get linear model
        m.l = artlm(m, aligned.by.term, response=response, factor.contrasts=factor.contrasts)
        
        #run anova and extract desired results 
        anova.j = flat.anova(m.l, type=type, test=test, ...)
        if (j == 1) table.description = attr(anova.j, "description")
        
        #extract desired result rows from anova
        #for art, this is the one row correponding to the effect we aligned and ranked by
        #for aligned, this is every effect *except* the one we aligned and ranked by 
        if (!all.rows) {
            include.row = anova.j$Term == aligned.by.term
            if (response == "aligned") include.row = !include.row
            anova.j = anova.j[include.row,]
        }
        
        #Add "Aligned By" column when needed to disambiguate
        if (nrow(anova.j) > 0) {	
            #if only one fixed effect we can get no rows here, e.g. if response="aligned" 
            #and all.rows=FALSE, so the above guard is necessary
            if (show.aligned.by.term) {
                anova.j = cbind(anova.j[,1,drop=FALSE], `Aligned By`=aligned.by.term, anova.j[,-1,drop=FALSE])
            }
        }
        
        anovas = rbind(anovas, anova.j)
    }
    
    #fill in the rest of the anova table metadata and return
    class(anovas) = c("anova.art", "anova", "data.frame")
    attr(anovas, "model") = 
        if (m$n.grouping.terms > 0) "lmer" 
        else if (m$n.error.terms > 0) "aov" 
        else "lm"
    attr(anovas, "table.description") = table.description
    attr(anovas, "response") = response
    attr(anovas, "response.term") = colnames(m$cell.means)[1]
    anovas	
}

### Generate p stars for a vector of p values
p.stars = function(p.values) {
    unclass(symnum(p.values, corr = FALSE, na = FALSE, cutpoints = c(0, 
                            0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", 
                            "*", ".", " ")))
}

print.anova.art = function(x, verbose=FALSE, digits=5, ...) {
    #print heading and metadata
    cat("Analysis of Variance of Aligned Rank Transformed Data\n\n")
    cat("Table Type:", attr(x, "table.description"), "\n")
    cat("Model:", switch(attr(x, "model"),
        lm = "No Repeated Measures (lm)\n",
        aov = "Repeated Measures (aov)\n",
        lmer = "Mixed Effects (lmer)\n",
    ))
    cat(sep="", "Response: ", attr(x, "response"), "(", attr(x, "response.term"), ")\n\n")
    
    #format p values
    p.col = last(which(grepl("^(P|Pr)\\(", names(x))))
    stars.legend = if (!is.na(p.col)) {
        #add column for p stars
        stars = p.stars(x[[p.col]])
        x = cbind(x, ` ` = stars)
        #reformat p values for printing
        x[[p.col]] %<>% format.pval()
        #return stars legend
        attr(stars, "legend")
    } else NULL

    #generate row names from Terms
    rownames(x) = paste(1:nrow(x), x$Term)
    x$Term = NULL

    #abbreviate columns
    if (!is.null(x$Error)) x$Error %<>% abbreviate(5)
    if (!is.null(x$`Aligned By`)) x$`Aligned By` %<>% abbreviate(10)
    
    #drop "Sum Sq" (etc) columns when not doing verbose output 
    if (!verbose) x %<>% select(everything(), -contains("Sum Sq"), -contains("Mean Sq"))
    
    #print table
    print.data.frame(x, digits=digits, ...)
    
    #print legend
    if (!is.null(stars.legend)) {
        cat("---\nSignif. codes:  ", stars.legend, "\n")
    }
}
