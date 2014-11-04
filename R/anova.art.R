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
    
    #determine anova function to use
    anova.function = if (type %in% c(1,"I")) {
        type = "I"
        function(m) anova(m, test=test, ...)
    } 
    else if (type %in% c(2,"II")) {
        type = "II"
        function(m) Anova(m, type="II", test=test, ...)
    } 
    else if (type %in% c(3,"III")) {
        type = "III"
        function(m) Anova(m, type="III", test=test, ...)[-1,]	#first row is intercept => ignore
    } 
    else {
        stop('type must be one of "I", "II", "III", 1, 2, 3. (got ', type, ')')
    }
    
    #get transformed responses based on response type requested
    responses = switch(response, 
        aligned=m$aligned, 
        art=m$aligned.ranks)
    
    #are we going to need to show the number of the factor we aligned by
    #for each row of the output?
    show.factor.numbers = response == "aligned" || all.rows
    
    #run linear models and anovas
    df = m$data
    anovas = NULL
    for (j in 1:ncol(responses)) {	#can't use ldply here because it appears to drop row names when binding rows
        #get linear model
        m.l = artlm(m, colnames(responses)[[j]], response=response, factor.contrasts=factor.contrasts)
        
        #run anova and extract desired results 
        anova.j = anova.function(m.l)
        
        #for lm (no grouping variables), anova table will have an extra row with residual df; 
        #add the residual df from that row as a column and drop the unneeded row
        if (m$n.grouping.variables == 0) {
            anova.j = cbind(anova.j[1:2], Df.res=anova.j$Df[nrow(anova.j)], anova.j[3:ncol(anova.j)])[-nrow(anova.j),]
        }
        
        #extract desired result rows from anova
        #for art, this is the one row correponding to the effect we aligned and ranked by
        #for aligned, this is every effect *except* the one we aligned and ranked by 
        if (!all.rows) {
            row.index.j = switch(response, aligned=-j, art=j)
            anova.j = anova.j[row.index.j,]
        }
        
        #prepend aligned factor number to row labels when needed to disambiguate
        if (nrow(anova.j) > 0) {	
            #if only one fixed effect we can get no rows here, e.g. if response="aligned" 
            #and all.rows=FALSE, so the above guard is necessary
            if (show.factor.numbers) rownames(anova.j) = paste(j, rownames(anova.j))
        }
        
        anovas = rbind(anovas, anova.j)
    }
    
    #fill in the rest of the anova table metadata and return
    class(anovas) = c("anova", "data.frame")
    attr(anovas, "heading") = c(
        paste("Aligned Rank Transform Anova Table (Type", type, "tests)\n"),
        if (show.factor.numbers) { c("Responses aligned by:", paste("\t", 1:ncol(responses), "=", colnames(responses)), "") },
        paste("Response: ", response, "(", colnames(m$cell.means)[1], ")", sep="")
    )
    anovas	
}
