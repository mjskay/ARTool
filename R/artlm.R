# art function and some basic generic function implementations for art objects
# 
# Author: mjskay
###############################################################################


artlm = function(m, term, 
    response=c("art", "aligned"),
    factor.contrasts="contr.sum",
	...
) {
	#match enum arguments
    response = match.arg(response)
    
    #for the duration of this function, switch to the supplied contrast types
    original.contrasts = getOption("contrasts")
    tryCatch({
        options(contrasts=c(factor.contrasts, original.contrasts[-1]))
            
        #place the transformed (aligned or aligned and ranked) version of y
        #into the data frame as the dummy response ".y"
        df = m$data
        df$.y = switch(response, 
            aligned=m$aligned[[term]], 
            art=m$aligned.ranks[[term]])
        
        #modify formula to use dummy response name ".y"
        f = update(m$formula, .y ~ .)
        
        #reassign the environment of the model formula to this frame so that the data can be correctly recreated
        #by other functions (e.g. lsmeans:::recover.data) that use the function call and environment
        environment(f) = sys.frame(sys.nframe())
        
        #run linear model
        if (m$n.grouping.terms > 0) {       #grouping terms => REML
            m = lmer(f, data=df, ...)
        } else if (m$n.error.terms > 0) {   #error terms => repeated measures ANOVA
            m = aov(f, data=df, ...)
        } else {	                        #no grouping or error terms => OLS
            m = lm(f, data=df, ...)
        }
        
        attr(m, "term") = term
        attr(m, "response") = response
        m
    }, finally = {
        options(contrasts=original.contrasts)
    })
}
