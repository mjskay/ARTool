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
            
        #modify formula to use dummy response name ".y"
        f = update(m$formula, .y ~ .)
        
        #place the transformed (aligned or aligned and ranked) version of y
        #into the data frame as the dummy response ".y"
        df = m$data
        df$.y = switch(response, 
            aligned=m$aligned[[term]], 
            art=m$aligned.ranks[[term]])
        
        #run linear model
        env = sys.frame(sys.nframe())
        if (m$n.grouping.variables == 0) {	#no grouping terms => OLS
            m = lm(f, data=df, ...)
            #reassign the environment of the model to this frame so that the data can be correctly recreated
            #by other functions (e.g. lsmeans:::recover.data) that use the function call and environment
            environment(m$terms) = env
            environment(attr(m$model, "terms")) = env
        }
        else {								#grouping terms => REML
            m = lmer(f, data=df, ...)
            #reassign the environment of the model to this frame so that the data can be correctly recreated
            #by other functions (e.g. lsmeans:::recover.data) that use the function call and environment
            environment(attr(m@frame, "terms")) = env
            environment(attr(m@frame, "formula")) = env
        }
        
        attr(m, "term") = term
        attr(m, "response") = response
        m
    }, finally = {
        options(contrasts=original.contrasts)
    })
}
