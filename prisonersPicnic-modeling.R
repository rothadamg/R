require(MASS)  #### This contains loglm()
.. = function() .Last.value  # for convenience

###  An independence model
( output.loglm = with(prisonersPicnic.dataframe,
                      loglm( Observed ~ A.ate + B.drank + C.sick , fit=TRUE)) )
anova(output.loglm)    ### no equivalent for loglin
summary(output.loglm)  ### more detail.

###  In these formulas, the "+" and the ":" do not mean the usual thing.  Run the command 
#              help("formula")
model1  = loglm(formula=  ~ 1 + 2 + 3,
                data=prisonersPicnic.array.Observed,
                fit=TRUE)
resid(..())
modelABCindependent  = loglm(formula=  ~ A.ate + B.drank + C.sick,
                data=prisonersPicnic.array.Observed,
                fit=TRUE)  ####  the same.
resid(..())

### A "interacts" with C
modelAwithC  = loglm(formula=  ~ A.ate*C.sick + B.drank,
                             data=prisonersPicnic.array.Observed,
                             fit=TRUE)  ####  the same.
### B "interacts" with C
modelBwithC  = loglm(formula=  ~ A.ate + C.sick*B.drank,
                     data=prisonersPicnic.array.Observed,
                     fit=TRUE)  
anova(modelABCindependent, modelAwithC)
resid(modelAwithC)  ## Wow, A really explains a lot

anova(modelABCindependent, modelBwithC)
resid(modelBwithC)  ##  B does not explain C;   deviance does not get much better

## A and B both interact with C
modelAandBwithC  = loglm(formula=  ~ C.sick*A.ate + C.sick*B.drank,
                     data=prisonersPicnic.array.Observed,
                     fit=TRUE)  ####  the same.
anova(modelABCindependent, modelAwithC, modelAandBwithC)
 ### No,  A.ate explains it all.

  

model1  = loglm(formula=  ~ 1 + 2 + 3,
                data=prisonersPicnic.array.Observed,
                fit=TRUE)
model2  = loglm(formula=  ~ 1:2 + 2:3,
                data=prisonersPicnic.array.Observed,
                fit=TRUE)  	###  The same! 
model2  = loglm(formula=  ~ 1*2 + 2*3,
                data=prisonersPicnic.array.Observed,
                fit=TRUE)   ###  The same!
model3  = loglm(formula=  ~ 1:3 + 1:2 + 2:3,
                data=prisonersPicnic.array.Observed,
                fit=TRUE) 
model4  = loglm(formula=  ~ 1:2:3,
                data=prisonersPicnic.array.Observed,
                fit=TRUE) 	###  Saturated model
resid(..())

#####
anova(model1, model2, model3, model4)
for (model in list(model1, model2, model3, model4)) {
  cat("======= ", as.character(model$formula)[2], " =======\n")
#  print(summary(model))
  print(coefficients(model))
}
options(digits=3)
for (model in list(model1, model2, model3, model4)) {
  cat("======= ", as.character(model$formula)[2], " =======\n")
  #  print(summary(model))
  print((c(residuals(model))))
}

##########################
## Other ways to do it. These are more flexible
( output.glm = with(prisonersPicnic.dataframe,
     glm( Observed ~ B.drank + C.sick + A.ate, family=poisson)) )

( output.glm.2 = with(prisonersPicnic.dataframe,
     glm(C.sick ~  A.ate + B.drank , family=binomial, 
        weights=Observed) ))

( output.loglin = loglin(prisonersPicnic.array.Observed, 
                         margin=list(1,2,3), fit=TRUE) )

### Compare fitted values
exp(predict(output.glm))
output.loglin$fit

### Compare fit statistics
c(output.glm$deviance, output.glm.2$deviance, model1$lrt, output.loglin$lrt)

anova(output.glm)    ### no equivalent for loglin
output.loglin.2 = loglin(prisonersPicnic.array.Observed, margin=list(1:2,3:2), fit=TRUE)
names(output.loglin.2 )
output.loglin$lrt
output.loglin.2$lrt
output.loglin.2$lrt - output.loglin$lrt  
  ### this compares 2 models; in the second, the deviance (loglikelihood*2) is reduced by 4.63, 
 ####     at the expense of an extra parameter.  Is it worth it?
