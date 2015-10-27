####   Example 9.3.1  from DeGroot-Schervish.
### In "15-categorical data.doc", select the data cells from the table for "voter preferences versus academic department"
### Then continue:

voterData = read.table(pipe("pbpaste"))
voterData
str(voterData)
## Sadly, it's a data frame.  For loglin, must be a matrix.
voterData = as.matrix(voterData)
voterData   #### OK
str(voterData)

# voterData[1,2] = 230   ### Later we'll change one number dramatically.

voterData.fitted = loglin(voterData, 1:2, fit=TRUE)
voterData.fitted$fit - voterData
(voterData.fitted$fit - voterData)^2/voterData.fitted$fit
(voterData.fitted$fit - voterData)/sqrt(voterData.fitted$fit)

Q = sum((voterData.fitted$fit - voterData)^2/voterData.fitted$fit)
c(Q=Q, pearson=voterData.fitted$pearson, lrt=voterData.fitted$lrt)

df.for.Q = (nrow(voterData)-1) * (ncol(voterData)-1)
c(voterData.fitted$df, df.for.Q)
cat("P = ", 1 - pchisq(Q, df.for.Q), "\n")

require("MASS")   ### "MASS" is a library in support of the book "Modern Applied Statistics with S-Plus", Venables and Ripley.
loglm( ~ 1 + 2, voterData)    #   or,  loglin(voterData, 1:2, fit=TRUE)

###  Reconstructing the likelihood ratio statistic:

maxLik1 = prod(voterData^voterData)   # SATURATED
log(maxLik1)  #### oops.
logmaxLik1 = sum(voterData*log(voterData))  ### better
logmaxLik0 = sum(voterData*log(voterData.fitted$fit))
2*(logmaxLik1 - logmaxLik0)
voterData.fitted$lrt   #### They agree, of course.

####  What if you want to test whether the departments are different?  
####  If not, then all the rows are IDENTICAL, not just proportional.

loglin( voterData, 1)
1 - pchisq(.Last.value$pearson, 8)
