0.8+c(-1,1)*sqrt(.8*.2/9)*qnorm(0.975)

### compare to exact:
binom.confint.new = 
function(k, n, alpha=0.05, range=0:1, 
		side=c("two", "upper", "lower")) {
	sideLetter = toupper(substring(side[1], 1, 1))
	if(sideLetter=="T") {
		alphaUpper = alphaLower = alpha/2
	} else if(sideLetter=="U") {
		alphaUpper = alpha; alphaLower = range[1]
	} else if(sideLetter=="L") {
		alphaLower = alpha; alphaUpper = range[2]
	}
	else stop("bad value for \"side\" ")  	
	huntForBoundary = function(p) 
  			alphaUpper - (1 - pbinom((k-1),n,p))
  	lb = try(uniroot(huntForBoundary, range)$root)
  	if(class(lb) == "try-error") lb = 0
	huntForBoundary = function(p) 
  			alphaLower - pbinom(k,n,p)
  	ub = try(uniroot(huntForBoundary, range)$root)
  	if(class(ub) == "try-error") ub = 1
	return(c(lb=lb, ub=ub))
}
binom.confint (k=8, n=10)
binom.confint.new (k=8, n=10)
binom.confint.new (k=8, n=10, side="u")
binom.confint.new (k=8, n=10, side="l")
