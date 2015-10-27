

#See "The Matching Problem", page 43.
# Here is the basic formula for exercises 1.10.3 and 1.10.4

prAnyMatches = function(n) 
	-sum(1/factorial(1:n) * (-1)^(1:n))

#or, more elaborately:

#' @name pAnyMatches
#' @param 	n	The number of objects in each of two sets to be paired one-to-one.
#' @return	The probability that there is at least one match.
prAnyMatches = function(n, 
	test=FALSE, 
	plot=FALSE,
	method=c("successive", "factorial"))
{
	if(length(n) > 1) 
		return(sapply(n, prAnyMatches, method=method))
	if(test) return(c(prAnyMatches(100), 1-exp(-1)))
	if(n==1) return(1)
	if(plot)
		return(plot(1:n, 
			sapply(1:n, prAnyMatches, method=method),
				type="b"))

	switch(pmatch(method[1],	c("successive", "factorial")),
	{
		terms = rep(NA, n)
		terms[1] = 1
		for(k in 2:n) terms[k] = (-1)*terms[k-1]/k
		return(sum(terms))
	},
	return(-sum(1/factorial(1:n) * (-1)^(1:n)))
	)
}

# Application:

prAnyMatches(3)    #  [1] 0.6666667
1 - prAnyMatches(4)

# 8:   answer is zero.


# 10
Pr(i-th correct) * pr(all others incorrect | first is correct)  [DISJOINT]
n=3
n *  1/n * (1-prAnyMatches(n-1))
= 0.166667 * 3 = 1/2

