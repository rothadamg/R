#########chisquareForEqualP.simulation

####  This is a one-way table.
chisquareForEqualP.simulation = function(
	nCells=5,  							# the number of cells in the table.
	samplesize=100,						# the count total for the table.
	modelProbs=rep(1/nCells, nCells),	# the null hypothesis probabilities.
	RANDOMIZATION_TEST=FALSE,
	PRINTME=TRUE,
	TESTME=TRUE,
	...)
{
	simulatedData = rmultinom(1, samplesize, modelProbs)
	simulatedData = as.vector(simulatedData)
	expected = samplesize * modelProbs
	if(PRINTME) print(rbind(E=expected, O=simulatedData))
	chisquare.result = chisq.test(simulatedData, p=modelProbs,
				simulate.p.value= RANDOMIZATION_TEST, ...)
	return(chisquare.result)
}

chisquareForEqualP.simulation()
unclass(.())   ###  Remember, I define   . = function().Last.value

#################
####   From man page for chisq.test:
### Case A. Tabulated data
#x <- c(89,37,30,28,2)
#p <- c(0.40,0.20,0.20,0.19,0.01)
#                     # Expected count in category 5
#                     # is 1.86 < 5 ==> chi square approx.
#chisq.test(x, p = p)            #   maybe doubtful, but is ok!
#chisq.test(x, p = p,simulate.p.value = TRUE)
#
### Case B. Raw data
#x <- trunc(5 * runif(100))	# Tabulate it first!
#chisq.test(table(x))            # NOT 'chisq.test(x)'!
#
#

########   Hardy-Weinberg example.  ###############

###  For individual results:
hwCounts=read.delim(pipe("pbpaste"), header=FALSE, nrow=1)
dim(hwCounts)
hwCounts=as.matrix(hwCounts)
str(hwCounts)
t.hwCounts = table(hwCounts)

###  For count data :   See Project4Tables-jlw-2010-03-29-Table4W4.csv
t.hwCounts = read.delim(pipe("pbpaste"), header=FALSE, nrow=1)
t.hwCounts = unlist(t.hwCounts)

hwThetaHat = (t.hwCounts[1]+t.hwCounts[2]/2)/sum(t.hwCounts)

Ehat = c(hwThetaHat^2, 
		  2*hwThetaHat*(1-hwThetaHat), 
		  (1-hwThetaHat)^2)  * 
		sum(t.hwCounts)

rbind(t.hwCounts, Ehat)
Q = sum( (t.hwCounts-Ehat)^2/Ehat )
Qadjusted = sum(( abs(t.hwCounts-Ehat) - 1/2)^2/Ehat)
1 - pchisq(Q, df=1)
1 - pchisq(Qadjusted, df=1)

install.packages("HardyWeinberg")
require("HardyWeinberg")
HWChisq(as.vector(t.hwCounts))
HWExact(tdata)


