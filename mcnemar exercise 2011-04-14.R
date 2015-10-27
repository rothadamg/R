### mcnemar exercise 2011-04-14

TRUTH:   disease present
Test B is positive
Test B is negative
Test A is positive
100
17
Test A is negative
8
15

TRUTH:   disease absent
Test B is positive
Test B is negative
Test A is positive
14
16
Test A is negative
22
200


mcdata.orig = read.delim(pipe("pbpaste"))
mcdata.orig
mcdata.n = as.numeric(unlist(mcdata.orig[-3,-1]))
(mcdata.array = array(mcdata.n, dim=c(2,2,2)))
dimnames(mcdata.array) = list(
	A=cq(Apos,Aneg),
	TRUTH=cq(present,absent),
	B=cq(Bpos,Bneg)
	)	
mcdata.array = aperm(mcdata.array, c(1,3,2))
######
mcdata.df = expand.grid(dimnames(mcdata.array))
mcdata.df$count = c(mcdata.array)
######  Check it!  OK.

######  Hypothesis:  A is more likely to say "positive" than B.
###### One approach: Just collapse over TRUTH:
(AposBneg = mcdata.array["Apos", "Bneg", "present"] +
			mcdata.array["Apos", "Bneg", "absent"])
(AposBneg = sum(
	mcdata.df [mcdata.df$A=="Apos" & 
				 mcdata.df$B=="Bneg", "count"]))
(AposBneg = sum(
	(mcdata.df %where% (A=="Apos" & B=="Bneg"))
	$count))   ####  You need all the parentheses when using mvbutils::`%where%` 
## 33
(AnegBpos = sum(
	(mcdata.df %where% (A=="Aneg" & B=="Bpos"))
	$count) )  ## 30
catn("P = " %&% round(digits=3,
		1 - pbinom(33-1, 33+30, 1/2)))
		
## Compare with mcnemar.test-- TWOSIDED
mcnemar.test(apply(mcdata.array, 1:2, sum) )  ## P=0.801 chisq.
## This is very limited, though.

##### Another approach:  control for TRUTH.
catn("(present) P = " %&% round(digits=3,
		Ppresent <- 1 - pbinom(17-1, 17+8, 1/2)))
catn("(absent) P = " %&% round(digits=3,
		Pabsent <- 1 - pbinom(16-1, 16+22, 1/2)))
### Combine Pvalues using Fisher's method:
catn("(combined) P = " %&% round(digits=3,
	Pcombined <- 1-pchisq(df=2*2, 
						-2*log(Ppresent*Pabsent))))


######  Hypothesis:  A is more likely to be right.
mcdata.df$Acorrect = 
 (mcdata.df$A=="Apos" & mcdata.df$TRUTH=="present") |
 (mcdata.df$A=="Aneg" & mcdata.df$TRUTH=="absent") 
mcdata.df$Bcorrect = 
 (mcdata.df$B=="Bpos" & mcdata.df$TRUTH=="present") |
 (mcdata.df$B=="Bneg" & mcdata.df$TRUTH=="absent") 

###  Collapsing over TRUTH:
AcorrectBincorrect = mcdata.df$Acorrect & 
						! mcdata.df$Bcorrect
nAcorrectBincorrect = sum(mcdata.df$count[AcorrectBincorrect])
ABdisagree = mcdata.df$Acorrect != mcdata.df$Bcorrect
nABdisagree = sum(mcdata.df$count[ABdisagree])
catn("(collapsing) P = ",
		1 - pbinom(nAcorrectBincorrect - 1, nABdisagree, 1/2))
#### Controlling for TRUTH
###  If most cases are "present", then A might be more right just because A says "positive" more often.

mcdata2 = rbind(mcdata.df, mcdata.df)
mcdata2$test = rep(cq(A,B), each=8)
(mcdata2$correct =  ifelse(mcdata2$test=="A", 
					mcdata2$Acorrect, mcdata2$Bcorrect))
mcdata2 = mcdata2[cq(TRUTH,count,test,correct)]
require("MASS")
glm(data=mcdata2,  correct ~ TRUTH + test, weights=count, family=poisson)	
summary(..())