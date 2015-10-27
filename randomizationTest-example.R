## randomizationTest-example

HairColorTable = matrix(c(3,2,5,90), nrow=2)
HairColorPchisq = chisq.test(HairColorTable)$p.value
###  note the warning about the test.
###  Rearrange for permutation:
HairColorDataFrame = data.frame(response=rep(c("R", "N"), times=c(8,92)))
HairColorDataFrame$hair=rep(c("D","L","D","L"), times=c(3,5,2,90))
with(HairColorDataFrame, table(response, hair))
HairColorPchisq = chisq.test(.Last.value)$p.value

nReps = 30000
options(warn=-1)  ### Turn off warnings

randomizationPvalues = sapply(1:nReps,
	function(ignoreMe) {
		dfTemp = data.frame(response=sample(HairColorDataFrame$response), hair=HairColorDataFrame$hair)
		tableTemp = with(dfTemp, table(response,hair))
		#print(tableTemp)
		pTemp = chisq.test(tableTemp)$p.
	}
)
table(randomizationPvalues)
mean(randomizationPvalues <= HairColorPchisq)

### Compare with what chisq.test provides via simulation.
HairColorPchisq.Sim = chisq.test(HairColorTable, simulate.p.value=T)$p.value


### "parametric bootstrap"

bootMyHairColor = function(ignoreMe) {
  #### Use the observed data frequencies to estimate the "truth".
  tableTemp = rmultinom(1, size=sum(HairColorTable), prob=HairColorTable/sum(HairColorTable))
  tableTemp = matrix(tableTemp, nrow=2)
  print(tableTemp)
  chisq.test(tableTemp)$p.
}

bootstrapPvalues = sapply(1:nReps, bootMyHairColor)
summary(bootstrapPvalues)
### This si the P value distribution under HA, not H0!
mean(bootstrapPvalues <= HairColorPchisq)

options(warn=1)   ### Reinstate warnings

