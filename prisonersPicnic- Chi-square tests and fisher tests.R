###  prisonersPicnic- Chi-square tests and fisher tests

###  Is A.ate related to C.sick?

marginACdata = apply(prisonersPicnic.array.Observed, c(1,3), sum)

##  relative risk
riskIfAte = marginACdata["ate", "sick"]/sum(marginACdata["ate", ])
riskIfAteNot = marginACdata["ate not", "sick"]/sum(marginACdata["ate not", ])
relativeRisk = riskIfAte / riskIfAteNot
relativeRisk
optSave = options(digits=3); relativeRisk; 
options(optSave)

odds(riskIfAte)
odds(riskIfAteNot)
odds(riskIfAte) / odds(riskIfAteNot)   ### "odds ratio"
marginACdata[1,1]*marginACdata[2,2]/marginACdata[1,2]/marginACdata[2,1]### "odds ratio"

c(chisq.test(marginACdata)$p.value,
chisq.test(marginACdata, simulate.p.value=TRUE)$p.value)
fisher.test(marginACdata)   ### Yikes! odds ratio is different!
# estimate  an estimate of the odds ratio. 
#           Note that the conditional Maximum Likelihood Estimate (MLE) 
#           rather than the unconditional MLE (the sample odds ratio) is used. 
#           Only present in the 2 by 2 case.

###  What if they are all unrelated?
marginA = apply(prisonersPicnic.array, 1, sum)
marginB = apply(prisonersPicnic.array, 2, sum)
marginC = apply(prisonersPicnic.array, 3, sum) 
prisonersPicnic.dataframe$Expected =
  SAMPLE_SIZE * with(prisonersPicnic.dataframe,
                     marginA[A.ate] * marginB[B.drank] * marginC[C.sick]
                     )
sum(prisonersPicnic.dataframe$Expected)
prisonersPicnic.dataframe$ObsMinusExp = prisonersPicnic.dataframe$Observed - prisonersPicnic.dataframe$Expected
prisonersPicnic.dataframe$Residuals = prisonersPicnic.dataframe$ObsMinusExp/sqrt(prisonersPicnic.dataframe$Expected)
prisonersPicnic.dataframe
### These are the RESIDUALS. Examining them can help you find patterns.
### "It's the models that DON'T fit that teach you something."
