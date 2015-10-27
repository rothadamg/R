### Data:  from "02-More Probability.docx"
### Exercise: from "14 -categorical data.docx"
#H1: Ate, Drank, and Sick are independent.
#H2: Ate and Drank are independent conditional on Sick.
#H3: Ate, Drank, and Sick may be pairwise associated, but there is no “3-way interaction”.

#  First, select the 8 data cells from the document, and "copy".

prisonersPicnic = scan(file="")
0.481   0.155   0.060 	0.168 
0.038 	0.010 	0.022 	0.066 

prisonersPicnic   

## Put into a data frame. 
### Looking across the rows,
## "sick" changes fastest, then "ate", then "drank".
prisonersPicnic.dataframe = 
	expand.grid( 
		C.sick=c("sick","ok"), A.ate=cq("ate","ate not"), 
    B.drank=cq("drank","drank not"))
prisonersPicnic.dataframe$proportion = prisonersPicnic
prisonersPicnic.dataframe$count = 1000 * prisonersPicnic

### Spot-check (compare against original):
with(prisonersPicnic.dataframe, 
     proportion[A.ate=="ate" & B.drank=="drank" & C.sick=="ok"])

####  Reshaping in the form of an array:
prisonersPicnic.array = 
  array(prisonersPicnic, 
        dim=c(2,2,2), 
        dimnames=list(
          C.sick=c("sick","ok"), A.ate=cq("ate","ate not"), 
          B.drank=cq("drank","drank not")) )
### Spot-check:
prisonersPicnic.array ["ok", "ate", "drank"]

## Now let's re-arrange in alphabetical order.
prisonersPicnic.dataframe = prisonersPicnic.dataframe[c(
  "A.ate", "B.drank", "C.sick", "proportion")]
  ### Spot-check again.
with(prisonersPicnic.dataframe, 
     proportion[A.ate=="ate" & B.drank=="drank" & C.sick=="ok"])
prisonersPicnic.array = aperm(prisonersPicnic.array, c("A.ate", "B.drank", "C.sick"))
### Spot-check again:
prisonersPicnic.array ["ate", "drank", "ok"]

###  Is A.ate related to C.sick?

marginAC = apply(prisonersPicnic.array, c(1,3), sum)
marginACdata = round(marginAC * 100)
chisq.test(marginACdata)
##  relative risk
riskIfAte = marginACdata["ate", "sick"]/sum(marginACdata["ate", ])
riskIfAteNot = marginACdata["ate not", "sick"]/sum(marginACdata["ate not", ])
relativeRisk = riskIfAte / riskIfAteNot
relativeRisk; options(digits=7); relativeRisk
odds(riskIfAte)
odds(riskIfAteNot)
odds(riskIfAte) / odds(riskIfAteNot)
fisher.test(marginACdata)

###  What if they are all unrelated?
marginA = apply(prisonersPicnic.array, 1, sum)
marginB = apply(prisonersPicnic.array, 2, sum)
marginC = apply(prisonersPicnic.array, 3, sum) 
prisonersPicnic.dataframe$expected =
  with(prisonersPicnic.dataframe,
       marginA[A.ate] * marginB[B.drank] * marginC[C.sick])
sum(prisonersPicnic.dataframe$expected)
options(digits=2)
prisonersPicnic.dataframe
prisonersPicnic.dataframe$ObsMinusExp = prisonersPicnic.dataframe$proportion - prisonersPicnic.dataframe$expected
### These are the RESIDUALS. Examining them can help you find patterns.
### "It's the models that DON'T fit that teach you something."
