### Data:  from "02-More Probability.docx"
### Exercise: from "15-categorical data.docx"

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

### Spot-check (compare against original):
with(prisonersPicnic.dataframe, 
     proportion[A.ate=="ate" & B.drank=="drank" & C.sick=="ok"])

### This next is the same... but what a mess!
prisonersPicnic.dataframe$proportion[prisonersPicnic.dataframe$A.ate=="ate" & prisonersPicnic.dataframe$B.drank=="drank" & prisonersPicnic.dataframe$C.sick=="ok"]

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
### Now re-order, so that the cycling is most rapid for A, next for B, slowest for C
prisonersPicnic.dataframe = prisonersPicnic.dataframe[with(prisonersPicnic.dataframe, order(C.sick,B.drank,A.ate)), ]
### And reshape the array...
prisonersPicnic.array = aperm(prisonersPicnic.array, c("A.ate", "B.drank", "C.sick"))
### Spot-check again:
prisonersPicnic.array ["ate", "drank", "ok"]

###  Now we make an example data set:

prisonersPicnic.dataframe$Observed = rmultinom( 1, SAMPLE_SIZE, prisonersPicnic.dataframe$proportion)

prisonersPicnic.array
cbind(prisonersPicnic.array, prisonersPicnic.dataframe$proportion)
prisonersPicnic.array.Observed = prisonersPicnic.array
prisonersPicnic.array.Observed[] = prisonersPicnic.dataframe$Observed
### check:
cbind(prisonersPicnic.dataframe$Observed,  c(prisonersPicnic.array.Observed))
prisonersPicnic.array.Observed["ate not", "drank not", "sick"]  ##ok!!
prisonersPicnic.array.Observed["ate", "drank", "ok"]  ##ok!!
