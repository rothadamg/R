#######   HealthySickTables.R

modelTable = rbind(c(.95,.03,.02), c(.03,.95,.02))

dimnames(modelTable) = list(
	c("healthy", "sick"),
	c("negative", "positive", "indeterminate"))

modelTable
rowSums(modelTable)
prevalence = 0.10

jointTable = modelTable * 
	matrix(c(1-prevalence, prevalence), nrow=2, ncol=3)
jointTable
sum(jointTable)
rowSums(jointTable)   #### Marginal Probability
colSums(jointTable)   #### Marginal Probability

marginalProbOfData = apply(jointTable, 2, sum)
marginalProbOfData
sum(marginalProbOfData)
marginalProbOfPatientStatus = apply(jointTable, 1, sum)
marginalProbOfPatientStatus
sum(marginalProbOfPatientStatus)

posteriorTable = jointTable /
	matrix(marginalProbOfData, nrow=2, ncol=3, byrow=T)
posteriorTable
jointTable/marginalProbOfData   ##### Conditional on the data
jointTable/marginalProbOfPatientStatus ##### Conditional on the true status.

getModelProbability <- function(
	stateOfNature, 
	event, 
	thisModelTable=modelTable,
	testMe=FALSE)
{
#' @name	getModelProbability
#' @param	stateOfNature	 A row index or row name to condition on.
#' @param	event A vector representing an event, a set of outcomes.
#' @param	thisModelTable The table holding the model family, one model per row.
#' @return The sum of probabilities of the outcomes, conditional on the state of nature.
  cat("\n")
   print(sys.call())
  if(testMe) {
			print(try(getModelProbability(1,2)))
			print(try(getModelProbability(1:2,2)))
			print(try(getModelProbability(2,1:3)))
			print(try(getModelProbability(2,1:4)))
			print(try(getModelProbability(2,numeric(0))))
			print(try(getModelProbability("healthy", "positive")))
    return(NULL)
	}
	if(length(stateOfNature) != 1) 
		stop("Model=conditional probability; exactly one stateOfNature, please.")
	return(sum(thisModelTable[stateOfNature, event]))
}
getModelProbability(testMe=TRUE)
  