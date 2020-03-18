
#' TODO: Describe in one short sentence what this function is good for
#' 
#' @param srvData a data.frame; it contains the original survey 
#'   (individual-level) data. (E.g. ESS data from https://www.europeansocialsurvey.org/)
#' @param cmpVar character string or \code{NULL}. If a character string, it must be the 
#'   name of the column in \code{srvData} which refers 
#'   to the categories (e.g. countries) which is the basis for the comparison in the 
#'   gegini/PiPi index. If \code{NULL} (the default) no comparison is performed.
#' @param ineqVar character string denoting the name of the variable in \code{srvData} 
#'   for which you want to determine the inequality. The variable must contain two valid 
#'   values of which the higher value will be considered the reference (e.g.: 0 means 
#'   did not vote and 1 means voted. In this scenario the PiPi will compute the inequality 
#'   of willingness to vote.)
#' @param groupingVars character vector denoting the grouping variables in\code{srvData}. 
#'   If there are multiple grouping variables, a new variable is created consisting of 
#'   all possible combinations of the levels of those variables. 
#' @param weight character string or \code{NULL}; the name of the survey weight variable in \code{srvData}, 
#'   if there is any. 
#' @param naomit logical flag; if \code{TRUE} (the default), and there is any missing value in any of the 
#'   relevant variables, the given record is omitted from the calculations. Otherwise, missing values 
#'   are treated as valid values.
#' @return A list that contains as many \code{geginiData} objects as there are unique categories in 
#'   the "cmpVar" vector.
#' @export 
#' @example examples/geginiData_ex.R
#' 
geginiData <- function(srvData=NULL, cmpVar=NULL, ineqVar=NULL, groupingVars=NULL, weight=NULL, naomit=TRUE) {
	 if (is.null(ineqVar)) {
		  stop("'ineqVar' must contain at least one valid variable name!") }
	 if (length(ineqVar)>1) {
		  warning("ineqVar should contain only one variable name: only the first element was considered in the analysis", call. = FALSE)
		  ineqVar <- ineqVar[1] }		
	 if (!any(colnames(srvData) %in% ineqVar)) {
		  stop("'ineqVar' must contain a valid variable name!") }
	 srvData$ineqVar <- srvData[,ineqVar]
	 if (is.null(weight)) {
		  srvData$weight <- rep(1, nrow(srvData)) 
		  weight="weight" }
	 srvData$weight <- srvData[,weight]
	 if (is.null(cmpVar)) {
		  srvData$cmpVar <- as.factor(rep(1, nrow(srvData))) 
		  cmpVar="cmpVar" }
	 srvData$cmpVar <- as.factor(srvData[,cmpVar])
	 if (is.null(groupingVars)) {
		  stop("'groupingVars' must contain at least one valid variable name!") }
	 if (!any(colnames(srvData) %in% groupingVars)) {
		  stop("'groupingVars' must contain at least one valid variable name! Please check your data!") }
	 if (length(groupingVars) > 1) {
		  srvData$categories <- as.factor(apply(srvData[,groupingVars], 1, paste, collapse="_")) 
	 } else {
		  srvData$categories <- as.factor(srvData[,groupingVars]) }
	 varslist <- c("cmpVar","ineqVar",groupingVars,"weight","categories")
	 if (naomit) {
		  dataValid <- na.omit(srvData[,varslist]) 
	 } else {
		  dataValid <- srvData[,varslist]
	 }
	 if (length(table(dataValid$ineqVar)) != 2 | any(as.numeric(table(dataValid$ineqVar))==0)) {
		  stop("'ineqVar' must be vector of two of valid values sequence! (The higher value is always the reference!)")}
	 dataValid$ineqVar <- as.numeric(as.factor(dataValid$ineqVar))-1
	 
	 geginiDatas <- list()
	 cmprs <- levels(unique(dataValid$cmpVar))
	 for (i in 1:length(cmprs)) {
		  k		<- dataValid[dataValid$cmpVar==cmprs[i],]																  #select a given subgroup
		  rv		<- as.numeric(crosstab(dataValid$ineqVar, dataValid$cmpVar, weights=dataValid$weight, plot=F)$tab[2,])[i] #get the number of  participant (weighted)
		  rv.sh	<- as.numeric(crosstab(k$categories,k$ineqVar,prop.c=TRUE, weight=k$weight, plot=F)$prop.col[,2])			  #get p rate in df!
		  strukt	<- as.numeric(crosstab(k$categories,k$ineqVar, weight=k$weight, plot=F)$rs) 									  #get k numbers of df
		  tot.sh  <- strukt/sum(strukt)																					  #get k rate of df
		  geginiDatas[[cmprs[i]]] 		 			<- list()
		  geginiDatas[[cmprs[i]]][['df']] 			<- data.frame(p=rv.sh, k=tot.sh)
		  rownames(geginiDatas[[cmprs[i]]][['df']])<- attr(crosstab(k$categories,k$ineqVar,prop.c=TRUE, weight=k$weight, plot=F)$rs, "names")
		  geginiDatas[[cmprs[i]]][['rv']] 			<- rv
		  geginiDatas[[cmprs[i]]][['str']]			<- strukt
		  class(geginiDatas[[cmprs[i]]]) 			<- "geginiData"
	 }
	 geginiDatas
}
