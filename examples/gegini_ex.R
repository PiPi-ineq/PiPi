dta <- data[[levels(sam.data$cntry)[1]]]
gegini(dta[[1]])

data <- geginiData(
  srvData=sam.data, 
  cmpVar='cntry', 
  ineqVar='vote', 
  groupingVars='edu', 
  naomit=TRUE
) 

par(mfrow=c(2,2))
gegini(data[['A']]$df, plot=TRUE, main2="A Country")
gegini(data[['B']]$df, plot=TRUE, main2="B Country")
gegini(data[['C']]$df, plot=TRUE, main2="C Country")
gegini(data[['D']]$df, plot=TRUE, main2="D Country")
