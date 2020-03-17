data <- geginiData(
  srvData=sam.data, 
  cmpVar='cntry', 
  ineqVar='vote', 
  groupingVars='edu', 
  naomit=TRUE
) 
dta <- data[[levels(sam.data$cntry)[4]]]
testV <- PiPi.Test(dta, ns=100)