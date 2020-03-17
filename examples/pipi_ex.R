## prepare the data
x <- geginiData(
  srvData = sam.data, 
  cmpVar = 'cntry', 
  ineqVar = 'vote', 
  groupingVars = 'edu', 
  naomit = TRUE
)
## calculate the PiPi value for any of the countries
selected_country <- levels(sam.data$cntry)[4]
PiPi(x[[selected_country]])

