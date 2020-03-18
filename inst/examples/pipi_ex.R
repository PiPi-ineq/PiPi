## load the data
data(sam.data)

## prepare the data
data <- geginiData(
  srvData = sam.data,
  cmpVar = 'cntry',
  ineqVar = 'vote',
  groupingVars = 'edu',
  naomit = TRUE
)

## calculate the PiPi value for any of the countries
selected_country <- levels(sam.data$cntry)[4]
PiPi(data[[selected_country]])
