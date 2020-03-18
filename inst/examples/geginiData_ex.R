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
