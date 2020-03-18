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

dta1 <- data[[levels(sam.data$cntry)[1]]]
bag1 <- PiPiBagging(dta1, ns=100)
dta2 <- data[[levels(sam.data$cntry)[2]]]
bag2 <- PiPiBagging(dta2, ns=100)

PiPibags.Test(bag1, bag2)
PiPibags.Test(bag1, bag2, alternative='greater')
