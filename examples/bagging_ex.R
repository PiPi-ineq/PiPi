dta <- data[[levels(sam.data$cntry)[1]]]

bag1 <- PiPiBagging(dta, ns=100)
dta2 <- data[[levels(sam.data$cntry)[2]]]
bag2 <- PiPiBagging(dta, ns=100)

PiPibags.Test(bag1, bag2)
PiPibags.Test(bag1, bag2, alternative='greater')
