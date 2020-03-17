# 
# TODO: Finish or drop
# This could be a vignette in which all relevant functionalities of 
# the package are demonstrated. 
#
###############################################################################

#TODO-devs: this is just copy-paste, needs minor refactoring
if (FALSE) {
  setwd("D:/GoogleDrive/PIPI")
  load("ineqmax.RData")
  source("./pkg_/gegini3.R")
  source("./pkg_/plot.gegini3.R")
  source("./pkg_/geginiMax3.R")
  source("./pkg_/geginiData3.R")
  source("./pkg_/PiPi3.R")
  source("./pkg_/PiPiBagging3.R")
  source("./pkg_/PiPi.Test3.R")
  source("./pkg_/PiPibags.Test3.R")
  load("./pkg_/sam.data.Rda")
  
  
  data <- geginiData(srvData=sam.data, cmpVar='cntry', ineqVar='vote', groupingVars='edu', naomit=TRUE) 
  
  PiPi.Min<- NULL
  PiPi.Max<- NULL
  PiPi.Mean<- NULL
  
  for (i in 1:4){
    dta	<- data[[levels(sam.data$cntry)[i]]]
    bags<- NULL
    bags <- PiPiBagging(dta, ns=100)
    PiPi.Min<- c(PiPi.Min, bags$PiPi.CI_lower) 
    PiPi.Max<- c(PiPi.Max, bags$PiPi.CI_upper) 
    PiPi.Mean<- c(PiPi.Mean, bags$PiPi.mean)
  }
  
  tafel<- data.frame(PiPi.Min = PiPi.Min,
    PiPi.Max = PiPi.Max,
    PiPi.Mean = PiPi.Mean)
  
  tafel
  
  dta <- data[[levels(sam.data$cntry)[4]]]
  gegini(dta[[1]])
  PiPi(dta)
  
  testV <- PiPi.Test(dta, ns=100)
  testV$PiPi0.CI_upper 	# random CI - upper limit
  testV$PiPiOriginal$PiPi # observed value in the data
  
  bag1 <- PiPiBagging(dta, ns=100)
  dta2 <- data[[levels(sam.data$cntry)[2]]]
  bag2 <- PiPiBagging(dta, ns=100)
  PiPibags.Test(bag1, bag2)
  PiPibags.Test(bag1, bag2, alternative='greater')
  
  
# Eredeti vizsgálat:
  dtaAll <- geginiData(srvData=aldata, cmpVar='cntry', ineqVar='rv', groupingVars='StrStand', weight='dweight', naomit=TRUE) 
  
  pars <- par()
  par(mfrow=c(2,2))
  gegini(dtaAll[['HU']]$df, plot=T, main2="Hungary")
  gegini(dtaAll[['ES']]$df, plot=T, main2="Spain")
  gegini(dtaAll[['FR']]$df, plot=T, main2="France")
  gegini(dtaAll[['PT']]$df, plot=T, main2="Portugal")
  par(pars)
  
# Ha kell vissza is tudja adni a táblát:
  (geginiT <- gegini(dtaAll[['HU']]$df, plot=F, returnTable=T))
  plot.gegini(geginiT, main2="Hungary")
  
  dtaAll <- geginiData(srvData=aldata, cmpVar='cntry', ineqVar='rv', groupingVars='MobPot', weight='dweight', naomit=TRUE) 
  
  pars <- par()
  par(mfrow=c(2,2))
  gegini(dtaAll[['HU']]$df, plot=T, main2="Hungary")
  gegini(dtaAll[['ES']]$df, plot=T, main2="Spain")
  gegini(dtaAll[['FR']]$df, plot=T, main2="France")
  gegini(dtaAll[['PT']]$df, plot=T, main2="Portugal")
  par(pars)
  
  
  dtaAll <- geginiData(srvData=aldata, cmpVar='cntry', ineqVar='rv', groupingVars=c('isk2','teltip2','kor3'), weight='dweight', naomit=TRUE) 
  
  pars <- par()
  par(mfrow=c(2,2))
  gegini(dtaAll[['HU']]$df, plot=T, main2="Hungary")
  gegini(dtaAll[['ES']]$df, plot=T, main2="Spain")
  gegini(dtaAll[['FR']]$df, plot=T, main2="France")
  gegini(dtaAll[['PT']]$df, plot=T, main2="Portugal")
  par(pars)
  
  
# Eredeti vizsgálat:
  dtaAll <- geginiData(srvData=aldata, cmpVar='cntry', ineqVar='rv', groupingVars='StrStand', weight='dweight', naomit=TRUE) 
  
  PiPi(dtaAll[['HU']])
  
  
# Csoportos hívás összehasonlítással
  res 	<- lapply(dtaAll, PiPi)
  (pipis 	<- unlist(lapply(res, function(x) x$PiPi)))
  (geginis <- unlist(lapply(res, function(x) x$geginiRaw)) )
  
  summary(pipis-geginis)
#in case of structural min
#      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.0040221  0.0005191  0.0064882  0.0105678  0.0143419  0.0659819 
  
#Eredeti Minimum fgv alapján:
  res2 <- lapply(dtaAll, PiPi, minMethod="statistical", nsim=100)
  (pipis 	<- unlist(lapply(res2, function(x) x$PiPi)))
  (geginis <- unlist(lapply(res2, function(x) x$geginiRaw)) )
#in case of statistical min
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.13709 -0.10686 -0.07594 -0.07702 -0.05932  0.02067
  
  
  ############# Bagging (bootstrap)
#Mivel ez nagyon sok tud lenni, ezért nem lehet egyszerre hívni, hanem országonként kell:
  
  dta	<- dtaAll[['HU']]
  
  (bags <- PiPiBagging(dta, ns=100))
# continue a previuos run (már 200 lesz összesen)
  bags <- PiPiBagging(dta, ns=100, prev=bags)
  
#EZ a te eredeti verziód alapján:
  
#Spanyoloknál a legnagyobb az eltérés:
  dta	<- dtaAll[['ES']]
  (bagsES <- PiPiBagging(dta, ns=100))
  PiPibags.Test(bags, bagsES)
  PiPibags.Test(bags, bagsES, alternative='greater')
  
}

