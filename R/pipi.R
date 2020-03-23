#' Calculate the PiPi Inequality Index
#'
#' @aliases pipi
#' @param dta an object of class \code{\link{geginiData}}
#' @details PiPi is an inequality index which takes into consideration 
#'   the given social constellation:
#'   \enumerate{
#'     \item{Number of participants in a sample (e.g. number of voters in a given country)}
#'     \item{Size of social categories (e.g. proportion of highly educated citizens in the country)}
#'   }
#' The index is normalized, so that it takes values from 0 to 1.
#' @references Susánszky, Somogyi and Tóth, manuscript
#' @return A list of class \code{PiPi}:
#' \describe{
#'   \item{data}{The input table}
#'   \item{PiPi}{Numeric value [0,1] with the same interpretation as the Gini-index, where 0 means there is no inequality between groups, and 1 means perfect inequality.}
#'   \item{geginiRaw}{gegini value without structural compensation}
#'   \item{StrMax}{theoretical maximum of gegini for the given structural constellation}
#'   \item{StrMin}{theoretical minimum of gegini for the given structural constellation}
#' }
#' @seealso \code{\link{gegini}}
#' @export
#' @example inst/examples/pipi_ex.R
#'   
PiPi <- function(dta) {
  baseValue<- gegini(df=dta$df)
  maxValue<- as.numeric(geginiMax(strukt=dta$str, rv=dta$rv))
  minValue<- as.numeric(gegini(data.frame(p=round(dta$rv*dta[['df']]$k)/sum(round(dta$rv*dta[['df']]$k)), k=dta[['df']]$k)))
  PiPiValue   <- as.numeric((baseValue-minValue)/(maxValue-minValue))
  if (PiPiValue<0) PiPiValue <- 0
  PiPi<- list(data=dta, PiPi=PiPiValue, geginiRaw=baseValue, StrMax=maxValue, StrMin=minValue)
  class(PiPi)<- "PiPi"
  PiPi
}


#' A permutation test for PiPi statistic calculated by using \code{ns} random permutations of
#' \code{dta} data for the given social structure, to establish the rank of the observed statistic
#' in relation to the \code{ns} simulated values. 
#' 
#' This statistical method tests whether observed PiPi differs from 0.
#' 
#' @param dta an object of class \code{\link{geginiData}}
#' @param ns integerish value; the number of bootstrap replicates
#' @param prev an object of class \code{PiPiBag}. By providing \code{prev} one can add new 
#'   resample results to the results of the previous one.
#' @param .progress logical; whether to show progress bars
#' @param prob numeric value in the range of [0, 1]; a probability value for CI.
#' @param ... further arguments passed to \code{PiPi}
#' @return an object of class \code{PiPi.test}
#' @export 
#' @example inst/examples/pipitest_ex.R
PiPi.Test <- function(dta=NULL, ns=100, prev=NULL, .progress=TRUE, prob=.95, ...) {
  if (is.null(prev)) {
    prev <- list(PiPi0=NULL, geginiRaw0=NULL, StrMax0=NULL, StrMin0=NULL)
    prev$PiPiOriginal <- PiPi(dta, ...)
  } 
  empi <- cbind(dta$df, Np=dta$df$p*dta$rv,Nt=dta$str)
  empi$Nnp <- empi$Nt-empi$Np
  empi$names <- rownames(empi) 
  dd <- apply(empi[,c('Np', 'Nnp', 'names')],1, function(x) cbind(group=x[3], rv=c(rep(1, x[1]), rep(0, x[2]))))
  dd <- Reduce(function(...) merge(..., all=TRUE), dd)
  dd$rv <- as.numeric(as.character(dd$rv))
  if (.progress) {
    pb <- txtProgressBar(style = 3)
  }
  for (i in 1:ns) {
    if (.progress) setTxtProgressBar(pb, i/ns)
    resamp <- data.frame(group=dd[sample(1:nrow(dd), nrow(dd), replace=FALSE),'group'], rv=dd[sample(1:nrow(dd), nrow(dd), replace=FALSE),'rv'])
    dtaRS <- geginiData(srvData=resamp, ineqVar='rv', groupingVars='group', weight=NULL, naomit=TRUE) 
    cache <- PiPi(dtaRS[[1]], ...)
    prev$PiPi0 <- c(prev$PiPi0, cache$PiPi)
    prev$geginiRaw0 <- c(prev$geginiRaw0, cache$geginiRaw)
    prev$StrMax0 <- c(prev$StrMax0, cache$StrMax)
    prev$StrMin0 <- c(prev$StrMin0, cache$StrMin)
  }
  if (.progress) {close(pb) }
  prev$prob <- prob
  prev$PiPi0.mean <- mean(prev$PiPi0)
  prev$PiPi0.median <- median(prev$PiPi0)
  q<- c((1-prob)/2, (1-(1-prob)/2))
  prev$PiPi0.CI_lower <- sort(prev$PiPi0)[round(length(prev$PiPi0)*q[1])]
  prev$PiPi0.CI_upper <- sort(prev$PiPi0)[round(length(prev$PiPi0)*q[2])]
  x<- prev$PiPi0
  prev$wilcox0<- wilcox.test(x, mu = prev$PiPiOriginal$PiPi, conf.level = prob)
  print(prev$wilcox0)
  class(prev) <- 'PiPi.Test'
  prev
}
