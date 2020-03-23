#' Estimate Confidence Interval for PiPi
#' 
#' \code{PiPiBagging} uses the bootstrap resampling method to estimate confidence interval 
#' for the PiPi measure.
#' @param dta an object of class \code{\link{geginiData}}
#' @param ns integerish value; the number of bootstrap replicates
#' @param prev an object of class \code{PiPiBag}. By providing \code{prev} one can add new 
#'   resample results to the results of the previous one.
#' @param .progress logical flag if a progress bar should be displayed
#' @param prob numeric value in the range of [0, 1]; the confidence level
#' @param ... further arguments passed to \code{PiPi}
#' @return An object of class \code{PiPiBag}:
#' \describe{
#'   \item{PiPi}{PiPi values for every iteration}
#'   \item{geginiRaw}{GEGINI values for every iteration}
#'   \item{StrMax}{maximum values of PiPi for every iteration}
#'   \item{StrMin}{minimum values of PiPi for every iteration}
#'   \item{PiPiOriginal}{the observed value of PiPi (class of \code{PiPi})}
#'   \item{prob}{value of probability}
#'   \item{PiPi.mean}{mean of all PiPi values (iteration)}
#'   \item{PiPi.median}{median of all PiPi values (iteration)}
#'   \item{PiPi.CI_lower}{lower limit of the Confidence Interval for the PiPi mean, where the limit comes from the \code{prob} parameter}
#'   \item{PiPi.CI_upper}{upper limit of the Confidence Interval for the PiPi mean, where the limit comes from the \code{prob} parameter}
#' }
#' @references 
#' Kolenikov, S. (2010). Resampling Variance Estimation for Complex Survey Data. The Stata Journal, 
#'   10(2), 165–199. https://doi.org/10.1177/1536867X1001000201\cr
#' Chen, H., & Shen, Q. R. (2019). Variance estimation for survey-weighted data using bootstrap 
#'   resampling methods: 2013 methods-of-payment survey questionnaire. In The Econometrics of 
#'   Complex Survey Data: Theory and Applications (pp. 87-106). Emerald Publishing Limited.  
#' @export
#' @example inst/examples/bagging_ex.R
PiPiBagging <- function(dta=NULL, ns=100, prev=NULL, .progress=TRUE, prob=0.95, ...) {
  if (is.null(prev)) {
    prev <- list(PiPi=NULL, geginiRaw=NULL, StrMax=NULL, StrMin=NULL)
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
    resamp <- dd[sample(1:nrow(dd), nrow(dd), replace=TRUE),]
    dtaRS <- geginiData(srvData=resamp, ineqVar='rv', groupingVars='group', weight=NULL, naomit=TRUE) 
    cache <- PiPi(dtaRS[[1]], ...)
    prev$PiPi     <- c(prev$PiPi, cache$PiPi)
    prev$geginiRaw   <- c(prev$geginiRaw, cache$geginiRaw)
    prev$StrMax   <- c(prev$StrMax, cache$StrMax)
    prev$StrMin   <- c(prev$StrMin, cache$StrMin)
  }
  if (.progress) {close(pb) }
  prev$prob			 <- prob
  prev$PiPi.mean     <- mean(prev$PiPi)
  prev$PiPi.median   <- median(prev$PiPi)
  q  <- c((1-prob)/2, (1-(1-prob)/2))
  prev$PiPi.CI_lower   <- sort(prev$PiPi)[round(length(prev$PiPi)*q[1])]
  prev$PiPi.CI_upper   <- sort(prev$PiPi)[round(length(prev$PiPi)*q[2])]
  class(prev)   <- 'PiPiBag'
  prev
}


#' Summary Method for PiPiBag
#'
#' @param object an object of class \code{PiPiBag}
#' @param ... not used at the moment
#' @return the original object of class \code{PiPiBag} (invisible)
#' @export
summary.PiPiBag <- function(object, ...) {
  cat(paste0('The observed level of inequality is ', round(object$PiPiOriginal$PiPi*100,2), '%.\n'))
  cat(paste0('The mean of the bootstrapped PiPi values is ', round(object$PiPi.mean*100,2), '% and the median is ', round(object$PiPi.median*100,2), '%.\n'))
  cat(paste0('The ', object$prob*100, '% Confidence Interval for the observed PiPi is ', round(object$PiPi.CI_lower*100,2), '% - ', round(object$PiPi.CI_upper*100,2), '%.\n'))
  cat(paste0('Number of simulations is ', length(object$PiPi), '.\n'))
  invisible(object)
}


#' Compare PiPi Bags Objects
#' 
#' \code{PiPibags.Test} runs two-sample Wilcoxon (Mann-Whitney) test to decide whether 
#' the difference in inequality values is significant.
#' @param x,y objects of class \code{PiPibag}
#' @param prob numeric value in the range of [0,1]; the confidence level
#' @param ... further arguments passed to \code{\link{wilcox.test}}
#' @return see \code{\link{wilcox.test}}
#' @seealso See \code{\link{PiPiBagging}} for an example.
#' @export
PiPibags.Test <- function(x, y, prob=.95, ...) {
  if (class(x)!="PiPiBag" | class(y)!="PiPiBag")  {
    stop("x and y objects should be 'PiPiBag' objects!") }
  wilcox <- wilcox.test(x$PiPi, y$PiPi, conf.int = TRUE, conf.level = prob, ...)
  wilcox
}

