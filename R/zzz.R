#' Calculate Inequalities Across Social Classes
#'
#' Calculate inequalities across social classes if the target variable can coded as 
#' a dummy variable (0, 1). To define the level of inequality the Lorenz Curve is 
#' used, which shows the distribution of participants (e.g. voters) across the 
#' pre-defined social classes (e.g. level of education). 
#' @name PiPi-package
#' @docType package
#' @import descr
#' @importFrom graphics abline plot points text
#' @importFrom stats median na.omit wilcox.test
#' @importFrom utils hasName setTxtProgressBar txtProgressBar
NULL