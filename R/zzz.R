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


#' Example data set #TODO
#'
#' An example data set containing political participation, education level and country.
#' The variables are as follows:
#'
#' \itemize{
#'   \item vote. political participation. Factor w/ 2 levels "vote","not-vote"
#'   \item edu. education level. Factor w/ 3 levels "primary","secondary","tertiary"
#'   \item cntry. country. Factor w/ 4 levels "A","B","C","D"
#' }
#'
#' @docType data
#' @keywords datasets
#' @name sam.data
#' @usage data(sam.data)
#' @format A data frame with 8000 rows and 3 variables
NULL