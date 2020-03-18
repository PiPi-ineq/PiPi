
#' Generalized GEGINI
#' 
#' \code{gegini} calculates and optionally displays the generalized Educational GINI (GEGINI) 
#' value as a measure of inequality (see References).
#' @param df a data.frame which contains \code{'p'} and \code{'k'} columns. If \code{df} 
#'   is \code{NULL}, \code{p} and \code{k} must be provided (see below).
#' @param p a numeric vector; each value refers to the percentage of a given group 
#'   relative to the total number of participant. The sum of \code{p} should be 1 (=100%) or 
#'   \code{normalize} must be \code{TRUE} (see below).
#' @param k a numeric vector; each value refers to the percentage of a given group 
#'   relative to the total. (TODO: how does this differ from 'p'?) The sum of \code{k} should be 1 (=100%) or 
#'   \code{normalize} must be \code{TRUE} (see below).
#' @param normalize logical flag whether \code{p} and \code{k} must be rescaled to 100% 
#'   (defaults to \code{TRUE})
#' @param plot logical flag whether to plot the Lorenz curve (default: \code{FALSE}) 
#' @param returnTable logical flag whether to return the ordered table (defaults to \code{FALSE} 
#' @param ... further arguments passed to \code{\link{plot.gegini}}. Ignored if 
#'   \code{plot} is \code{FALSE}. 
#' @return If \code{returnTable} is \code{FALSE}, an object of class \code{gegini}: The 
#'   uncorrected value of the gegini-index. Otherwise, a list object of class \code{geginiTable}:
#' \itemize{
#'   \item{gegini}{an object of class \code{gegini}, the uncorrected value of the gegini-index}
#'   \item{inequalityTable}{the ordered table by which the gegini-index and plot are defined}
#' }
#' @references 
#' Hellevik, O. (1997). Class inequality and egalitarian reform. Acta sociologica, 40(4), pp.377-398.\cr
#' Hellevik, O. (2000). Debate on Inequality: A Less Biased Allocation Mechanism Ottar Hellevik. 
#'   Acta Sociologica, 43(1), 81-83.\cr
#' Hu, A. (2015). Evaluating Educational Inequality Within Educational Expansion: A Formal Comparison 
#'   Between Odds Ratio and the Educational Gini Coefficient. The Journal of Mathematical 
#'   Sociology, 39(4), 280-303.
#' @export 
#' @example inst/examples/gegini_ex.R
#' 
gegini <- function(df=NULL, p=NULL, k=NULL, normalize = TRUE, plot=FALSE, returnTable=FALSE, ...) {
	 if (!is.null(df)) {
		  if (!all(hasName(df, c("p", "k"))))
			   stop("'df' must contain 'p' and 'k' columns")
		  p <- df$p
		  k <- df$k
	 }	
	 if (any(p<0)) 
		  stop("'p' cannot contain negative values!")
	 if (any(k<0)) 
		  stop("'k' cannot contain negative values!")
	 if (all(p==0)) 
		  stop("'p' cannot contain only 0 values!")
	 if (all(k==0)) 
		  stop("'k' cannot contain only 0 values!")
	 if (length(p) != length(k))
		  stop("The lengths of the vectors 'p' and 'k' must be the same!")
	 if (is.null(p) | is.null(k)) 
		  stop("'p' and 'k' must contain valid values!")
	 if (!is.numeric(p) | !is.numeric(p))
		  stop("'p' and 'k' must contain numeric values!")
	 if (round(sum(p), 6) != 1) {
		  warning("The sum of p is not equal to 100%!\n", call. = FALSE)
		  if (normalize) {
			   warning("'p' vector is normalized\n")
			   p <- p/sum(p)  
		  } else {
			   stop("Please correct the p values, or set 'normalize' parameter to 'TRUE'!")
		  }
	 }
	 if (round(sum(k), 6) != 1) {
		  warning("The sum of k is not equal to 100%!\n", call. = FALSE)
		  if (normalize) {
			   warning("'k' vector is normalized\n")
			   k <- k/sum(k)  
		  } else {
			   stop("Please correct the k values, or set 'normalize' parameter to 'TRUE'!")
		  }
	 }
	 x		<- data.frame(p, k, stringsAsFactors=FALSE)
	 if (!is.null(df)) {
		  rownames(x) <- rownames(df) }
	 x$slope	<- x$p/x$k   
	 x		<- x[order(x$slope),]
	 x$cumSum<- cumsum(x$k)
	 k		<- (x$k*x$p)/2
	 kk		<- x$p*(1-x$cumSum)
	 gegini	<- 2*(0.5-(sum(k)+sum(kk)))
	 class(gegini) <- 'gegini'
	 if (plot) {
		  plot.gegini(x, value=gegini, ...)
	 }
	 if (returnTable) {
		  gegini 		 <- list(gegini=gegini, inequalityTable=x)
		  class(gegini) <- 'geginiTable' }
	 gegini
}

#' Plot the GEGINI-Index
#' 
#' @param x an object of class \code{geginiTable} or a data.frame of \code{p}
#'   and \code{k} columns (see \code{\link{gegini}}
#' @param value numeric vector of \code{\link{gegini}} values. Ignored if \code{x} is 
#'   of class \code{geginiTable}.
#' @param lwd numeric value; the width of the line
#' @param xlab character string; the label of the \code{x} axis
#' @param ylab character string; the label of the \code{y} axis
#' @param main character string; the title of the plot  
#' @param texts logical flag; if \code{TRUE} (the default), #TODO ...finish
#' @param main2 character string; the subtitle of the plot
#' @param ... further arguments passed to \code{plot}
#' @seealso See \code{\link{gegini}} for example.
#' @export
plot.gegini <- function(
  x, value=NULL, lwd = 2, xlab = "p", ylab = "L(p)", 
  main = "GEGINI - Lorenz curve", texts=TRUE, main2="", ...
) {
	 if (class(x) == 'geginiTable') {
		  value	<- as.numeric(x$gegini)
		  x <- x$inequalityTable
	 }
	 L <- c(0,cumsum(x$p))
	 K <- c(0,cumsum(x$k)) # TODO: *1/max(cumsum(kis$k)) - its value is 1, and uses an external data.frame (`kis`)
	 if (!is.null(value))
		  main <- paste0(main, "\n", "gegini=", round(value,4)*100, "%")
  plot(K, L, type = "l", main = paste(main, main2, sep="\n"), lwd = lwd, xlab = xlab, 
    ylab = ylab, xaxs = "i", yaxs = "i", ...)
  abline(0, max(L)) 
	 points(K,L,pch=19, cex=2, col="blue")
	 if (texts) {
		  LL <- apply(cbind(L[-length(L)], L[-1]),1,mean)
		  KK <- apply(cbind(K[-length(K)], K[-1]),1,mean)
		  half <- round(nrow(x)/2)
		  alls <- nrow(x)
		  text(KK[1:half],LL[1:half],labels=rownames(x)[1:half], cex=0.7, adj=c(0,0))
		  text(KK[(half+1):alls],LL[(half+1):alls],labels=rownames(x)[(half+1):alls], cex=0.7, adj=c(1,1))
	 }	
  invisible(NULL)
}

#' Compute the Maximum Value of GEGINI
#' 
#' @param strukt the number of person in each groups (total)
#' @param rv the total number of participants
#' @return an object of class \code{geginiMax}
#' @keywords internal
geginiMax <- function(strukt=NULL, rv=NULL) {
	 if (is.null(strukt) | is.null(rv)) {
		  stop("Please define valid 'strukt' and 'rv' values!") }
	 hossz 	<- length(strukt)
	 ixx		<- 1:hossz
	 ix		<- ixx
	 if (rv > sum(strukt)) {
		  warning("There are more participants than the entire population! It has been adjusted to sum of participant! Be careful when interpreting the result!", call. = FALSE)	
		  rv	<- sum(strukt)
	 }
	 RV 		<- rv
  
	 ment.str 	<- NULL
	 ment.rv 	<- NULL
	 ment.ix		<- NULL
	 holSTOP.base <- 0
  
	 CS0 <- 1
	 CS 	<- 1
	 szintlepes <- list()
	 szintlepes[[1]]						<- list()
	 szintlepes[[1]][["ix.tmp"]]			<- ix 
	 szintlepes[[1]][["holSTOP.base"]]	<- 1
	 holSTOP								<- 1
  
	 while (CS0 == 1) {
		  CS 	<- 1
		  szintlepes[[length(szintlepes)]][["ix.tmp"]]		<- ix
		  szintlepes[[length(szintlepes)]][["holSTOP.base"]]	<- holSTOP	
#		df.generator(ix)
		  cummulalt 	<- cumsum(strukt[ix])
		  holSTOP 	<- min(which(cummulalt >= RV))			
		  ment.str	<- rbind(ment.str, strukt[ix])
		  if (holSTOP > 1) {
			   ment.rv <- rbind(ment.rv, c(strukt[ix][1:holSTOP-1], RV-cummulalt[holSTOP-1], rep(0, hossz-holSTOP)))
		  } else {
			   ment.rv	<- rbind(ment.rv, c(RV, rep(0, hossz-1)))
		  }
		  ment.ix		<- rbind(ment.ix, ix)
		  
		  if (holSTOP > szintlepes[[length(szintlepes)]][["holSTOP.base"]] & (holSTOP != hossz)) {  # tehát ha nagyobb szintre lép, és az nem az utolsó szint
			   szintlepes[[length(szintlepes)+1]]					<- list()
			   szintlepes[[length(szintlepes)]][["holSTOP.base"]]	<- holSTOP
			   ix <- c(ix[1:(holSTOP-1)], ix[holSTOP:hossz][c((hossz-holSTOP+1), 1:(hossz-holSTOP))])	# léptetek egyet
			   szintlepes[[length(szintlepes)]][["ix.tmp"]]		<- ix
		  } else {			# ha a végére ért, vagy ha azonos szinten maradt
			   if (holSTOP != hossz & holSTOP != 1) {
				    holSTOP	<- szintlepes[[length(szintlepes)]][["holSTOP.base"]]
				    ix <- c(ix[1:(holSTOP-1)], ix[holSTOP:hossz][c((hossz-holSTOP+1), 1:(hossz-holSTOP))])
			   } else {
				    if (holSTOP == hossz) {
					     if (length(szintlepes) > 1) { # Ha a végére esik a telítődés, de nem alapból, akkor önmaga marad (következő szintre fog lépni
						      ix <- ix
					     } else {
						      ix <- ix[c(hossz, 1:(hossz-1))]
						      holSTOP	<- szintlepes[[length(szintlepes)]][["holSTOP.base"]]
						      CS <- 0
					     }
				    } else {
					     ix <- ix[c(hossz, 1:(hossz-1))]
				    }
			   }
		  } 
		  if (length(szintlepes)==1 & all(ix == ixx)) { break }					# Ha visszatért a legelejére, akkor álljon meg
		  while (CS == 1) {			# addig csinálja míg vagy az alapszintre ér, vagy pedig egy szintet visszalépve még léphet tovább
			   if (any(apply(ment.ix, 1, function(x) all(x == ix)))) {	# Ha már volt az adott állapot, akkor lépjen egy szintet vissza és lépjen tovább
				    if (length(szintlepes)==1 & all(ix == ixx)) { 
					     CS0 <- 0
					     break 
				    } else {						# visszalépett egyet és léptet egyet
					     szintlepes[[length(szintlepes)]]	<- NULL
					     ix 		<- szintlepes[[length(szintlepes)]][["ix.tmp"]]
					     holSTOP	<- szintlepes[[length(szintlepes)]][["holSTOP.base"]]
					     if (holSTOP > 1 ) {  						# tehát ha nem az elsőre ugrott vissza
						      ix <- c(ix[1:(holSTOP-1)], ix[holSTOP:hossz][c((hossz-holSTOP+1), 1:(hossz-holSTOP))])	# léptetek egyet
						      szintlepes[[length(szintlepes)]][["ix.tmp"]]		<- ix
					     } else {
						      ix <- ix[c(hossz, 1:(hossz-1))]					# az első szinten is léptetek egyet
						      szintlepes[[length(szintlepes)]][["ix.tmp"]]		<- ix
					     }
				    }
			   } else {
				    CS <- 0
			   }
		  }
	 }
	 xx	<- data.frame(cbind(ment.rv, ment.str))
	 geginiMax <- suppressWarnings(max(apply(xx, 1, function(x) gegini(data.frame(p=x[1:(length(x)/2)], k=x[((length(x)/2)+1):length(x)])))))
	 class(geginiMax) <- "geginiMax"
	 geginiMax
}
