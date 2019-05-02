#' Univariate logistic function
#'
#' The univariate logistic function. A variant of \code{\link[enmSdmPredImport]{logistic}}, it returns logistic output in response to one or two variables, but includes an additional term for "shifting" the response to the "left" or "right" along \code{x1}.
#' @param x1 Numeric, first variable.
#' @param b0 Numeric, intercept. Default is 0.
#' @param b1 Numeric, slope with respect to \code{x1}. Default is 1.
#' @param b11 Numeric, value by which to "shift" the response along \code{x1}. Default is 0 (no shift).
#' @param ... Additional arguments (not used).
#' @return A logistic function.
#' @examples
#' x1 <- seq(-1, 1, by=0.01)
#' pr1 <- logisticShift(x1=x1, b1=1)
#' pr2 <- logisticShift(x1=x1, b1=2)
#' pr3 <- logisticShift(x1=x1, b1=3)
#' pr4 <- logisticShift(x1=x1, b0=-1, b1=3)
#' 
#' plot(x1, pr1, ylab='Logistic output', type='l', ylim=c(0, 1))
#' lines(x1, pr2, col='red')
#' lines(x1, pr3, col='blue')
#' lines(x1, pr4, col='green')
#' @export

logisticShift <- compiler::cmpfun(function(
	x1,
	b0=0,
	b1=1,
	b11=0,
	...
) {
	
	exp(b0 + b1 * (x1 - b11)) / (1 + exp(b0 + b1 * (x1 - b11)))
	
})
attr(logisticShift, 'equationType') <- 'logisticShift'
