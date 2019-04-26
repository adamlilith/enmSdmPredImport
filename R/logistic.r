#' Bivariate logistic function
#'
#' The bivariate logistic function.
#' @param x1 Numeric, first variable.
#' @param x2 Numeric, second variable. Default is 0 (makes the function respond only to \code{x1} so it is univariate).
#' @param b0 Numeric, intercept. Default is 0.
#' @param b1 Numeric, slope with respect to \code{x1}. Default is 1.
#' @param b2 Numeric, slope with respect to \code{x2}. Default is 0.
#' @param b12 Numeric, slope with respect to \code{x1 * x2}. Default is 0 (no interaction).
#' @param ... Other arguments (not used).
#' @return A logistic function.
#' @examples
#' x1 <- seq(-1, 1, by=0.01)
#' pr1 <- logistic(x1=x1, b1=1)
#' pr2 <- logistic(x1=x1, b1=2)
#' pr3 <- logistic(x1=x1, b1=3)
#' pr4 <- logistic(x1=x1, b0=-1, b1=3)
#' 
#' plot(x1, pr1, ylab='Logistic output', type='l', ylim=c(0, 1))
#' lines(x1, pr2, col='red')
#' lines(x1, pr3, col='blue')
#' lines(x1, pr4, col='green')
#' @export

logistic <- compiler::cmpfun(function(
	x1,
	x2=0,
	b0=0,
	b1=1,
	b2=0,
	b12=0,
	...
) {
	
	exp(b0 + b1 * x1 + b2 * x2 + b12 * x1 * x2) / (1 + exp(b0 + b1 * x1 + b2 * x2 + b12 * x1 * x2))
	
})
attr(logistic, 'equationType') <- 'logistic'
