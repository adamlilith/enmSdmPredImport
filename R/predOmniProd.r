#' Predict OMNISCIENT Gaussian model object using permutation of linear terms
#' 
#' Predict OMNISCIENT Gaussian model object using permutation of linear terms (i.e., not interactions--see \code{\link[enmSdmPredImport]{predictOmniscientPermIa}}.
#' @param x1 Numeric, values of the first variable.
#' @param x2, Numeric, values of the second variable.
#' @param mu1 Numeric, mean of \code{x1} in a Gaussian function. The maximum value of the function with respect to \code{x1} occurs at this value of \code{x1}.
#' @param mu2 Numeric, mean of \code{x2} in a Gaussian function. The maximum value of the function with respect to \code{x2} occurs at this value of \code{x2}.
#' @param sigma1 Numeric, standard deviation of \code{x1} in a Gaussian function. Scales the width of the response to \code{x1}.
#' @param sigma2 Numeric, standard deviation of \code{x2} in a Gaussian function. Scales the width of the response to \code{x2}.
#' @param rho Numeric, strength of covariance between \code{x1} and \code{x2}.
#' @param product Numeric vector representing pre-multiplied (and possibly permuted) product: \code{(x1 - mu1) * (x2 - mu2)}.
#' @param extreme Numeric, most extreme value of product possible in this milieu. Used to standardize result to (0, 1].
#' @return Numeric vector.
#' @export

predOmniProd <- compiler::cmpfun(
	function(x1, x2, mu1, mu2, sigma1, sigma2, rho, product, extreme) {

		# change sign of extreme product
		extreme <- ifelse(rho < 0, -abs(extreme), abs(extreme))
	
		# writing this piece-by-piece because putting it in one line causes errors when predicting to rasters
		first <- ((x1 - mu1) / sigma1)^2
		prod <- ((2 * rho * product) / (sigma1 * sigma2))
		second <- ((x2 - mu2) / sigma2)^2
		adjust <- 2 * (1 - rho^2)

		inside <- first - prod + second
		inside <- (-1 * inside) / adjust
		
		numer <- exp(inside)

		standard <- exp((2 * rho * extreme) / (2 * sigma1 * sigma2 * (1 - rho^2)))
		
		out <- numer / standard
		out
	
	}
)
	
