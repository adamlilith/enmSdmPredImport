#' Standardized bivariate Gaussian function
#' 
#' Bivariate Gaussian function useful for simulating the response of a species to two environmental variables. The function is standardized so that it yields a value of 1 when the covariates are at their "optimal" values. The default values produce a univariate Gaussian function centered on 0 with unit variance.
#' @param x1 Numeric, values of the first variable.
#' @param x2, Numeric, values of the second variable.
#' @param mu1 Numeric, mean of \code{x1} in a Gaussian function. The maximum value of the function with respect to \code{x1} occurs at this value of \code{x1}.
#' @param mu2 Numeric, mean of \code{x2} in a Gaussian function. The maximum value of the function with respect to \code{x2} occurs at this value of \code{x2}.
#' @param sigma1 Numeric, standard deviation of \code{x1} in a Gaussian function. Scales the width of the response to \code{x1}.
#' @param sigma2 Numeric, standard deviation of \code{x2} in a Gaussian function. Scales the width of the response to \code{x2}.
#' @param rho Numeric, strength of covariance between \code{x1} and \code{x2}.
#' @param ... Other arguments (unused).
#' @export

gaussian <- compiler::cmpfun(
	function(
		x1,
		x2=0,
		mu1=0,
		mu2=0,
		sigma1=1,
		sigma2=0,
		rho=0,
		...
	) {

	# writing this piece-by-piece because putting it in one line
	# causes errors when predicting to rasters
	first <- ((x1 - mu1) / sigma1)^2
	prod <- ((2 * rho * (x1 - mu1) * (x2 - mu2)) / (sigma1 * sigma2))
	second <- ((x2 - mu2) / sigma2)^2
	denom <- 2 * (1 - rho^2)

	inside <- first - prod + second
	inside <- (-1 * inside) / denom
	
	expo <- exp(inside)
	
	expo
	
})
attr(gaussian, 'equationType') <- 'gaussian'
