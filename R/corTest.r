#' Robust correlation test
#'
#' Correlation test robust to \code{NA}s in the data.
#' @param x Numeric vector.
#' @param y Numeric vector.
#' @return Pearson's correlation coefficient.
#' @export

corTest <- compiler::cmpfun(function(x, y) {

	# cull to shortest vector
	complete <- stats::complete.cases(x, y)
	x <- x[complete]
	y <- y[complete]
	
	out <- if (!all(complete)) {
		NA
	} else {
		stats::cor(x, y, use='complete.obs')
	}
	
	out
	
})
