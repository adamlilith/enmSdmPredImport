#' Indices of NA values in one or more vectors.
#' 
#' Returns the indices of appearances of \code{NA} values in one or more vectors. The vectors don't have the be of same length, but the results may be less useful if they are not!
#' @param ... One or more vectors.
#' @return Integer vector.
#' @export

whichIsNaVec <- function(...) {

	x <- list(...)
	isna <- integer()
	for (i in seq_along(x)) isna <- c(isna, which(is.na(x[[i]])))
	isna <- sort(unique(isna))
	isna
	
}

