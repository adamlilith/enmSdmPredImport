#' Add noise to a layer of layer raster stack by swapping values
#'
#' Adds noise to a layer of a raster stack by swapping values across cells.
#' @param landscape Raster stack object.
#' @param geography List of lists, each sublist represents a layer in \code{landscape}. See \code{\link[enmSdmPredImport]{genesis}}.
#' @return Raster stack.
#' @examples
#' set.seed(123)
#' geog <- list(
#' 	control=list(type='linear', min=-1, max=1),
#' 	noisy2=list(type='linear', min=-1, max=1, noisy=TRUE, noisyp=0.2),
#' 	noisy4=list(type='linear', min=-1, max=1, noisy=TRUE, noisyp=0.4),
#' 	noisy6=list(type='linear', min=-1, max=1, noisy=TRUE, noisyp=0.6),
#' 	noisy8=list(type='linear', min=-1, max=1, noisy=TRUE, noisyp=0.8)
#' )
#' land <- genesis(geog, size=201)
#' plot(land)
#' @export

noisy <- function(landscape, geography) {

	## adds noise to one or more variables
	# landscape		landscape object
	# geography		geography object
	
	name <- names(landscape)
	
	# for each layer
	for (i in seq_along(geography)) {
	
		if (any(names(unlist(geography[[i]])) %in% 'noisy')) {
		
			if (as.logical(unlist(geography[[i]])[which(names(unlist(geography[[i]])) %in% 'noisy')])) {
			
				# proportion to swap
				p <- as.numeric(unlist(geography[[i]])[which(names(unlist(geography[[i]])) %in% 'noisep')])
			
				val <- c(as.matrix(subset(landscape, i)))
				cellNum <- seq_along(val)
				notNa <- which(!is.na(val))
				
				swap <- sample(notNa, round(p * length(notNa)))
				
				swapA <- swap[1:(floor( 0.5 * length(swap)))]
				swapB <- swap[((floor( 0.5 * length(swap))) + 1):(((floor( 0.5 * length(swap))) + 1) + length(swapA) - 1)]
				
				newvalA <- val[swapB]
				newvalB <- val[swapA]
				
				val[swapA] <- newvalA
				val[swapB] <- newvalB
			
				r <- raster::raster(matrix(val, ncol=ncol(landscape), byrow=FALSE))
				r <- raster::setMinMax(r)
				raster::projection(r) <- raster::projection(landscape)
				
				landscape[[i]] <- r
			
			}
		
		} 
	
	}

	names(landscape) <- name
	landscape
	
}

