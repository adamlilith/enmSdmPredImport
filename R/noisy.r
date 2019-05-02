#' Add noise to a layer of layer raster stack by swapping values
#'
#' Adds noise to a layer of a raster stack by swapping values across cells. This function swaps values between a user-defined proportion of cells. It ignores (does not swap) cells with \code{NA}.
#' @param landscape Raster stack object.
#' @param geography List of lists, each sublist represents a layer in \code{landscape}. See \code{\link[enmSdmPredImport]{genesis}}.
#' @return Raster stack.
#' @examples
#' set.seed(123)
#' geog <- list(
#' 	noNoise1a=list(type='linear', min=-1, max=1),
#' 	noNoise1b=list(type='linear', min=-1, max=1, noise=0),
#' 	noisy2=list(type='linear', min=-1, max=1, noise=0.2),
#' 	noisy4=list(type='linear', min=-1, max=1, noise=0.4),
#' 	noisy6=list(type='linear', min=-1, max=1, noise=0.6),
#' 	noisy8=list(type='linear', min=-1, max=1, noise=0.8),
#'  noise1a=list(type='linear', min=-1, max=1, noise=1),
#'  noise1b=list(type='random', min=-1, max=1)
#' )
#' land <- genesis(geog, size=201)
#' raster::plot(land)
#' @export

noisy <- function(landscape, geography) {

	## adds noise to one or more variables
	# landscape		landscape object
	# geography		geography object
	
	name <- names(landscape)
	
	# for each layer
	for (i in seq_along(geography)) {

		if (any(names(unlist(geography[[i]])) %in% 'noise')) {

			# proportion to swap
			p <- as.numeric(unlist(geography[[i]])[which(names(unlist(geography[[i]])) %in% 'noise')])

			if (!is.null(p)) {

				rast <- raster::subset(landscape, i)
				val <- raster::values(rast)

				cellNum <- seq_along(val)
				notNa <- which(!is.na(val))

				swap <- sample(notNa, round(p * length(notNa)))
				
				swapA <- swap[1:(floor( 0.5 * length(swap)))]
				swapB <- swap[((floor( 0.5 * length(swap))) + 1):(((floor( 0.5 * length(swap))) + 1) + length(swapA) - 1)]

				newvalA <- val[swapB]
				newvalB <- val[swapA]

				val[swapA] <- newvalA
				val[swapB] <- newvalB

				r <- raster::raster(matrix(val, ncol=ncol(landscape), byrow=TRUE))
				r <- raster::setMinMax(r)
				raster::projection(r) <- raster::projection(landscape)

				landscape[[i]] <- r
				
			}
		
		} 
	
	}

	names(landscape) <- name
	landscape
	
}

