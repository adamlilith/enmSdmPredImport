#' Generate a raster stack representing variables on a landscape.
#'
#' This function generates a raster stack in which each layer represents a variable. Each variable can have one of several spatial patterns: linear, uniform, random, step, hinge, Gaussian, or sine. Random noise can also be added to any one of these patterns, and the layer pattern can be split in half (e.g., increasing in the "east" side and decreasing in the "west" side). The landscape can be circular or square. Layers can be rotated around their center to have different orientations relative to other rasters.
#' @param geography List object composed of lists, with one list per layer on the landscape. The name of the sublists correspond to the names of the layers to be generated. Each sublist has some of the following components (some are required, some optional).
#' \itemize{
	#' \item \code{type} (REQUIRED) Character, specifies the spatial pattern of the values of the variable. Depending on the type, different subsequent items in the list can be used to specify the parameters of the pattern:
	#' \itemize{
		#' \item \code{uniform}: Spatially uniform with all values equal to 1.
		#' \item \code{random}: Uniformly randomly drawn values across the range specified by:
		#' \itemize{
			#' \item \code{min} and \code{max}: Numeric, minimum and maximum value.
		#' }
		#' \item \code{linear}: Variable changes linearly across the landscape with these parameters:
		#' \itemize{
			#' \item \code{min} and \code{max}: Numeric, minimum and maximum value.
		#' }
		#' \item \code{step}: A step function pattern with these parameters:
		#' \itemize{
			#' \item \code{min} and \code{max}: Numeric, minimum and maximum value below and above the step.
			#' \item \code{at} Numeric, location of the step specified such that the bottom ("south") edge of the raster is at position -1 and the top ("north") edge is at position 1.
		#' }
		#' \item \code{hinge}: An area with a constant value adjacent to an area with a linearly increasing or decreasing value, which is in turn adjacent to an area with another constant value.
		#' \itemize{
			#' \item \code{min} and \code{max}: Numeric, minimum and maximum value below and above the step.
			#' \item \code{from} Numeric, location where the hinge "starts" such that the bottom ("south") edge of the raster is at position -1 and the top ("north") edge is at position 1.
			#' \item \code{to} Numeric, location where the hinge "ends" such that the bottom ("south") edge of the raster is at position -1 and the top ("north") edge is at position 1.
		#' }
		#' \item \code{gaussian}: A spatially Gaussian distribution with these parameters:
		#' \itemize{
			#' \item \code{center1} and \code{center2}: Numeric, position on (or off) the landscape where the center of the distribution lies where position -1 is at the "south" (\code{center1}) or "west" (\code{center2}) of the raster, and position 1 is at the "north" or "east" side of the raster.
			#' \item \code{sd1} and \code{sd2}: Numeric, standard deviation in the x- and y-directions.
			#' \item \code{rho} Numeric, rotation (interaction between x and y directions). A value of 0 yields no rotation with values closer to 1 collapsing into a univariate distribution in the x-direction and closer to -1 in the y-direction.
		#' }
		#' \item \code{sin} A sine wave with these parameters:
		#' \itemize{
			#' \item \code{freq} Numeric, frequency (assumes going from one edge of the landscape to the other is equal to one wavelength).
			#' \item \code{offset} Numeric, value to add/subtract from the position the the sine wave (when this is 0 then the value of \code{sine(0) = 1} is positioned at the edge of the landscape).
			#' \item \code{min} and \code{max} Numeric, amplitude of the sine wave.
		#' }
	#' }
	#' \item \code{rot} (Optional) Numeric, degrees by which to rotate the raster relative to "north". This is useful for manipulating the correlation between layers.
	#' \item \code{randOrient} (Optional) Logical, if \code{TRUE} then then rotate the raster in a random direction.
	#' \item \code{noisy} (Optional) Logical, if \code{TRUE} then add random noise by randomly swapping values across cells after the pattern specified by \code{type} has been created. Note that swapping ensures the original frequency distribution of the values of the variable is retained. If used, must also specify a value \code{noisyp} which represents the proportion of cells that have values swapped.
	#' \item \code{split} (Optional) Logical, if \code{TRUE} then before any rotation is performed swap values between the upper left ("northwest") corner and the lower right ("southeast") corners of the raster.
#' }
#' @param size Positive integer, number of rows/columns in each landscape raster.
#' @param rescale Either \code{agg} in which case the \code{link[raster]{aggregate}} function is called after the landscape is created, \emph{or} \code{disagg} in which case the \code{link[raster]{disaggregate}} function is called after the landscape is created, \emph{or} \code{NULL} (default) in which case no dis/aggregation is performed. Arguments to can be passed to \code{aggregate} or \code{disaggregate} using \code{...}.
#' @param circle Logical, if \code{TRUE} then the raster stack is cropped to a circle with values outside the circle left as \code{NA}. If \code{FALSE} (default), then the stack is left as a square.
#' @return A raster stack.
#' @examples
#' geog1 <- list(
#' 	uniform=list(type='uniform'),
#' 	random=list(type='random', min=-1, max=1),
#' 	linear=list(type='linear', min=-1, max=1),
#' 	step=list(type='step', min=-1, max=1, at=0.5),
#' 	hinge=list(type='hinge', min=-1, max=1, from=-0.5, to=0),
#' 	gauss=list(type='gaussian', center1=0, center2=0.25,
#' 		sd1=0.5, sd2=0.25, rho=2/3),
#' 	sine=list(type='sin', freq=2, offset=0, min=-1, max=1)
#' )
#' 
#' # circular landscape
#' land <- genesis(geography=geog1, size=201)
#' plot(land)
#' 
#' # square landscape
#' land <- genesis(geography=geog1, size=201, circle=FALSE)
#' plot(land)
#' 
#' # layer rotation
#' geog2 <- list(
#' 	x1=list(type='linear', min=-1, max=1),
#' 	x2=list(type='linear', min=-1, max=1, rot=45)
#' )
#' land <- genesis(geog2, size=201, circle=TRUE)
#' plot(land)
#' 
#' # fancy stuff
#' set.seed(123)
#' geog3 <- list(
#' 	control=list(type='linear', min=-1, max=1),
#' 	noisy=list(type='linear', min=-1, max=1, noisy=TRUE),
#' 	split=list(type='linear', min=-1, max=1, split=TRUE),
#' 	randOrient=list(type='linear', min=-1, max=1, randOrient=TRUE),
#' 	all=list(type='linear', min=-1, max=1, noisy=TRUE, split=TRUE,
#' 		randOrient=TRUE)
#' )
#' land <- genesis(geog3, size=201)
#' plot(land)
#' @export

genesis <- function(
	geography,
	size=1001,
	rescale=NULL,
	circle=TRUE,
	verbose=FALSE,
	...
) {

	# position rasters... needed if generating from scratch
	x <- matrix(rep(seq(-1, 1, length.out=size), size), nrow=size, byrow=TRUE)
	y <- matrix(rep(seq(1, -1, length.out=size), each=size), nrow=size, byrow=TRUE)

	# position rasters... needed if generating from scratch
	dist <- sqrt(x^2 + y^2)

	# position rasters... needed if generating from scratch
	template <- ifelse(dist <= 1, 1, NA)
	template <- raster::raster(template)

	# generate each raster or obtain from disk
	# for (i in seq_along(geography)) {
	for (i in seq_along(geography)) {

		if (verbose) omnibus::say('Creating landscape layer ', i)
	
		if (any(names(unlist(geography[[i]])) %in% 'pregen')) {
			
			# get pre-generated raster
			if (geography[[i]]$pregen) {
				usepregen <- TRUE
			} else {
				usepregen <- FALSE
			}
			
		} else {
			usepregen <- FALSE
		}

		### use pre-generated raster
		if (usepregen) {

			mat <- raster(
				paste0(workDir, '/Simulated Landscape Rasters/',
					geography[[i]]$type,
					'From',
					sub(pattern='[-]', replacement='Neg', x=as.character(min)),
					'To',
					sub(pattern='[-]', replacement='Neg', x=as.character(max)),
					'Rotation',
					ifelse(any(names(unlist(geography[[i]])) %in% 'rot'), sub(as.character(geography[[i]]$rot), pattern='[.]', replacement='pt'), 0),
					ifelse(any(names(unlist(geography[[i]])) %in% 'split'), '_split', ''),
					'.tif'
				)
			)
		
		### generate raster from scratch
		} else {
				
			# uniform values
			mat <- if (geography[[i]]$type=='uniform') {
			
				matrix(rep(1, size^2), nrow=size)
		
			# random values
			} else if (geography[[i]]$type=='random') {
			
				matrix(runif(n=size^2, min=geography[[i]]$min, max=geography[[i]]$max), nrow=size)

			# linear
			} else if (geography[[i]]$type=='linear') {
		
				matrix(rep(seq(geography[[i]]$max, geography[[i]]$min, length.out=size), each=size), nrow=size, byrow=T)
			
			# step
			} else if (geography[[i]]$type=='step') {
			
				(y >= geography[[i]]$at) * geography[[i]]$max + (y < geography[[i]]$at) * geography[[i]]$min
			
			# hinge
			} else if (geography[[i]]$type=='hinge') {
			
				(y >= geography[[i]]$to) * geography[[i]]$max +
				(y >= geography[[i]]$from & y < geography[[i]]$to) * (((geography[[i]]$max - geography[[i]]$min) / (geography[[i]]$to - geography[[i]]$from)) * (y - geography[[i]]$from) + geography[[i]]$min) +
				(y < geography[[i]]$from) * geography[[i]]$min
			
			# gaussian
			} else if (geography[[i]]$type=='gaussian') {

				gaussian(x1=x, x2=y, mu1=geography[[i]]$center1, mu2=geography[[i]]$center2, sigma1=geography[[i]]$sd1, sigma2=geography[[i]]$sd2, rho=geography[[i]]$rho)
			
			# sine
			} else if (geography[[i]]$type=='sin') {
			
				(geography[[i]]$max  - geography[[i]]$min) * ((sin(geography[[i]]$freq * pi * y - geography[[i]]$offset) + 1) / 2) + geography[[i]]$min
			
			}
			
			# split (swap) values of raster from top to bottom on one half of raster
			if (any(names(unlist(geography[[i]])) %in% 'split')) {

				if (geography[[i]]$split) mat[1:size, 1:round( 0.5 * ncol(mat))] <- mat[size:1, round( 0.5 * ncol(mat)):1]
				
			}
		
			# rotate raster
			if (any(names(geography[[i]])=='rot')) if (!is.na(geography[[i]]$rot)) mat <- omnibus::rotMatrix(x=mat, rot=geography[[i]]$rot)
		
			# random orientation for rasters of "linear-ish" type
			if (any(names(geography[[i]])=='randOrient')) if (!is.na(geography[[i]]$randOrient)) {
				mat <- omnibus::rotateMatrix(mat, runif(1, 0, 360 - eps()))
			}
		
			if (class(mat)!='RasterLayer') mat <- raster::raster(mat)
			
			# trim values to circle
			if (circle) mat <- mat * template
			
			
		}

		# stack
		landscape <- if (!exists('landscape', inherits=FALSE)) { raster::stack(mat) } else { raster::stack(landscape, mat) }

	} # next raster

	# add noise to any raster that needs it
	landscape <- noisy(landscape, geography)

	# rescale
	if (!is.null(rescale)) {

		landscape <- if (rescale == 'agg') {
			raster::aggregate(landscape, ...)
		} else if (rescale == 'disagg') {
			raster::disaggregate(landscape, ...)
		}
	}
	
	raster::projection(landscape) <- '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs' # Mollweide (equal area)
	landscape <- raster::setMinMax(landscape)
	
	# name landscape
	names(landscape) <- names(geography)
	
	landscape
	
}
