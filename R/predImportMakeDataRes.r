#' Master function to create common sets of data for simulations
#'
#' This function is used to create multiple simulated data sets for further analysis. The simulated data represents data that one would typically need for modeling species distributions (i.e., training and test presences, test absences and background sites). Typical implementation is to use \code{predImportMakeData} to create simulated data sets, then \code{predImportTrainModels} to train SDMs on those data sets, then \code{predImportEval} to evaluate the models. This particular function is similar to \code{predImportMakeData} except that it allows simulating the effects of differences in spatial resolution between the scales at which the species responds to the environment and the scales at which predictor variables are available. See \emph{Details} for more information.
#' @param geography A list of lists describing the simulated environmental layers.  Each sublist pertains to one later. See \code{\link[enmSdmPredImport]{genesis}} for details.
#' @param response A function describing the response of the species to the environment. This must be one of: \code{\link[enmSdmPredImport]{logistic}}, \code{\link[enmSdmPredImport]{logisticShift}}, or \code{\link[enmSdmPredImport]{gaussian}}.
#' @param simDirNative Character, path name of directory in which scenario data files are saved. This directory will contain data at the spatial resolution at which species respond to the environment.
#' @param simDirNative Character, path name of directory in which scenario data files are saved. This directory will contain data at the spatial resolution at which predictors are available (which may or may not correspond to the scale at which species respond to the environment).
#' @param numTrainPres Positive integer, number of training presences to locate.
#' @param numTestPres Positive integer, number of test presences to locate. The number of test absences will also be equal to this value.
#' @param numBg Positive integer, number of training and number of test background sites to locate.
#' @param iters Vector of positive integers, data iterations to generate.
#' @param circle Logical, if \code{FALSE} (default), all landscapes are square. If \code{TRUE} then landscapes are circular.
#' @param sizeNative Positive integer, size of landscape (number of cells on a side) at the resolution at which the species responds to the environment. This can be the same as \code{sizeSampled} or larger or smaller.
#' @param sizeSampled Positive integer, size of landscape (number of cells on a side) at the simulation assumes environmental data is available.
#' @param filePrepend Either \code{NULL} or a character string. If a character string then this is pre-pended to the simulated data file name. If \code{NULL} (default), nothing is appended. File names will be as "sim XXX.RData" (no append) or "PREPEND sim XXX.RData" where "XXX" is the iteration number and PREPEND the string in \code{filePrepend}.
#' @param b0 Numeric, parameters for \code{\link[enmSdmPredImport]{logistic}} or \code{\link[enmSdmPredImport]{logisticShift}} function specified in the \code{response} argument (above). Logistic intercept. Default is \code{NA}.
#' @param b1 Numeric, parameters for \code{\link[enmSdmPredImport]{logistic}} or \code{\link[enmSdmPredImport]{logisticShift}} function specified in the \code{response} argument (above). Logistic slope. Default is \code{NA}.
#' @param b2 Numeric, parameters for \code{\link[enmSdmPredImport]{logistic}} or \code{\link[enmSdmPredImport]{logisticShift}} function specified in the \code{response} argument (above). Logistic slope. Default is c.
#' @param b11 Numeric, parameters for \code{\link[enmSdmPredImport]{logistic}} or \code{\link[enmSdmPredImport]{logisticShift}} function specified in the \code{response} argument (above). Left-right shift along variable. Default is \code{NA}.
#' @param b12 Numeric, parameters for \code{\link[enmSdmPredImport]{logistic}} or \code{\link[enmSdmPredImport]{logisticShift}} function specified in the \code{response} argument (above). Interaction term. Default is \code{NA}.
#' @param mu1 Numeric, parameters for \code{\link[enmSdmPredImport]{gaussian}} function specified in the \code{response} argument (above). Mean of variable. Default is \code{NA}.
#' @param mu2 Numeric, parameters for \code{\link[enmSdmPredImport]{gaussian}} function specified in the \code{response} argument (above). Mean of variable. Default is \code{NA}.
#' @param sigma1 Numeric, parameters for \code{\link[enmSdmPredImport]{gaussian}} function specified in the \code{response} argument (above). Standard deviation of variable. Default is \code{NA}.
#' @param sigma2 Numeric, parameters for \code{\link[enmSdmPredImport]{gaussian}} function specified in the \code{response} argument (above). Standard deviation of variable. Default is \code{NA}.
#' @param rho Numeric, parameters for \code{\link[enmSdmPredImport]{gaussian}} function specified in the \code{response} argument (above). Covariance term. Default is \code{NA}.
#' @param overwrite Logical, if \code{TRUE} then save over pre-existing data files. Default is \code{FALSE}.
#' @param verbose Numeric, if 0 then show minimal output, 1 more output, 2 even more, >2 all of it.
#' @param ... Other arguments (unused).
#' @details This function is useful for examining how well SDMs can measure the importance of variables if the spatial resolution (grain size) of the predictors is different from the spatial scale at which a species responds to the environment. Hereafter, the response scale is called the "native" scale and the scale at which predictors are available the "sampled" scale.  The sampled scale resolution can be larger or smaller than the native scale resolution.  
#' Unlike the \code{predImportMakeData} function, this function creates two \code{sim} objects and saves them in two directories. The first \code{sim} object, saved in the \code{simDirNative} will have training and test data for the native scale. The second \code{sim} object will have training and test data at the sampled scale from the same sites used to generate train and test data at the native scale. Generating two \code{sim} objects using the same underlying sites allows you to compare models trained/tested on the native versus sampled scales.  
#' Rasters in the native and sampled landscapes have the same extent. Thus, even though we're talking about resolution (size of a cell relative to the size of the raster), we specify resolution by the number of cells along the side of the landscape.  Since all landscapes are simulated to have the same extent, specifying a larger number of cells causes the resolution to become higher (grain size smaller).  Thus, the user must specify a size of the landscape (number of cells on a side) at the native and sampled resolutions.
#' @return Nothing (saves data files to disc).
#' @seealso \code{\link[enmSdmPredImport]{predImportMakeData}}, \code{\link[enmSdmPredImport]{predImportTrainModels}}, \code{\link[enmSdmPredImport]{predImportEval}}
#' @export

predImportMakeDataRes <- function(
	geography,
	response,
	simDirNative,
	simDirSampled,
	numTrainPres=200,
	numTestPres=200,
	numBg=10000,
	iters=1:100,
	circle=FALSE,
	sizeNative=1024,
	sizeSampled=256,
	filePrepend=NULL,
	b0=NA, b1=NA, b2=NA, b11=NA, b12=NA, mu1=NA, sigma1=NA, sigma2=NA, rho=NA,
	overwrite=FALSE,
	verbose=1,
	...
) {

	if (verbose >= 0) omnibus::say(date(), ' | Creating simulation data for ', max(iters), ' simulations:', post=0)

	# file prefix
	filePrependEndSpace <- if (!is.null(filePrepend)) { paste0(filePrepend, ' ') } else { '' }

	# flag to indicate if any data creation iterations were skipped
	skippedAny <- FALSE

	# for each simulation sample landscape, train model, and evaluate
	for (iter in iters) {

		# DO NOT re-create data
		simFileExists <- file.exists(paste0(simDirNative, '/', filePrependEndSpace, 'sim ', prefix(iter, 3), '.Rdata'))
		if (!overwrite && simFileExists) {

			omnibus::say(iter, '\U2713', post=0)
			skippedAny <- TRUE

		# RE-CREATE DATA
		} else {

			if (verbose > 0) omnibus::say(iter, post=0)

			### generate new random layers if needed
			########################################

			seed <- as.numeric(Sys.time())
			set.seed(seed)

			# make landscape
			if (iter == 1 | skippedAny) {

				# create FINEST-SCALE landscape
				landscapeNative <<- genesis(geography, circle=circle, size=sizeNative, verbose=verbose > 1)
				resNative <- raster::res(landscapeNative)
				
				# rescale landscape to "SAMPLED" scale at which predictors are available
				templateSampled <- raster::raster(
					nrows=sizeSampled,
					ncols=sizeSampled,
					crs=raster::projection(landscapeNative),
					ext=extent(landscapeNative)
				)
				resSampled <- raster::res(templateSampled)
				landscapeSampled <- if (resNative != resSampled) {
					raster::resample(landscapeNative, templateSampled, ...)
				} else {
					landscapeNative
				}

				# map of species' probability of occurrence
				args <- if (attr(response, 'equationType') == 'logistic') {
					list(x1=landscapeNative[[1]], x2=landscapeNative[[2]], b0=b0, b1=b1, b2=b2, b12=b12)
				} else if (attr(response, 'equationType') == 'logisticShift') {
					list(x1=landscapeNative[[1]], x2=landscapeNative[[2]], b0=b0, b1=b1, b11=b11)
				} else if (attr(response, 'equationType') == 'gaussian') {
					list(x1=landscapeNative[[1]], x2=landscapeNative[[2]], mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
				}
				
				# remake map of species true probability of occurrence
				speciesMap <- do.call(response, args=args)
					
			# re-make any random layers for next simulation
			} else if (any(unlist(geography) %in% 'random')) {
			
				landscapeNative <<- genesis(geography, circle=circle, size=sizeNative)
				
				# rescale landscape to "SAMPLED" scale at which predictors are available
				templateSampled <- raster::raster(
					nrows=sizeSampled,
					ncols=sizeSampled,
					crs=raster::projection(landscapeNative),
					ext=extent(landscapeNative)
				)

				resSampled <- raster::res(templateSampled)
				
				landscapeSampled <- if (resNative != resSampled) {
					raster::resample(landscapeNative, templateSampled, ...)
				} else {
					landscapeNative
				}
				
				# remake map of species true probability of occurrence
				speciesMap <- do.call(response, args=args)
			
			}

			### generate training/test and background sites at NATIVE resolution
			####################################################################

			# create mask from which to sample (avoids inclusion of sites with NAs)
			maskNative <- sum(landscapeNative)

			# estimate sample size needed to get sufficient presences and absences
			n <- round(1.5 * (numTrainPres + numTestPres)) # initial number of randomly located sites to draw
			prevWithNa <- cellStats(speciesMap, 'sum')  / ncell(speciesMap) # prevalence (including NA cells)

			while (n * prevWithNa < numTrainPres + numTestPres | n * (1 - prevWithNa) < numTestPres) { n <- round(n * 1.5) }

			# draw sites
			presAbs <- -Inf # initial sum of sampled presences
			while (sum(presAbs) < numTrainPres + numTestPres | sum(!presAbs) < numTestPres) {

				sites <- sampleRast(x=maskNative, n=n, adjArea=FALSE, replace=TRUE, prob=FALSE)
				prOcc <- extract(speciesMap, sites)
				presAbs <- runif(nrow(sites)) <= prOcc

				n <- 1.5 * n

			}

			### training/test presences and absence SITES
			allPresSites <- sites[which(presAbs), ]
			trainPresSites <- allPresSites[1:numTrainPres, ]
			testPresSites <- allPresSites[(numTrainPres + 1):(numTrainPres + numTestPres), ]
			testAbsSites <- sites[which(!presAbs), ]
			testAbsSites <- testAbsSites[sample(1:nrow(testAbsSites), numTestPres), ]

			### compile NATIVE training/test ENVIRONMENTAL DATA
			presBg <- data.frame(presBg=c(rep(1, numTrainPres), rep(0, numBg)))

			trainPresNative <- as.data.frame(extract(landscapeNative, trainPresSites))
			testPresNative <- as.data.frame(extract(landscapeNative, testPresSites))
			testAbsNative <- as.data.frame(extract(landscapeNative, testAbsSites))

			bgSitesTrain <- sampleRast(x=maskNative, n=numBg, adjArea=FALSE, replace=TRUE, prob=FALSE)
			bgEnvTrainNative <- as.data.frame(extract(landscapeNative, bgSitesTrain))

			trainDataNative <- cbind(presBg, rbind(trainPresNative, bgEnvTrainNative))

			testBgSites <- sampleRast(x=maskNative, n=numBg, adjArea=FALSE, replace=TRUE, prob=FALSE)
			testBgNative <- as.data.frame(extract(landscapeNative, testBgSites))

			### compile SAMPLED training/test ENVIRONMENTAL DATA
			trainPresSampled <- as.data.frame(extract(landscapeSampled, trainPresSites))
			testPresSampled <- as.data.frame(extract(landscapeSampled, testPresSites))
			testAbsSampled <- as.data.frame(extract(landscapeSampled, testAbsSites))

			bgEnvTrainSampled <- as.data.frame(extract(landscapeSampled, bgSitesTrain))
			
			trainDataSampled <- cbind(presBg, rbind(trainPresSampled, bgEnvTrainSampled))

			testBgSampled <- as.data.frame(extract(landscapeSampled, testBgSites))

			# prevalence
			prev <- cellStats(speciesMap, 'mean')

			# remember NATIVE data
			sim <- list()
			sim$iter <- iter
			sim$seed <- seed

			sim$stats <- data.frame(numTrainPres=numTrainPres, numTestPres=numTestPres, numBg=numBg, prev=prev, circle=circle, landscapeSize=nrow(landscapeNative), b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
			sim$response <- response
			sim$vars <- names(landscapeNative)
			sim$geography <- geography
			sim$trainData <- trainDataNative
			sim$testData <- list()
			sim$testData$testPres <- testPresNative
			sim$testData$testAbs <- testAbsNative
			sim$testData$testBg <- testBgNative

			class(sim) <- c('sim', class(sim))

			omnibus::dirCreate(simDirNative)
			save(sim, file=paste0(simDirNative, '/', filePrependEndSpace, 'sim ', prefix(iter, 3), '.Rdata'))

			# remember SAMPLED data
			sim <- list()
			sim$iter <- iter
			sim$seed <- seed

			sim$stats <- data.frame(numTrainPres=numTrainPres, numTestPres=numTestPres, numBg=numBg, prev=prev, circle=circle, landscapeSize=nrow(landscapeSampled), b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
			sim$response <- response
			sim$vars <- names(landscapeSampled)
			sim$geography <- geography
			sim$trainData <- trainDataSampled
			sim$testData <- list()
			sim$testData$testPres <- testPresSampled
			sim$testData$testAbs <- testAbsSampled
			sim$testData$testBg <- testBgSampled

			class(sim) <- c('sim', class(sim))

			omnibus::dirCreate(simDirSampled)
			save(sim, file=paste0(simDirSampled, '/', filePrependEndSpace, 'sim ', prefix(iter, 3), '.Rdata'))
			gc()

		} # re-create data?

	} # next simulation

	omnibus::say('')

}
