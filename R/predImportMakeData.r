#' Master function to create common sets of data for simulations
#'
#' This function is used to create multiple simulated data sets for further analysis. The simulated data represents data that one would typically need for modeling species distributions (i.e., training and test presences, test absences and background sites). Typical implementation is to use \code{predImportMakeData} to create simulated data sets, then \code{predImportTrainModels} to train SDMs on those data sets, then \code{predImportEval} to evaluate the models.
#' @param geography A list of lists describing the simulated environmental layers.  Each sublist pertains to one later. See \code{\link[enmSdmPredImport]{genesis}} for details.
#' @param response A function describing the response of the species to the environment. This must be one of: \code{\link[enmSdmPredImport]{logistic}}, \code{\link[enmSdmPredImport]{logisticShift}}, or \code{\link[enmSdmPredImport]{gaussian}}.
#' @param simDir Character, path name of directory in which scenario data files are saved.
#' @param numTrainPres Positive integer, number of training presences to locate.
#' @param numTestPres Positive integer, number of test presences to locate. The number of test absences will also be equal to this value.
#' @param numBg Positive integer, number of training and number of test background sites to locate.
#' @param iters Vector of positive integers, data iterations to generate.
#' @param circle Logical, if \code{FALSE} (default), all landscapes are square. If \code{TRUE} then landscapes are circular.
#' @param sizeNative Positive integer, size of landscape in number of cells on a side. This specifies the spatial resolution at which the species perceives the landscape. See \emph{Details}.
#' @param sizeResampled Positive integer, size of landscape in number of cells on a side. This specifies the spatial resolution at which environmental data for model calibration and evaluation is available to the "modeler". Note that resampling will thus change the environmental values of the training and test data. See \emph{Details}.
#' @param fileFlag Either \code{NULL} or a character string. If a character string then this is included in the simulated data file name and each model file name. If \code{NULL} (default), nothing is added, so file names will be as "model XXX.RData". If a character string, then the file name will be as "ALGORITHM FLAG model XXX.RData" where "XXX" is the iteration number, "FLAG" the string in \code{fileFlag}, and "ALGORITHM" the model algorithm name.
#' @param userdata Either \code{NULL} (default) or a 1-line data frame to be included as part of the data in the \code{sim} object. This will be included in the evaluation data frame generated by the \code{predImportEval} function. It is useful for specifying aspects of the simulation that are not recorded by default. This metadata will be included in the evaluation data frame created by the \code{prediMportEval} function so will be available for analysis.
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
#' @details In addition to its many capabilities, this function can be used to examine the effects of differences in spatial resolution of the scale at which a species responds to the environment and the scale at which environmental data is available to the modeler. When \code{sizeResampled} is \code{NULL} then the response scale is the same as the scale of environmental data. But when \code{sizeResampled} is not \code{NULL} and different from \code{sizeNative} then the landscape will be resampled to the stated resolution before environmental calibration and evaluation data is extracted. However, presences and absences and background sites will be drawn from the distribution of the true probability of presence generated using the native resolution landscape. Thus resampling to a different resolution maybe (intentionally) "confusing" to model because it wil be presented with data that is not necessarily indicative of the observed state (presence/background).
#' @return Nothing (saves data files to disc).
#' @seealso \code{\link[enmSdmPredImport]{predImportTrainModels}}, \code{\link[enmSdmPredImport]{predImportEval}}
#' @export

predImportMakeData <- function(
	geography,
	response,
	simDir,
	numTrainPres=200,
	numTestPres=200,
	numBg=10000,
	iters=1:100,
	circle=FALSE,
	sizeNative=1024,
	sizeResampled=NULL,
	fileFlag=NULL,
	userdata=NULL,
	b0=NA, b1=NA, b2=NA, b11=NA, b12=NA, mu1=NA, mu2=mu2, sigma1=NA, sigma2=NA, rho=NA,
	overwrite=FALSE,
	verbose=1,
	...
) {

	if (verbose >= 0) omnibus::say(date(), ' | Creating simulation data for ', max(iters), ' simulations:', post=0)

	# user data
	if (!is.null(userdata)) {
		if (!('data.frame' %in% class(userdata))) stop('Argument "userdata" must be "NULL" or a data frame with one row.')
		if (nrow(userdata) != 1) stop('Argument "userdata" must be "NULL" or a data frame with one row.')
	}
	
	# file prefix
	filePrependEndSpace <- if (!is.null(fileFlag)) { paste0(fileFlag, ' ') } else { '' }

	# resampling resolution
	if (is.null(sizeResampled)) sizeResampled <- sizeNative
	
	# geography unlisted form
	geogUnlist <- unlist(geography)
	geogHasRandom <- any(geogUnlist %in% 'random')
	geogHasNoise <- any(grepl(names(geogUnlist), pattern='noise'))

	# for each simulation sample landscape, train model, and evaluate
	for (iter in iters) {

		# DO NOT re-create data
		simFileExists <- file.exists(paste0(simDir, '/', filePrependEndSpace, 'sim ', omnibus::prefix(iter, 3), '.Rdata'))

		if (!overwrite && simFileExists) {

			omnibus::say(iter, '\U2713', post=0)

		# RE-CREATE DATA
		} else {

			if (verbose > 0) omnibus::say(iter, post=0)

			### generate landscape and species
			##################################

			# seed <- iter
			# set.seed(seed)

			# make landscape and species
			if (!exists('landscapeNative', inherits=FALSE) | geogHasRandom | geogHasNoise) {

				landscapeNative <- genesis(geography, circle=circle, size=sizeNative, verbose=verbose > 1)
				
				### resample landscape
				if (sizeNative == sizeResampled) {
					landscapeResampled <- landscapeNative
				} else {
					
					# rescale landscape to "SAMPLED" scale at which predictors are available
					templateSampled <- raster::raster(
						nrows=sizeResampled,
						ncols=sizeResampled,
						crs=raster::projection(landscapeNative),
						ext=raster::extent(landscapeNative)
					)
					
					landscapeResampled <- raster::resample(landscapeNative, templateSampled, ...)

				}
				
				# map of species' probability of occurrence
				args <- if (attr(response, 'equationType') == 'logistic') {
					list(x1=landscapeNative[[1]], x2=landscapeNative[[2]], b0=b0, b1=b1, b2=b2, b12=b12)
				} else if (attr(response, 'equationType') == 'logisticShift') {
					list(x1=landscapeNative[[1]], x2=landscapeNative[[2]], b0=b0, b1=b1, b11=b11)
				} else if (attr(response, 'equationType') == 'gaussian') {
					list(x1=landscapeNative[[1]], x2=landscapeNative[[2]], mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
				}
					
				speciesMap <- do.call(response, args=args)
				
			}			

			### generate training/test and background sites
			###############################################

			# create mask from which to sample (avoids inclusion of sites with NAs)
			mask <- sum(landscapeResampled)

			# estimate sample size needed to get sufficient presences and absences
			n <- round(1.5 * (numTrainPres + numTestPres)) # initial number of randomly located sites to draw
			prev <- raster::cellStats(speciesMap, 'sum')  / raster::ncell(speciesMap) # prevalence (including NA cells)

			while (n * prev < numTrainPres + numTestPres | n * (1 - prev) < numTestPres) { n <- round(n * 1.5) }

			# draw sites
			presAbs <- -Inf # initial sum of sampled presences
			while (sum(presAbs) < numTrainPres + numTestPres | sum(!presAbs) < numTestPres) {

				sites <- enmSdm::sampleRast(x=mask, n=n, adjArea=FALSE, replace=TRUE, prob=FALSE)
				prOcc <- raster::extract(speciesMap, sites)
				presAbs <- stats::runif(nrow(sites)) <= prOcc

				n <- 1.5 * n

			}

			# training/test presences and absences
			allPresSites <- sites[which(presAbs), ]
			trainPresSites <- allPresSites[1:numTrainPres, ]
			testPresSites <- allPresSites[(numTrainPres + 1):(numTrainPres + numTestPres), ]
			testAbsSites <- sites[which(!presAbs), ]
			testAbsSites <- testAbsSites[sample(1:nrow(testAbsSites), numTestPres), ]

			trainPres <- as.data.frame(raster::extract(landscapeResampled, trainPresSites))
			testPres <- as.data.frame(raster::extract(landscapeResampled, testPresSites))
			testAbs <- as.data.frame(raster::extract(landscapeResampled, testAbsSites))

			# training/test random background sites
			trainBgSites <- enmSdm::sampleRast(x=mask, n=numBg, adjArea=FALSE, replace=TRUE, prob=FALSE)
			trainBgEnv <- as.data.frame(raster::extract(landscapeResampled, trainBgSites))

			# compile training data
			trainPresBg <- data.frame(presBg=c(rep(1, nrow(trainPres)), rep(0, nrow(trainBgEnv))))
			trainData <- cbind(trainPresBg, rbind(trainPres, trainBgEnv))

			# compile test data
			testBgSites <- enmSdm::sampleRast(x=mask, n=numBg, adjArea=FALSE, replace=TRUE, prob=FALSE)
			testBg <- as.data.frame(raster::extract(landscapeResampled, testBgSites))

			# prevalence
			prev <- raster::cellStats(speciesMap, 'mean')

			# remember
			sim <- list()
			sim$iter <- iter
			# sim$seed <- seed

			stats <- data.frame(
				numTrainPres=numTrainPres,
				numTestPres=numTestPres,
				numBg=numBg,
				prev=prev,
				circle=circle,
				sizeNative=sizeNative,
				sizeResampled=sizeResampled,
				b0=b0, b1=b1, b2=b2, b11=b11, b12=b12,
				mu1=mu1, mu2=mu2,
				sigma1=sigma1, sigma2=sigma2,
				rho=rho
			)

			if (!is.null(userdata)) stats <- cbind(stats, userdata)
			
			sim$stats <- stats
			sim$response <- response
			sim$vars <- names(landscapeNative)
			sim$geography <- geography
			sim$trainData <- trainData
			sim$testData <- list()
			sim$testData$testPres <- testPres
			sim$testData$testAbs <- testAbs
			sim$testData$testBg <- testBg

			class(sim) <- c('sim', class(sim))

			omnibus::dirCreate(simDir)
			save(sim, file=paste0(simDir, '/', filePrependEndSpace, 'sim ', omnibus::prefix(iter, 3), '.Rdata'))
			gc()

		} # re-create data?

	} # next simulation

	omnibus::say('')

}
