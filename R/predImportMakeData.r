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
#' @param size Positive integer, size of landscape in number of cells on a side.
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
#' @return Nothing (saves data files to disc).
#' @seealso \code{\link[enmSdmPredImport]{predImportMakeDataRes}}, \code{\link[enmSdmPredImport]{predImportTrainModels}}, \code{\link[enmSdmPredImport]{predImportEval}}
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
	size=1001,
	filePrepend=NULL,
	b0=NA, b1=NA, b2=NA, b11=NA, b12=NA, mu1=NA, sigma1=NA, sigma2=NA, rho=NA,
	overwrite=FALSE,
	verbose=1,
	...
) {

	if (verbose >= 0) omnibus::say(date(), ' | Creating simulation data for ', max(iters), ' simulations:', post=0)

	# file prefix
	# fileAppendStartSpace <- if (!is.null(filePrepend)) { paste0(' ', filePrepend) } else { '' }
	filePrependEndSpace <- if (!is.null(filePrepend)) { paste0(filePrepend, ' ') } else { '' }

	# flag to indicate if any data creation iterations were skipped
	skippedAny <- FALSE

	# for each simulation sample landscape, train model, and evaluate
	for (iter in iters) {

		# DO NOT re-create data
		simFileExists <- file.exists(paste0(simDir, '/', filePrependEndSpace, 'sim ', prefix(iter, 3), '.Rdata'))
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

				landscape <<- genesis(geography, circle=circle, size=size, verbose=verbose > 1)

				# map of species' probability of occurrence
				args <- if (attr(response, 'equationType') == 'logistic') {
					list(x1=landscape[[1]], x2=landscape[[2]], b0=b0, b1=b1, b2=b2, b12=b12)
				} else if (attr(response, 'equationType') == 'logisticShift') {
					list(x1=landscape[[1]], x2=landscape[[2]], b0=b0, b1=b1, b11=b11)
				} else if (attr(response, 'equationType') == 'gaussian') {
					list(x1=landscape[[1]], x2=landscape[[2]], mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
				}
					
			# re-make any random layers for next sim
			} else if (any(unlist(geography) %in% c('random', 'noisy'))) {
				landscape <<- genesis(geography, circle=circle, size=size)
				speciesMap <- do.call(response, args=args)
			}
			
			if (!exists('speciesMap', inherits=FALSE)) speciesMap <- do.call(response, args=args)

			### generate training/test and background sites
			###############################################

			# create mask from which to sample (avoids inclusion of sites with NAs)
			mask <- sum(landscape)

			# estimate sample size needed to get sufficient presences and absences
			n <- round(1.5 * (numTrainPres + numTestPres)) # initial number of randomly located sites to draw
			prev <- cellStats(speciesMap, 'sum')  / ncell(speciesMap) # prevalence (including NA cells)

			while (n * prev < numTrainPres + numTestPres | n * (1 - prev) < numTestPres) { n <- round(n * 1.5) }

			# draw sites
			presAbs <- -Inf # initial sum of sampled presences
			while (sum(presAbs) < numTrainPres + numTestPres | sum(!presAbs) < numTestPres) {

				sites <- sampleRast(x=mask, n=n, adjArea=FALSE, replace=TRUE, prob=FALSE)
				prOcc <- extract(speciesMap, sites)
				presAbs <- runif(nrow(sites)) <= prOcc

				n <- 1.5 * n

			}

			# training/test presences and absences
			allPresSites <- sites[which(presAbs), ]
			trainPresSites <- allPresSites[1:numTrainPres, ]
			testPresSites <- allPresSites[(numTrainPres + 1):(numTrainPres + numTestPres), ]
			testAbsSites <- sites[which(!presAbs), ]
			testAbsSites <- testAbsSites[sample(1:nrow(testAbsSites), numTestPres), ]

			trainPres <- as.data.frame(extract(landscape, trainPresSites))
			testPres <- as.data.frame(extract(landscape, testPresSites))
			testAbs <- as.data.frame(extract(landscape, testAbsSites))

			# training/test random background sites
			bgSitesTrain <- sampleRast(x=mask, n=numBg, adjArea=FALSE, replace=TRUE, prob=FALSE)
			bgEnvTrain <- as.data.frame(extract(landscape, bgSitesTrain))

			# compile training data
			presBg <- data.frame(presBg=c(rep(1, nrow(trainPres)), rep(0, nrow(bgEnvTrain))))
			trainData <- cbind(presBg, rbind(trainPres, bgEnvTrain))

			# compile test data
			testBgSites <- sampleRast(x=mask, n=numBg, adjArea=FALSE, replace=TRUE, prob=FALSE)
			testBg <- as.data.frame(extract(landscape, testBgSites))

			# prevalance
			prev <- cellStats(speciesMap, 'mean')

			# remember
			sim <- list()
			sim$iter <- iter
			sim$seed <- seed

			sim$stats <- data.frame(numTrainPres=numTrainPres, numTestPres=numTestPres, numBg=numBg, prev=prev, circle=circle, landscapeSize=nrow(landscape), b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
			sim$response <- response
			sim$vars <- names(landscape)
			sim$geography <- geography
			sim$trainData <- trainData
			sim$testData <- list()
			sim$testData$testPres <- testPres
			sim$testData$testAbs <- testAbs
			sim$testData$testBg <- testBg

			class(sim) <- c('sim', class(sim))

			omnibus::dirCreate(simDir)
			save(sim, file=paste0(simDir, '/', filePrependEndSpace, 'sim ', prefix(iter, 3), '.Rdata'))
			gc()

		} # re-create data?

	} # next simulation

	omnibus::say('')

}
