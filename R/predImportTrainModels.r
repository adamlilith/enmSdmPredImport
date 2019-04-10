#' Train SDMs across multiple iterations of a scenario
#'
#' This function trains species distribution models on simulate data. Typical implementation is to use \code{predImportMakeData} to create simulated data sets, then \code{predImportTrainModels} to train SDMs on those data sets, then \code{predImportEval} to evaluate the models.
#' @param simDir Character, path name of directory in which scenario data files are saved.
#' @param modelDir Character, path name of directory in which model files are saved. Depending on whether multivariate, reduced, and/or univariate models are trained, inside this folder will be subfolders named "multivariate", "reduced", and/or "univariate" followed by the name of the algorithm (e.g., "multivariate brt").
#' @param vars Character vector, names of variables to use in model training. These should match the names in the \code{geography} argument supplied to the \code{predImportMakeData} function. See \code{\link[enmSdmPredImport]{genesis}} function for more details on \code{geography}.
#' @param algos Character list of model algorithms. Options include \code{omniscient}, \code{brt} (boosted regression trees), \code{gam} (generalized additive models), \code{glm} (generalized linear models), \code{maxent} (Maxent, using version 3.3.3k or before), or \code{rf} (random forests).
#' @param type Character, type of models to train. Options include \code{multivariate} (use all variables in \code{vars}, \code{reduced} (a series of models, each using all but one variable in \code{vars}), and/or \code{univariate} (a series of models, one per variable in \code{vars}).
#' @param iters Vector of positive integers, data iterations to train models for.
#' @param numBg Positive integer, vector of positive integers, or \code{NULL}. This is the number of background sites used to train the model. If this is \code{NULL} (default), then the number of background sites will be equal to the number of sites available in the "sim" object created by \code{predImportMakeData}. If this is a single integer, then the background sites used for training will be taken from the first \code{numBg} sites available in the "sim" object. If this is a vector, then it must be the same length as \code{algos}, and each algorithm will then be presented with the respective number of background sites. If the latter, the vector must have names that match the algorithm(s) being used.
#' @param filePrepend Either \code{NULL} or a character string. If a character string then this is \emph{pre}-pended to the simulated data file name and each model file name. If \code{NULL} (default), nothing is pre-pended. File names will be as "model XXX.RData" (no append) or "ALGORITHM PREPEND model XXX.RData" where "XXX" is the iteration number, "PREPEND" the string in \code{filePrepend}, and "ALGORITHM" the model algorithm name.
#' @param overwrite Logical, if \code{TRUE} then overwrite existing model results files. Default is \code{FALSE}.
#' @param tempDir Character, path of temporary directory. Used to store ancillary modeling files generated by Maxent 3.3.3k and earlier. Not used for any other modeling algorithm. Maxent can generate \emph{a lot} of these files which eventually fill up a disc. By specifying this folder you can have some control over where they are saved and thus if they fill up a hard drive (e.g., send temp files to a fast secondary drive with lots of space).
#' @param verbose Numeric, if 0 then show minimal output, 1 more output, 2 even more, >2 all of it.
#' @param ... Other arguments to pass to "train~~~" functions in the \pkg{enmSdm} package.
#' @return Nothing (writes models to disc).
#' @seealso \code{\link[enmSdmPredImport]{predImportMakeData}}, \code{\link[enmSdmPredImport]{predImportEval}}
#' @export

predImportTrainModels <- function(
	simDir,
	modelDir,
	vars,
	algos=c('omniscient', 'brt', 'gam', 'glm', 'maxent', 'rf'),
	type=c('multivariate', 'reduced', 'univariate'),
	iters=1:100,
	numBg=NULL,
	filePrepend=NULL,
	overwrite=FALSE,
	tempDir=raster::rasterOptions()$tmpdir,
	verbose=1,
	...
) {

	# file prefix
	fileAppendStartSpace <- if (!is.null(filePrepend)) { paste0(' ', filePrepend) } else { '' }
	fileAppendEndSpace <- if (!is.null(filePrepend)) { paste0(filePrepend, ' ') } else { '' }

	### for each SDM algorithm
	##########################
	
	for (algo in algos) {

		if (verbose >= 0) omnibus::say('Modeling: ', toupper(algo), ' | ', date(), ' |', post=0)

		thisNumBg <- if (is.null(numBg)) {
			NULL
		} else if (length(numBg) == 1) {
			numBg
		} else {
			numBg[[algo]]
		}
	
		# for each iteration sample landscape, train model, and evaluate
		for (iter in iters) {

			if (verbose==1) {
				omnibus::say(iter, post=0)
			} else if (verbose > 1) {
				omnibus::say(date(), ' | ', toupper(algo), ' | simulation ', iter, post=0, pre=1)
			}

			### load training/test data
			###########################
			
			load(paste0(simDir, '/', fileAppendEndSpace, 'sim ', prefix(iter, 3), '.Rdata'))
			
			# select background sites
			if (is.null(thisNumBg)) thisNumBg <- sim$stats$numBg
			
			trainPres <- sim$trainData[sim$trainData$presBg == 1, c('presBg', vars)]
			trainBg <- sim$trainData[sim$trainData$presBg == 0, c('presBg', vars)]
			
			if (thisNumBg > nrow(trainBg)) stop('The "sim" object does not contain the desired number of training background sites (see argument "numBg").')
			
			trainBg <- trainBg[1:thisNumBg, ]
			trainData <- rbind(trainPres, trainBg)
			
			### train MULTIVARIATE models
			#############################
			if ('multivariate' %in% type) {
				
				if (verbose > 0) { omnibus::say('| multi', post=0) }
				
				# if overwriting models is OK, OR if model doesn't exist
				fileMissing <- !file.exists(paste0(modelDir, '/multivariate ', algo, '/', algo, ' ', fileAppendEndSpace, 'model ', prefix(iter, 3), '.RData'))
				
				if (overwrite | fileMissing) {
					
					if (algo=='omniscient') {

						out <- response
						attr(out, 'modelType') <- 'full'

					} else if (algo=='glm') {
					
						out <- enmSdm::trainGlm(
							data=trainData,
							resp='presBg',
							preds=names(trainData)[2:ncol(trainData)],
							construct=FALSE,
							verbose=verbose > 2,
							...
						)

					} else if (algo=='maxent') {
					
						out <- enmSdm::trainMaxEnt(
							data=trainData,
							resp='presBg',
							preds=names(trainData)[2:ncol(trainData)],
							scratchDir=tempDir,
							verbose=(verbose > 2),
							...
						)

					} else if (algo=='brt') {

						set.seed(sim$seed)

						out <- enmSdm::trainBrt(
							data=trainData,
							resp='presBg',
							preds=names(trainData)[2:ncol(trainData)],
							w=TRUE,
							verbose=(verbose > 2),
							...
						)
					
					} else if (algo=='gam') {
			
						out <- enmSdm::trainGam(
							data=trainData,
							resp='presBg',
							preds=names(trainData)[2:ncol(trainData)],
							construct=FALSE,
							verbose=(verbose > 2),
							...
						)
						
					} else if (algo=='rf') {
			
						trainData$presBg <- as.factor(trainData$presBg)

						out <- enmSdm::trainRf(
							data=trainData,
							resp='presBg',
							preds=names(trainData)[2:ncol(trainData)],
							importance=TRUE,
							verbose=(verbose > 2),
							...
						)
						
					}

					model <- if (is.null(out)) {
						FALSE
					} else {
						out
					}

					if (!(algo %in% c('omniscient', 'maxent'))) {
						if (!is.na(model)) model$stats$numTrainBg <- thisNumBg
					}
					
					omnibus::dirCreate(modelDir, '/multivariate ', algo)
					fileName <- paste0(modelDir, '/multivariate ', algo, '/', algo, ' ', fileAppendEndSpace, 'model ', prefix(iter, 3), '.RData')
					save(model, file=fileName)
					rm(model); gc()

				} # if overwriting models OK OR model doesn't exist
				
				if (verbose > 0) omnibus::say('\U2713', post=0)
			
			} # if wanting multivariate models
			
			### train REDUCED models
			########################
			if ('reduced' %in% type) {
				
				if (length(vars) > 2) {
				
					if (verbose > 0) { omnibus::say('red', post=0) }
						
					# if overwriting models OK OR model doesn't exist
					fileMissing <- !file.exists(paste0(modelDir, '/reduced ', algo, '/', algo, fileAppendStartSpace, ' model ', prefix(iter, 3), '.Rdata'))
					if (overwrite | fileMissing) {
						
						model <- list()

						# for EACH variable
						for (count in seq_along(vars)) {

							reducedTrainData <- trainData[ , which(!(names(trainData) %in% vars[count]))]
							
							if (algo=='omniscient') {
							
								out <- response
								attr(out, 'modelType') <- 'reduced'
								attr(out, 'reducedSans') <- names(sim$geography)[count]
							
							} else if (algo=='glm') {
							
								out <- enmSdm::trainGlm(
									data=reducedTrainData,
									resp='presBg',
									preds=names(reducedTrainData)[2:ncol(reducedTrainData)],
									construct=FALSE,
									verbose=verbose > 2,
									...
								)

							} else if (algo=='maxent') {
			
								out <- enmSdm::trainMaxEnt(
									data=reducedTrainData,
									resp='presBg',
									preds=names(reducedTrainData)[2:ncol(reducedTrainData)],
									scratchDir=tempDir,
									verbose=(verbose > 2),
									...
								)
								
							} else if (algo=='brt') {
						
								set.seed(sim$seed)
						
								out <- enmSdm::trainBrt(
									data=reducedTrainData,
									resp='presBg',
									preds=names(reducedTrainData)[2:ncol(reducedTrainData)],
									verbose=(verbose > 2),
									...
								)
							
							} else if (algo=='gam') {

								out <- enmSdm::trainGam(
									data=reducedTrainData,
									resp='presBg',
									preds=names(reducedTrainData)[2:ncol(reducedTrainData)],
									construct=FALSE,
									verbose=(verbose > 2),
									...
								)
							
							} else if (algo=='rf') {

								reducedTrainData$presBg <- as.factor(reducedTrainData$presBg)
							
								out <- enmSdm::trainRf(
									data=reducedTrainData,
									resp='presBg',
									preds=names(reducedTrainData)[2:ncol(reducedTrainData)],
									importance=TRUE,
									verbose=(verbose > 2),
									...
								)
							
							}
						
							model[[count]] <- if (!is.null(out)) {
								out
							} else {
								FALSE
							}
							
							names(model)[[count]] <- paste0('sans', vars[count])
							
							if (!(algo %in% c('omniscient', 'maxent'))) {
								if (!is.na(model[[count]])) model[[count]]$stats$numTrainBg <- thisNumBg
							}

						} # next reduced model
					
						dirCreate(modelDir, '/reduced ', algo)
						fileName <- paste0(modelDir, '/reduced ', algo, '/', algo, fileAppendStartSpace, ' model ', prefix(iter, 3), '.Rdata')
						save(model, file=fileName)
						rm(model); gc()
						
					} # if overwriting models OK OR model doesn't exist

				} # if enough layers to do reduced models
			
				if (verbose > 0) omnibus::say('\U2713', post=0)
			
			} # if wanting reduced models
			
			### train UNIVARIATE models
			###########################
			if ('univariate' %in% type) {
				
				if (verbose > 0) { omnibus::say('uni', post=0) }
				
				# if overwriting models OK OR model doesn't exist
				if (overwrite | !file.exists(paste0(modelDir, '/univariate ', algo, '/', algo, ' ', fileAppendEndSpace, 'model ', prefix(iter, 3), '.RData'))) {

					model <- list()
					
					for (count in seq_along(vars)) {
					
						if (verbose > 1) { omnibus::say(vars[count], post=0) }
					
						univarTrainData <- trainData[ , c('presBg', vars[count])]

						# omniscient model
						if (algo=='omniscient') {
						
							out <- response
							attr(out, 'modelType') <- 'univariate'
							attr(out, 'univarWith') <- names(sim$geography)[count]

						# GLM
						} else if (algo=='glm') {
						
							out <- enmSdm::trainGlm(
								data=univarTrainData,
								resp='presBg',
								preds=names(univarTrainData)[2:ncol(univarTrainData)],
								construct=FALSE,
								select=TRUE,
								verbose=verbose > 2,
								...
							)
								
						# maxent
						} else if (algo=='maxent') {

							out <- enmSdm::trainMaxEnt(
								data=univarTrainData,
								resp='presBg',
								preds=names(univarTrainData)[2:ncol(univarTrainData)],
								scratchDir=tempDir,
								verbose=(verbose > 2),
								...
							)

						### BRTs
						} else if (algo=='brt') {

							set.seed(sim$seed)
						
							out <- enmSdm::trainBrt(
								data=univarTrainData,
								resp='presBg',
								preds=names(univarTrainData)[2:ncol(univarTrainData)],
								w=TRUE,
								verbose=(verbose > 2),
								...
							)

						### GAMs
						} else if (algo=='gam') {

							out <- enmSdm::trainGam(
								data=univarTrainData,
								resp='presBg',
								preds=names(univarTrainData)[2:ncol(univarTrainData)],
								construct=FALSE,
								select=FALSE,
								verbose=(verbose > 2),
								...
							)

						### RFs
						} else if (algo == 'rf') {

							univarTrainData$presBg <- as.factor(univarTrainData$presBg)
						
							out <- enmSdm::trainRf(
								data=univarTrainData,
								resp='presBg',
								preds=names(univarTrainData)[2:ncol(univarTrainData)],
								importance=FALSE,
								verbose=(verbose > 2),
								...
							)

						}
						
						model[[count]] <- if (!is.null(out)) {
							out
						} else {
							FALSE
						}
						
						names(model)[[count]] <- paste0('only', vars[count])

						if (!(algo %in% c('omniscient', 'maxent'))) {
							if (!is.na(model[[count]])) model[[count]]$stats$numTrainBg <- thisNumBg
						}

					} # next univariate model

					dirCreate(modelDir, '/univariate ', algo)
					fileName <- paste0(modelDir, '/univariate ', algo, '/', algo, ' ', fileAppendEndSpace, 'model ', prefix(iter, 3), '.RData')
					save(model, file=fileName)
					rm(model); gc()
					
				} # if overwriting models OK OR model doesn't exist
					
				if (verbose > 0) omnibus::say('\U2713', post=0)
					
			} # if wanting univariate models
			
		} # next iteration

		omnibus::say('')
		
	} # next algorithm

}