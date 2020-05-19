#' Evaluate predictor importance of models trained on simulated data
#' 
#' This function is used evaluate the importance of variables in models trained on simulated data. Typical implementation is to use \code{predImportMakeData} to create simulated data sets, then \code{predImportTrainModels} to train SDMs on those data sets, then \code{predImportEval} to evaluate the models.
#' @param simDir Character, path name of directory in which scenario data files are saved.
#' @param modelDir Character, path name of directory in which model files are saved.
#' @param evalDir Character, path name of directory to which to save evaluations.
#' @param algos Character list of model algorithms to evaluate. Options include \code{omniscient}, \code{brt} (boosted regression trees), \code{gam} (generalized additive models), \code{glm} (generalized linear models), \code{maxent} (Maxent, using version 3.3.3k or before), \code{maxnet} (Maxent, version 3.4 or higher), or \code{rf} (random forests).
#' @param type Character, type of models to train. Options include \code{multivariate} (use all variables in \code{vars}, \code{reduced} (a series of models, each using all but one variable in \code{vars}), and/or \code{univariate} (a series of models, one per variable in \code{vars}).
#' @param iters Vector of positive integers, data iterations to evaluate.
#' @param perms Positive integer, number of permutations for permutation tests of variable importance. Default is 30.
#' @param ia Logical, if \code{TRUE} (default) evaluate the importance of interaction terms in Maxent (permutation importance), BRTs (native importance only) and OMNISCIENT (permutation importance) models.
#' @param overwrite Logical, if \code{TRUE} (default) then write over existing evaluation files..
#' @param fileFlag Either \code{NULL} or a character string. If a character string then this is included in the simulated data file name and each model file name. If \code{NULL} (default), nothing is added, so file names will be as "model XXX.RData". If a character string, then the file name will be as "ALGORITHM FLAG model XXX.RData" where "XXX" is the iteration number, "FLAG" the string in \code{fileFlag}, and "ALGORITHM" the model algorithm name.
#' @param userdata Either \code{NULL} or a data frame with extra information a user desires to include in the evaluation data frame output by this function. Note that an argument named \code{userdata} can also be specified in the \code{predImportMakeData} function, which will be stored as part of the \code{sim} object created by that function. This \code{sim}-associated metadata will \emph{also} be included in the evaluation data frame.
#' @param verbose Numeric, if 0 then show minimal output, 1 more output, 2 even more, >2 all of it.
#' @param ... Arguments to pass to \code{\link[enmSdmPredImport]{iaImport}}.
#' @return Nothing (saves a data frame to disc).
#' @seealso \code{\link[enmSdmPredImport]{predImportMakeData}}, \code{\link[enmSdmPredImport]{predImportTrainModels}}
#' @export

predImportEval <- function(
	simDir,
	modelDir,
	evalDir,
	algos=c('omniscient', 'bioclim', 'brt', 'gam', 'glm', 'maxent', 'maxnet', 'rf'),
	type=c('multivariate', 'reduced', 'univariate'),
	iters=1:100,
	perms=30L,
	ia=TRUE,
	overwrite=FALSE,
	fileFlag=NULL,
	userdata=NULL,
	verbose=1,
	...
) {

	# file flag
	fileFlagStartSpace <- if (!is.null(fileFlag)) { paste0(' ', fileFlag) } else { '' }
	fileFlagEndSpace <- if (!is.null(fileFlag)) { paste0(fileFlag, ' ') } else { '' }
	
	omnibus::dirCreate(evalDir)
	
	### by ALGORITHM
	################
	for (algo in algos) {
	
		if (verbose > 0) omnibus::say(date(), ' | Evaluating ', paste(toupper(type), collapse=' '), ' ', toupper(algo), ' models:', post=0)
		
		# if already done and not doing overwrite
		fileName <- paste0(evalDir, '/Evaluations for ', paste(type, collapse=' '), ' ', toupper(algo), fileFlagStartSpace, '.RData')
		fileExists <- file.exists(fileName)
		if (!overwrite & fileExists) {

			if (verbose > 0) omnibus::say('Already done \U2713!', post=0)
		
		# if NOT already done or doing overwrite
		} else {
		
			# output data frame for this algorithm
			perform <- data.frame()
		
			### by ITERATION
			################
			
			for (iter in iters) {
		
				omnibus::say(iter, post=0)
		
				# load simulation data
				simFile <- paste0(simDir, '/', fileFlagEndSpace, 'sim ', omnibus::prefix(iter, 4), '.Rdata')
				load(simFile)
				
				# variable names
				vars <- sim$vars

				# remember statistics
				testPres <- sim$testData$testPres
				testAbs <- sim$testData$testAbs
				testBg <- sim$testData$testBg

				b0 <- sim$stats$b0
				b1 <- sim$stats$b1
				b2 <- sim$stats$b2
				b11 <- sim$stats$b11
				b12 <- sim$stats$b12
				mu1 <- sim$stats$mu1
				mu2 <- sim$stats$mu2
				sigma1 <- sim$stats$sigma1
				sigma2 <- sim$stats$sigma2
				rho <- sim$stats$rho
				
				### REMEMBER
				thisPerform <- data.frame(
					iter = sim$iter,
					algo = algo,
					perms = perms,
					trainDataType = 'presence/background'
				)
				
				thisPerform <- cbind(thisPerform, sim$stats)
				
				# user data
				if (!is.null(userdata)) {
					thisPerform <- cbind(thisPerform, userdata)
				}
				
				# remember stats for landscape
				for (i in seq_along(sim$geography)) {
					
					# add 
					subOut <- data.frame(
						min=sim$geography[[i]]$min,
						max=sim$geography[[i]]$max,
						rot=if (sim$stats$circle) {
							if (exists('rot', where=sim$geography[[i]])) {
								sim$geography[[i]]$rot
							} else {
								0 
							}
						} else {
							NA
						}
					)
					
					names(subOut) <- paste0(names(subOut), names(sim$geography)[i])
					thisPerform <- cbind(thisPerform, subOut)
					
				}
					
				###########################
				### MULTIVARIATE MODELS ###
				###########################

				if ('multivariate' %in% type) {
					
					if (verbose > 1) omnibus::say('multi', post=0)
					
					# load model
					load(paste0(modelDir, '/multivariate ', algo, '/', algo, fileFlagStartSpace, ' model ', omnibus::prefix(iter, 4), '.Rdata'))
					
					# number of training background sites
					modelClass <- class(model)
					
					trainSize <- if (modelClass == 'function') {
						NA
					} else {
						enmSdm::modelSize(model, binary=TRUE)
					}
					
					numTrainBg <- if (!is.na(trainSize[1])) { trainSize[['num0s']] } else { NA }

					thisPerform$numTrainBgMulti <- numTrainBg

					### OBSERVED: presences and background sites
					############################################
					
					if (!is.na(model) && class(model) != 'logical') {

						# compute observed predictions
						predPres <- predictModel(model, testPres, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
						predAbs <- predictModel(model, testAbs, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
						predBg <- predictModel(model, testBg, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)

						### OBSERVED PERFORMANCE
						########################

						aucPresAbs <- enmSdm::aucWeighted(pres=predPres, bg=predAbs, na.rm=TRUE)
						aucPresBg <- enmSdm::aucWeighted(pres=predPres, bg=predBg, na.rm=TRUE)
						cbi <- enmSdm::contBoyce(pres=predPres, bg=predBg, na.rm=TRUE, numBins=1001)

						subOut <- data.frame(aucPresAbs, aucPresBg, cbi)
						names(subOut) <- c('aucPresAbsMulti', 'aucPresBgMulti', 'cbiMulti')
						thisPerform <- cbind(thisPerform, subOut)

						### PERMUTATED VARIABLE IMPORTANCE
						##################################

						if (verbose > 2) omnibus::say('perm', post=0)

						# by EACH VARIABLE
						for (i in seq_along(vars)) {

							thisVar <- vars[i]
							if (verbose > 2) omnibus::say(thisVar, post=0)
							
							aucPresAbsPerm <- aucPresBgPerm <- cbiPerm <- corPresAbsPerm <- corPresBgPerm <- rep(NA, perms)
							
							# by PERMUTATION
							for (perm in 1L:perms) {

								# permute presences/absences
								combo <- statisfactory::sampleAcross(testPres, testAbs, by=thisVar)
								testPresPerm <- combo$testPres
								testAbsPerm <- combo$testAbs

								# permute presences/background
								combo <- statisfactory::sampleAcross(testPres, testBg, by=thisVar)
								testPresPerm <- combo$testPres
								testBgPerm <- combo$testBg

								# compute permuted predictions
								predPresPerm <- predictModel(model, testPresPerm, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
								predAbsPerm <- predictModel(model, testAbsPerm, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
								predBgPerm <- predictModel(model, testBgPerm, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)

								# evaluate
								aucPresAbsPerm[perm] <- enmSdm::aucWeighted(pres=predPresPerm, bg=predAbsPerm, na.rm=TRUE)
								aucPresBgPerm[perm] <- enmSdm::aucWeighted(pres=predPresPerm, bg=predBgPerm, na.rm=TRUE)
								cbiPerm[perm] <- enmSdm::contBoyce(pres=predPresPerm, bg=predBgPerm, na.rm=TRUE, numBins=1001)
								corPresAbsPerm[perm] <- corTest(c(predPres, predAbs), c(predPresPerm, predAbsPerm))
								corPresBgPerm[perm] <- corTest(c(predPres, predBg), c(predPresPerm, predBgPerm))
								
							} # next permutation
							
							# average across permutations
							aucPresAbsPerm <- mean(aucPresAbsPerm, na.rm=TRUE)
							aucPresBgPerm <- mean(aucPresBgPerm, na.rm=TRUE)
							cbiPerm <- mean(cbiPerm, na.rm=TRUE)
							corPresAbsPerm <- mean(corPresAbsPerm, na.rm=TRUE)
							corPresBgPerm <- mean(corPresBgPerm, na.rm=TRUE)
							
							# remember
							subOut <- data.frame(aucPresAbsPerm, aucPresBgPerm, cbiPerm, corPresAbsPerm, corPresBgPerm)
							names(subOut) <- gsub(names(subOut), pattern='Perm', replacement='')
							names(subOut) <- paste0(names(subOut), 'Multi_perm', thisVar)
							
							thisPerform <- cbind(thisPerform, subOut)
						
						} # next variable

					# model failed to converge
					} else {
					
						subOut <- data.frame(aucPresAbs=NA, aucPresBg=NA, cbi=NA)
						names(subOut) <- c('aucPresAbsMulti', 'aucPresBgMulti', 'cbiMulti')
						thisPerform <- cbind(thisPerform, subOut)

						for (thisVar in vars) {
						
							subOut <- data.frame(aucPresAbsPerm=NA, aucPresBgPerm=NA, cbiPerm=NA, corPresAbsPerm=NA, corPresBgPerm=NA)
							names(subOut) <- gsub(names(subOut), pattern='Perm', replacement='')
							names(subOut) <- paste0(names(subOut), 'Multi_perm', thisVar)
							
							thisPerform <- cbind(thisPerform, subOut)

						}
							
					}
						
					### INTERACTION IMPORTANCE
					##########################
					
					if (ia && length(vars) > 1 && algo %in% c('omniscient', 'maxent')) {

						if (verbose > 2) omnibus::say('ia', post=0)
					
						iaTest <- iaImport(
							model=model,
							vars=vars,
							sim=sim,
							perms=perms,
							pres=predPres,
							contrast=predBg,
							testPres=sim$testData$testPres,
							testContrast=sim$testData$testBg,
							...
						)
						
						names(iaTest) <- paste0(names(iaTest), 'Multi')
						thisPerform <- cbind(thisPerform, iaTest)

					}
					
					### NATIVE algorithm importance
					###############################

					# importance in MAXENT
					if (algo == 'maxent') {
						
						if (verbose > 2) omnibus::say('mx:import', post=0)
						
						mxVars <- names(model@presence)
						
						for (count in seq_along(mxVars)) {
					
							thisVar <- mxVars[count]
					
							nativeImp <- data.frame(
								contribution=model@results[paste0(thisVar, '.contribution'), ] / 100,
								permImport=model@results[paste0(thisVar, '.permutation.importance'), ] / 100,
								trainGainWithout=model@results[paste0('Training.gain.without.', thisVar), ] / 100,
								trainGainWithOnly=model@results[paste0('Training.gain.with.only.', thisVar), ] / 100
							)
							
							names(nativeImp) <- c(
								paste0('maxentMultiContrib', thisVar),
								paste0('maxentMultiPermImport', thisVar),
								paste0('maxentMultiTrainGainWithout', thisVar),
								paste0('maxentMultiTrainGainWithOnly', thisVar)
							)
							
							names(nativeImp) <- paste0(names(nativeImp))
							thisPerform <- cbind(thisPerform, nativeImp)

						} # next layer
					
					# importance in BRT
					} else if (algo == 'brt') {
						
						if (verbose > 2) omnibus::say('brt:import', post=0)
						
						if (class(model) != 'logical') {
						
							for (count in seq_along(model$var.names)) {
							
								thisVar <- model$var.names[count]
							
								imp <- if (class(model) != 'logical') {
									model$contributions$rel.inf[which(model$contributions$var == thisVar)] / 100
								} else {
									NA
								}
								
								nativeImp <- data.frame(import = imp)
									
								names(nativeImp) <- paste0('brtMultiNativeImport', thisVar)
								thisPerform <- cbind(thisPerform, nativeImp)
							
							}
							
							## GBM interaction importance
							if (ia) {
								
								if (class(model) != 'logical') {
									brtIa <- dismo::gbm.interactions(model)$interactions
								} else {
									brtIa <- matrix(NA, nrow=2, ncol=2)
									rownames(brtIa) <- colnames(brtIa) <- vars
								}

								for (count1 in 1:(length(vars) - 1)) {
								
									var1 <- vars[count1]
								
									for (count2 in (count1 + 1):length(vars)) {
									
										var2 <- vars[count2]
								
										if (any(rownames(brtIa) %in% var1) & any(colnames(brtIa) %in% var2)) {
									
											nativeImp <- data.frame(DUMMY=brtIa[rownames(brtIa) == var1, colnames(brtIa) == var2])
											nativeImp[1, 1] <- nativeImp[1, 1] / 100
											names(nativeImp) <- paste0('brtMultiNativeIa_', var1, 'x', var2)
											thisPerform <- cbind(thisPerform, nativeImp)
											
										}
								
									}
								
								}
								
							}
							
						# BRT model failed to converge (assumes there has been at least *one* successful evaluation!)
						} else {
						
							# single variables
							for (thisVar in vars) {
							
								missing <- data.frame(DUMMY=NA)
								names(missing) <- paste0('brtMultiNativeImport', thisVar)
								thisPerform <- cbind(thisPerform, missing)
							
							}
							
							# interactions
							if (ia) {
							
								for (thisVar1 in vars[1:(length(vars) - 1)]) {
									for (thisVar2 in vars[2:length(vars)]) {
									
										missing <- data.frame(DUMMY=NA)
										names(missing) <- paste0('brtMultiNativeIa_', thisVar1, 'x', thisVar2)
										thisPerform <- cbind(thisPerform, missing)
									
									}
								
								}
								
							}
						
						} # BRT model failed to converge
						
					} # native algorithm importance
					
				} # if doing multivariate models
				
				######################
				### REDUCED MODELS ###
				######################

				if ('reduced' %in% type) {
					
					if (verbose > 1) omnibus::say('red', post=0)
					
					# load model
					load(paste0(modelDir, '/reduced ', algo, '/', algo, fileFlagStartSpace, ' model ', omnibus::prefix(iter, 4), '.Rdata'))
					
					### OBSERVED: normal
					####################
					
					for (countVar in seq_along(vars)) {

						# number of training background sites
						modelClass <- class(model[[countVar]])
						trainSize <- if (modelClass == 'function') {
							NA
						} else {
							enmSdm::modelSize(model[[countVar]], binary=TRUE)
						}
						
						numTrainBg <- if (!is.na(trainSize[1])) { trainSize[['num0s']] } else { NA }
						
						subOut <- data.frame(numTrainBg=numTrainBg)
						names(subOut) <- paste0('numTrainBgRed_sans', vars[[countVar]])

						thisPerform <- cbind(thisPerform, subOut)
							
						# if model converged
						if (!is.na(model[[countVar]]) && class(model[[countVar]]) != 'logical') {
						
							# compute observed predictions
							predPres <- enmSdmPredImport::predictModel(model[[countVar]], testPres, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
							predAbs <- enmSdmPredImport::predictModel(model[[countVar]], testAbs, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
							predBg <- enmSdmPredImport::predictModel(model[[countVar]], testBg, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)

							# OBSERVED PERFORMANCE
							aucPresAbs <- enmSdm::aucWeighted(pres=predPres, bg=predAbs, na.rm=TRUE)
							aucPresBg <- enmSdm::aucWeighted(pres=predPres, bg=predBg, na.rm=TRUE)
							cbi <- enmSdm::contBoyce(pres=predPres, bg=predBg, na.rm=TRUE, numBins=1001)

							subOut <- data.frame(aucPresAbs, aucPresBg, cbi)
							
						} else {
						
							subOut <- data.frame(aucPresAbs=NA, aucPresBg=NA, cbi=NA)
						
						}
						
						names(subOut) <- c(
							paste0('aucPresAbsRed_sans', vars[countVar]),
							paste0('aucPresBgRed_sans', vars[countVar]),
							paste0('cbiRed_sans', vars[countVar])
						)
						thisPerform <- cbind(thisPerform, subOut)
						
					}
						
				} # if doing reduced model
					
				#########################
				### UNIVARIATE MODELS ###
				#########################

				if ('univariate' %in% type) {
					
					if (verbose > 1) omnibus::say('uni', post=0)
					
					# load model
					load(paste0(modelDir, '/univariate ', algo, '/', algo, fileFlagStartSpace, ' model ', omnibus::prefix(iter, 4), '.Rdata'))
					
					### OBSERVED: normal
					for (countVar in seq_along(model)) {

						thisVar <- names(model)[[countVar]]
						thisVar <- substr(thisVar, 5, nchar(thisVar))

						# number of training background sites
						modelClass <- class(model[[countVar]])
						trainSize <- if (modelClass == 'function') {
							NA
						} else {
							enmSdm::modelSize(model[[countVar]], binary=TRUE)
						}
						
						numTrainBg <- if (!is.na(trainSize[1])) { trainSize[['num0s']] } else { NA }

						subOut <- data.frame(numTrainBg=numTrainBg)
						names(subOut) <- paste0('numTrainBgUni_only', vars[[countVar]])

						thisPerform <- cbind(thisPerform, subOut)
						
						# if model converged
						if (class(model[[countVar]]) != 'logical') {

							# compute observed predictions
							predPres <- enmSdmPredImport::predictModel(model[[countVar]], testPres, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
							predAbs <- enmSdmPredImport::predictModel(model[[countVar]], testAbs, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
							predBg <- predictModel(model[[countVar]], testBg, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)

							# OBSERVED PERFORMANCE
							aucPresAbs <- enmSdm::aucWeighted(pres=predPres, bg=predAbs, na.rm=TRUE)
							aucPresBg <- enmSdm::aucWeighted(pres=predPres, bg=predBg, na.rm=TRUE)
							cbi <- enmSdm::contBoyce(pres=predPres, bg=predBg, na.rm=TRUE, numBins=1001)
						
							subOut <- data.frame(aucPresAbs, aucPresBg, cbi)
						
						} else {

							subOut <- data.frame(aucPresAbs=NA, aucPresBg=NA, cbi=NA)
						
						}
						
						names(subOut) <- c(
							paste0('aucPresAbsUni_only', thisVar),
							paste0('aucPresBgUni_only', thisVar),
							paste0('cbiUni_only', thisVar)
						)
						thisPerform <- cbind(thisPerform, subOut)
						
					} # next variable
					
				} # if doing univariate model

				perform <- rbind(perform, thisPerform)
				rownames(perform) <- 1:nrow(perform)
				if (verbose >= 3) { omnibus::say(''); print(perform) }
			
			} # next ITERATION

			save(perform, file=paste0(evalDir, '/Evaluations for ', paste(type, collapse=' '), ' ', toupper(algo), fileFlagStartSpace, '.RData'))
			rm(perform); gc()
			
		} # if NOT already done or doing overwrite

		if (verbose > 0) omnibus::say('')
		
	} # next ALGORITHM

}
