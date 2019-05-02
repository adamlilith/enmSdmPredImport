#' Test importance of interaction terms in Maxent or omniscient model
#' 
#' @param model Either a MaxEnt object or function object.
#' @param vars Character vector, names of variables of which to test interactions.
#' @param sim Object of class \code{sim}.
#' @param perms Positive integer, number of times to permute data.
#' @param testPres Data frame with environment at test presence sites.
#' @param testContrast Data frame with environment at test background or absence sites.
#' @param predPres Predictions at test presences (if \code{NULL} or not supplied then will be computed).
#' @param predContrast Predictions at test background or absence sites (if \code{NULL} or not supplied then will be computed).
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
#' @param ...	Arguments to pass to \code{predictModel} function.
#' @return Data frame with values of AUC (area under the receiver-operator curve), CBI (Continuous Boyce Index), and the Pearson correlation coefficient between predictions from the model with data as-observed and predictions with each variable permuted in turn. Permutations an be performed before the values are multiplied or after they are multiplied.
#' @export

iaImport <- compiler::cmpfun(function(
	model,
	vars,
	sim,
	perms,
	testPres,
	testContrast,
	predPres=NULL,
	predContrast=NULL,
	b0=NA, b1=NA, b2=NA, b11=NA, b12=NA, mu1=NA, mu2=mu2, sigma1=NA, sigma2=NA, rho=NA,
	...
) {

	# predict to observed data (if not supplied)
	if (is.null(predPres)) predPres <- predictModel(model, testPres, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)
	if (is.null(predContrast)) predContrast <- predictModel(model, testContrast, b0=b0, b1=b1, b2=b2, b11=b11, b12=b12, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, rho=rho)

	### test interaction between each pair of variables
	###################################################

	# by FIRST VARIABLE
	for (countVar1 in seq_along(vars)[-length(vars)]) {
	
		var1 <- vars[countVar1]

		# by SECOND VARIABLE
		for (countVar2 in (countVar1 + 1):length(vars)) {
		
			var2 <- vars[countVar2]
		
			# by product permutation RULE
			for (permProdRule in c('before', 'after')) {
				
				# containers for results
				auc <- cbi <- cor <- rep(NA, perms)
		
				# by PERMUTATIION
				for (perm in 1:perms) {
			
					# test presences and test CONTRAST points: permute BEFORE product
					permPresContrast <<- if (class(model)=='MaxEnt') {
						
						enmSdm::predictMaxEnt(
							x=model,
							data=rbind(testPres, testContrast),
							type='cloglog',
							permProd=list(c(var1, var2)),
							permProdRule=permProdRule,
							...
						)
					
					} else if (class(model)=='function') {
					
						predictOmniscientPermIa(
							model=model,
							data=rbind(testPres, testContrast),
							sim=sim,
							permProd=('T1' %in% c(var1, var2) & 'T2' %in% c(var1, var2)),
							permProdRule=permProdRule,
							...
						)
							
					}

					### evaluate
					predPresPerm <- permPresContrast[1:nrow(testPres)]
					predContrastPerm <- permPresContrast[(nrow(testPres) + 1):length(permPresContrast)]

					# NOTE: using ***OBSERVED*** contrast predictions as background
					auc[perm] <- enmSdm::aucWeighted(pres=predPresPerm, bg=predContrast, na.rm=TRUE)
					cbi[perm] <- enmSdm::contBoyce(pres=predPresPerm, bg=predContrast, numBins=101, na.rm=TRUE)
					cor[perm] <- corTest(c(predPres, predContrast), c(predPresPerm, predContrastPerm))
					
				} # by permutation

				# summarize and remember
				auc <- mean(auc, na.rm=TRUE)
				cbi <- mean(cbi, na.rm=TRUE)
				cor <- mean(cor, na.rm=TRUE)
			
				# remember
				thisOut <- data.frame(auc, cbi, cor)

				names(thisOut) <- paste0(names(thisOut), '_perm', omnibus::capIt(permProdRule), paste(vars, collapse=''))
				if (permProdRule == 'before') {
					before <- thisOut
				} else if (permProdRule == 'after') {
					after <- thisOut
				}
				
			} # by product permutation RULE

			# remember
			out <- cbind(before, after)
			
		} # by 2nd variable
		
	} # by first variable
	
	out
	
})
