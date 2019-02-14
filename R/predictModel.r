#' Predict OMNISCIENT, Maxent, BRT, RF, and GAM model objects
#' 
#' Predict OMNISCIENT, Maxent, BRT, RF, and GAM model objects.
#' @param model Model object.
#' @param data Data frame.
#' @param ... Arguments to pass to \code{predict} function.
#' @return Numeric vector.
#' @export

predictModel <- function(model, data, ...) {

	modelClass <- class(model)

	### omniscient model
	####################

	pred <- if (nrow(data) == 0 | 'logical' %in% modelClass) {

		rep(NA, nrow(data))
	
	} else if ('function' %in% modelClass) {
	
		### full model
		if (attr(model, 'modelType')=='full') {
			
			model(
				x1=if (!any(grepl(names(data), pattern='T1'))) { rep (0, nrow(data)) } else { data[ , which(grepl(names(data), pattern='T1'))] },
				x2=if (!any(grepl(names(data), pattern='T2'))) { rep (0, nrow(data)) } else { data[ , which(grepl(names(data), pattern='T2'))] },
				...
			)
			
		### reduced model
		} else if (attr(model, 'modelType')=='reduced') {
		
			if (grepl(attr(model, 'reducedSans'), pattern='T1')) {
				
				model(
					x1=rep(0, nrow(data)),
					x2=if (!any(grepl(names(data), pattern='T2'))) { rep (0, nrow(data)) } else { data[ , which(grepl(names(data), pattern='T2'))] },
					...
				)
				
			} else if (grepl(attr(model, 'reducedSans'), pattern='T2')) {
			
				model(
					x1=if (!any(grepl(names(data), pattern='T1'))) { rep (0, nrow(data)) } else { data[ , which(grepl(names(data), pattern='T1'))] },
					x2=rep(0, nrow(data)),
					...
				)
			
			} else {
			
				model(
					x1=if (!any(grepl(names(data), pattern='T1'))) { rep (0, nrow(data)) } else { data[ , which(grepl(names(data), pattern='T1'))] },
					x2=if (!any(grepl(names(data), pattern='T2'))) { rep (0, nrow(data)) } else { data[ , which(grepl(names(data), pattern='T2'))] },
					...
				)
				
			}
		
		### univariate model
		} else if (attr(model, 'modelType')=='univariate') {
		
			if (grepl(attr(model, 'univarWith'), pattern='T1')) {
				
				model(
					x1=if (!any(grepl(names(data), pattern='T1'))) { rep (0, nrow(data)) } else { data[ , which(grepl(names(data), pattern='T1'))] },
					x2=rep(0, nrow(data)),
					...
				)
				
			} else if (grepl(attr(model, 'univarWith'), pattern='T2')) {

				model(
					x1=rep(0, nrow(data)),
					x2=if (!any(grepl(names(data), pattern='T2'))) { rep (0, nrow(data)) } else { data[ , which(grepl(names(data), pattern='T2'))] },
					...
				)
				
			} else {
			
				model(
					x1=rep(0, nrow(data)),
					x2=rep(0, nrow(data)),
					...
				)
				
			}
		
		}
	
	### Maxent models
	#################
	} else if ('MaxEnt' %in% modelClass) {
	
		enmSdm::predictMaxEnt(x=model,
			data=data,
			type='cloglog',
			...
		)
	
	### BRTs
	###################
	} else if ('gbm' %in% modelClass) {
	
		gbm::predict.gbm(
			model,
			data,
			n.trees=model$gbm.call$best.trees,
			response=TRUE,
			na.rm=TRUE,
			...
		)
	
	### standard models
	###################
	} else {
	
		predict(
			object=model,
			x=data,
			newdata=data,
			type=ifelse('randomForest' %in% modelClass, 'prob', 'response'),
			response=TRUE,
			na.rm=TRUE,
			...
		)
	
	}

	if ('randomForest' %in% modelClass) pred <- pred[ , 2]
	pred
	
}

