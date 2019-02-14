#' Predict OMNISCIENT, Maxent, BRT, RF, and GAM model objects
#' 
#' Predict OMNISCIENT, Maxent, BRT, RF, and GAM model objects.
#' @param model Model object.
#' @param data Data frame.
#' @param ... Arguments to pass to \code{predict} function.
#' @return Numeric vector.
#' @export

predictModel <- function(model, data, ...) {

	### omniscient model
	####################

	pred <- if (nrow(data) == 0 | 'logical' %in% class(model)) {

		rep(NA, nrow(data))
	
	} else if ('function' %in% class(model)) {
	
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
	} else if ('MaxEnt' %in% class(model)) {
	
		predictMaxEnt(x=model,
			data=data,
			type = 'cloglog',
			...
		)
	
	### standard models
	###################
	} else {
	
		predict(
			object=model,
			x=data,
			newdata=data,
			n.trees=model$gbm.call$best.trees,
			type=ifelse('randomForest' %in% class(model), 'prob', 'response'),
			response=TRUE,
			na.rm=TRUE,
			...
		)
	
	}

	if ('randomForest' %in% class(model)) pred <- pred[ , 2]
	pred
	
}

