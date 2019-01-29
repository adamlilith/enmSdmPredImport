#' Load evaluation files and compile into a single data frame
#'
#' This is a helper function that loads multiple evaluation files and compiles them into a single data frame.
#' @param outDir Character, name of the \emph{scenario} directory in which a folder named "evaluations" is found containing evaluation files.
#' @param algos Character, name of algorithm(s) for which to collate evaluation files. Examples: \code{c('omniscient', 'brt', 'gam', 'maxent')}.
#' @return Data frame.
#' @export

loadEvals <- function(outDir, algos=c('omniscient', 'brt', 'gam', 'maxent')) {

	# loads evaluation files and compiles them into a single data frame
	
	for (algo in algos) {
		omnibus::say(algo)
		files <- listFiles(paste0(outDir, '/evaluations'), pattern=toupper(algo))
		algoPerform <- data.frame()
		
		for (file in files) {
			omnibus::say(file)
			load(file)
			if (!exists('perform', inherits=FALSE)) stop('File does not contain a variable named "perform."')
			if (any(!(perform$algo %in% algo))) stop(paste('Algorithm listed in perform data frame is not the intended algorithm (data frame is for', perform$algo[1], 'but intended algorithm is', algo, '.'))
			algoPerform <- rbind(algoPerform, perform)
		}
		
		masterPerform <- if (exists('masterPerform', inherits=FALSE)) {
			merge(masterPerform, algoPerform, all=TRUE)
		} else {
			algoPerform
		}
		
	}

	masterPerform <- masterPerform[orderAlgos(masterPerform$algo), ]
	masterPerform
	
}
