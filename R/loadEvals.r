#' Load evaluation files and compile into a single data frame
#'
#' This is a helper function that loads multiple evaluation files and compiles them into a single data frame. The data frame is then sorted by the order of the algorithms in the argument \code{algo}.
#' @param scenarioDir Character, name of the \emph{scenario} directory in which a folder named "evaluations" is found containing evaluation files.
#' @param algos Character, name of algorithm(s) for which to collate evaluation files. Examples: \code{c('omniscient', 'brt', 'gam', 'maxent')}. Algorithms will appear in this order in the data frame output.
#' @param save Logical, if \code{TRUE} then save collated data frame in the evaluation folder. The name of the file will be "!Collated Evaluations.RData".
#' @param redo Logical, if \code{TRUE} and a file named "!Collated Evaluations.RData" is already in the evaluations folder, then this function will load that file instead of re-collating individual evaluation files. If \code{FALSE}, then re-collate evaluation files.
#' @param verbose Logical, if \code{TRUE} then display progress.
#' @return Data frame.
#' @export

loadEvals <- function(scenarioDir, algos=c('omniscient', 'gam', 'maxent', 'brt'), save=TRUE, redo=FALSE, verbose=TRUE) {

	# loads evaluation files and compiles them into a single data frame

	if (!redo & file.exists(paste0(scenarioDir, '/evaluations/!Collated Evaluations.RData'))) {
	
		load(paste0(scenarioDir, '/evaluations/!Collated Evaluations.RData'))
		
	} else {
		
		for (algo in algos) {

			omnibus::say(algo)
			files <- listFiles(paste0(scenarioDir, '/evaluations'), pattern=toupper(algo))

			for (file in files) {

				if (verbose) omnibus::say(file)
				load(file)
				if (!exists('perform', inherits=FALSE)) stop('File does not contain a variable named "perform."')
				if (any(!(perform$algo %in% algo))) stop(paste('Algorithm listed in perform data frame is not the intended algorithm (data frame is for', perform$algo[1], ' (intended algorithm is', algo, ').'))
				master <- if (!exists('master', inherits=FALSE)) {
					master <- perform
				} else {
					merge(master, perform, all=TRUE)
				}
				rm(perform)

			}

		}

		master$algo <- as.character(master$algo)

		# sort
		ranks <- rep(NA, nrow(master))
		for (i in seq_along(algos)) {
			ranks[master$algo == algos[i]] <- i
		}
		master <- master[order(ranks), ]

		if (save) save(master, file=paste0(scenarioDir, '/evaluations/!Collated Evaluations.RData'))
		
	}
		
	master

}
