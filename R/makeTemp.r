#' Make temporary directory
#'
#' Make a temporary directory.
#' @param path Character name of path.
#' @return Path name of temporary directory plus creates the directory and assigns the raster temporary directory to this directory.
#' @export

makeTemp <- function(path='C:/ecology/!Scratch/_scratchDir_dontDelete') {

	tempDir <- paste0('/_temp', round(10^7 * stats::runif(1)))
	dir.create(path=tempDir, showWarnings=FALSE, recursive=TRUE)
	raster::rasterOptions(tmpdir=tempDir)
	tempDir
	
}

