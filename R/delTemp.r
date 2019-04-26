#' Delete temporary directory
#'
#' Delete temporary directory. Useful for running Maxent 3.3.3k or earlier because it creates many temporary files.
#' @param tempDir Character, directory path to delete.
#' @param defaultTempDir Character, default temporary directory (temp directory will be redirected here).
#' @return Nothing (deletes a directory).
#' @export
delTemp <- function(tempDir, defaultTempDir='C:/ecology/!Scratch') {

	Sys.sleep(1)
	raster::rasterOptions(tmpdir=defaultTempDir)
	unlink(tempDir, recursive=TRUE, force=TRUE)

}
