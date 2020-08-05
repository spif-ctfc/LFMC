#' Set the LFMC database file
#'
#' Specifies the location of the LFMC database file. This causes setting global variable \code{lfmcdbfile}.
#'
#' @param file A string with the path to a *.sqlite file
#'
#' @examples
#' \dontrun{
#'    setDBpath("../lfmc.sqlite")
#' }
#'
#' @export
#'

setDBpath <- function(file) {
  if(!endsWith(file, ".sqlite")) file = paste0(file, ".sqlite")
  if(file.exists(file)) {
    assign("lfmcdbfile", normalizePath(file), envir = .GlobalEnv)
    cat(paste0("Database file path set to '", lfmcdbfile, "'.\n"))
  } else {
    stop(paste0("Database file '", file, "' does not exist."))
  }
}
