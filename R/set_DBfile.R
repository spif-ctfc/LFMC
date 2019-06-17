#' Specifies the LFMC database file
#'
#' @param file A string with the path to a *.sqlite file
#'
#' @return
#' @export
#'
#' @examples
set_DBpath<-function(file){
  if(file.exists(file)) {
    assign("lfmcdbfile", normalizePath(file), envir = .GlobalEnv)
    cat(paste0("Database file path set to '", lfmcdbfile, "'.\n"))
  } else {
    stop(paste0("Database file '", file, "' does not exist."))
  }
}
