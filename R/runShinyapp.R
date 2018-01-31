#' Shiny Apps: Find and launch apps bundled with bulletr
#' @param name Name of the shiny app to be launched
#' @export
#' @examples
#' \dontrun{
#' bulletr::runShinyapp('wrApp')
#' }
#'
runShinyapp <- function(name) {
  # locate all the shiny apps that exist
  validNames <- list.files(system.file("shiny-examples", package = "bulletr"))
  
  validNamesMsg <-
    paste0(
      "Valid Name apps for bulletr are: '",
      paste(validNames, collapse = "', '"),
      "'")
  
  # if an invalid Name is given, throw an error
  if (missing(name) || !nzchar(name) ||
      !name %in% validNames) {
    stop(
      'Please run `runShinyapp()` with a valid app name as an argument.\n',
      validNamesMsg,
      call. = FALSE)
  }
  
  # find and launch the app
  appDir <- system.file("shiny-examples", name, package = "bulletr")
  shiny::runApp(appDir, display.mode = "normal")
}