#' run sattagqlook
#'
#' runs the shiny app sattagqlook
#' @name run_sattagqlook
#' @export
#' @examples
#' \dontrun{
#' run_sattagqlook()
#' }

run_sattagqlook  <- function() {
	shinydir <- system.file("shiny-guts", package = "sattagqlook")
	if(shinydir == "") stop("couldn't find shiny app...")

	shiny::runApp(shinydir, display.mode = "normal", launch.browser = TRUE)
}

