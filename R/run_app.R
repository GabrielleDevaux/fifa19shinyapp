
#' run_app
#'
#' @import shiny
#'
#' @export
run_app <- function() {
  # shinyApp(ui = app_ui(), server = app_server)
  shiny::runApp(appDir = system.file("app", package = "fifa2019demo", mustWork = TRUE))
}
