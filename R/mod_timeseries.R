#' @title mod_timeseries_ui and mod_timeseries_server
#'
#' @description Shiny module
#'
#' @param id shiny id
#'
#' @import shiny

mod_timeseries_ui <- function(id) {
  ns <- NS(id)
  tagList()
}


#' mod_timeseries_server server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_timeseries_ui
#'
mod_timeseries_server <- function(input, output, session) {
  ns <- session$ns
}
