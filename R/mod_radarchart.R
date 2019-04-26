
#' mod_radarchart_ui and mod_radarchart_server
#'
#' mod_radarchart_ui and mod_radarchart_server zzz
#'
#' @param id shiny id
#'
#' @import shiny
#' @import shinydashboard

mod_radarchart_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "display:inline-block; width:20%;",
      plotOutput(ns("skill_radar"), height = "200px")
    )
  )
}


#' mod_radarchart_server
#'
#'
#' @param input internal
#' @param output internal
#' @param session internal
#' @param joueur data frame of 1 line
#' @param vector_of_skills list of 2 elements : title and features
#' @param data data
#'
#' @import magrittr
#' @import dplyr
#' @import shiny
#' @importFrom fmsb radarchart
#' @importFrom graphics par
#' @importFrom grDevices rgb
#'
#' @rdname mod_radarchart_ui
mod_radarchart_server <- function(input, output, session, joueur, vector_of_skills, data = NULL) {
  ns <- session$ns

  output$skill_radar <- renderPlot({
    plot_radar(
      player = joueur(), vector_of_skills = vector_of_skills, data_compare = data()$raw,
      color = COLOR
    )
  })
}
