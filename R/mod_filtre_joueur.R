#' mod_filtre_joueur_ui and mod_filtre_joueur_server
#'
#' @param id shiny id
#' @param fifa2019 fifa 2019 dataframe
#'
#' @import shiny
#' @import shinydashboard

mod_filtre_joueur_ui <- function(id, fifa2019) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 2,
        offset = 5,
        align = "center",
        selectInput(
          inputId = ns("choix_joueur"),
          label = "Select a player",
          choices = setNames(fifa2019$ID, fifa2019$Name)
        )
      )
    )

    # shinydashboard::box(
    #   title = "Player selection",
    #   collapsible = TRUE,
    #   width = 12,
    #   fluidRow(
    #     column(
    #       width = 3,
    #       selectInput(inputId = ns("choix_joueur"),
    #                   label = "Select a player",
    #                   #choices = fifa2019$ID,
    #                   choices = setNames(fifa2019$ID, fifa2019$Name))
    #     )
    #   )
    #
    # )
  )
}


#' mod_filtre_joueur_server
#'
#' @param input internal
#' @param output internal
#' @param sessions internal
#'

mod_filtre_joueur_server <- function(input, output, sessions) {
  return(input)
}
