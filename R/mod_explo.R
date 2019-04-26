#' mod_explo_ui and mod_explo_server
#'
#' @param id shiny id
#'
#' @import shiny
#' @¶import shinydashboard
mod_explo_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$br(),
    box(
      title = "Fifa 2019 Kaggle dataset",
      width = 12,

      # tags$p("This dataset includes lastest edition FIFA 2019 players attributes. Raw data is available",
      #        tags$a(
      #          target = "_blank", rel = "noopener noreferrer",
      #          href = "https://www.kaggle.com/karangadiya/fifa19",
      #          "here.")),
      # tags$p("This Shiny app aims at making football analytics over 18147 unique players, represented
      #        with around 60 attributes such as their age, height, weight, speed ..."),

      # tags$br(),
      div(DT::dataTableOutput(outputId = ns("fifa_table")),
        style = "font-size: 90%; white-space:nowrap;"
      )
    ),
    fluidRow("")
  )
}


#' mod_explo_server
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @import shiny
#'
#' @rdname mod_explo_ui

mod_explo_server <- function(input, output, session) {
  ns <- session$ns

  output$fifa_table <- DT::renderDT({
    fifa <- fifa2019_raw %>% select(-Photo, -Flag, -Club.Logo)
    DT::datatable(fifa,
      rownames = FALSE,
      filter = "top",
      selection = "single",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(list(className = "dt-right", targets = "_all"))
      )
    )
  })
}
