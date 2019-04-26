#' mod_cluster_ui and mod_cluster_server
#'
#' UI and server of the Shiny module to make classification with the fifa19 dataset
#'
#' @param id shiny id
#'
#' @import shinydashboard
#' @import shiny
#'

mod_cluster_ui <- function(id, fifa19_raw) {
  ns <- NS(id)

  tagList(
    fluidRow(
      tags$h3("Supervised classification with Fifa 2019 dataset", style = "padding-left : 15px; font-weight:bold;")
    ),

    splitLayout(
      box(
        title = "Parameters",
        width = 12,
        # status = "primary",
        # background = "light-blue",
        radioButtons(
          inputId = ns("choose_dataset"),
          label = "Choose a dataset",
          choices = c("All attributes", "Simplified attributes"),
          selected = "Simplified attributes",
          inline = FALSE
        ),

        selectInput(
          inputId = ns("choose_response"),
          label = "Choose a response variable",
          choices = "PositionSimplify"
        ),

        uiOutput(ns("filtre_col")),

        tags$hr(),
        tags$hr(),

        radioButtons(
          inputId = ns("choose_model"),
          label = "Choose a classification model",
          choices = c("Random Forest")
        ),

        sliderInput(
          inputId = ns("ratio_train"),
          label = "% of data for training set",
          min = 5,
          max = 95,
          step = 1,
          value = 80
        ),

        actionButton(
          inputId = ns("launch_model"),
          label = "Launch model"
        )
      ),


      verticalLayout(
        box(
          title = "Selected data",
          # status = "primary",
          # solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,

          tags$i("NB : ID, Name, Nationality and Club are not included in the models."),
          DT::dataTableOutput(ns("fifa_table"))
        ),

        conditionalPanel(
          condition = "(input.choose_model == 'Random Forest') && (input.launch_model > 0)",
          ns = ns,
          mod_random_forest_ui(id = ns("RF"), fifa19_raw = fifa19_raw)
        )
      ),
      cellWidths = c("20%", "80%")
    )
  )
}

#' mod_cluster_server
#'
#' @param input internal
#' @param output interlan
#' @param session internal
#' @param fifa19_raw dataset with all skill attributes
#' @param fifa19_simple dataset with simplified skill attributes
#'
#' @import dplyr
#' @import shiny
#' @importFrom randomForest randomForest varImpPlot
#' @importFrom shinyWidgets pickerInput
#'
#' @rdname mod_cluster_ui

mod_cluster_server <- function(input, output, session, fifa19_raw, fifa19_simple) {
  ns <- session$ns

  data_full <- reactive({
    if (input$choose_dataset == "All attributes") {
      dat <- fifa19_raw %>%
        select(
          -Photo, -Flag, -Overall, -Potential, -Club.Logo, -Special,
          -International.Reputation, -Weak.Foot, -Skill.Moves, -Position, -Position,
          -Jersey.Number, -Joined, -Loaned.From, -Contract.Valid.Until, -Release.Clause
        )
    } else {
      dat <- fifa19_simple
    }
    dat
  })


  data_filter <- reactive({
    data_full() %>% select(ID, Name, Nationality, Club, PositionSimplify, input$choose_features)
  })

  output$fifa_table <- DT::renderDT({
    DT::datatable(data_filter(),
      rownames = FALSE,
      options = list(
        pageLength = 5,
        scrollX = TRUE
      )
    )
  })


  output$filtre_col <- renderUI({
    ch <- colnames(data_full())
    ch <- ch[!ch %in% c("ID", "Name", "PositionSimplify", "Nationality", "Club")]
    pickerInput(
      inputId = ns("choose_features"),
      label = "Choose features",
      choices = ch,
      selected = ch,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "Deselect all",
        `select-all-text` = "Select all",
        `none-selected-text` = "zero",
        dropupAuto = FALSE
      )
    )
  })


  observeEvent(input$launch_model, {
    dat <- data_filter()


    if (input$choose_model == "Random Forest") {
      callModule(mod_random_forest_server,
        id = "RF",
        data = dat,
        ratio_train = reactive({
          input$ratio_train
        })
      )
    }
  })
}
