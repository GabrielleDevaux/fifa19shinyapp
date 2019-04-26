#' @title mod_cv_joueur_ui and mod_cv_joueur_server
#'
#' @description Shiny module to display a football player CV
#'
#' @param id shiny id
#'
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
mod_cv_joueur_ui <- function(id, fifa19) {
  ns <- NS(id)
  tagList(
    box(
      title = "Player informations",
      width = 12,

      fluidRow(
        column(
          width = 3,
          div(
            style = "display: inline-block;vertical-align:top; width: 90px;",
            htmlOutput(outputId = ns("photo_joueur"))
          ),
          div(
            style = "display: inline-block;vertical-align:top;",
            htmlOutput(outputId = ns("infos_generales"))
          )
        ),
        column(
          width = 3,
          htmlOutput(outputId = ns("infos_generales_2"))
        ),

        column(
          width = 3,
          htmlOutput(outputId = ns("infos_generales_3"))
        ),

        column(
          width = 3,
          div(
            style = "display: inline-block;vertical-align:top; width: 40px;",
            htmlOutput(outputId = ns("photo_club"))
          ),
          div(
            style = "display: inline-block;vertical-align:top;",
            htmlOutput(outputId = ns("infos_club"))
          )
        )
      ),

      tags$hr(),

      fluidRow(
        column(
          width = 4,
          # align = 'center',
          plotOutput(ns("skills_plot"), height = "300px"),
          tags$br(),
          fluidRow(
            column(
              width = 4,
              offset = 4,
              materialSwitch(
                inputId = ns("show_radars"),
                label = "Show details"
              )
            )
          )
        ),

        column(
          width = 3,
          plotOutput(ns("plots_legend"), height = "300px")
        ),

        column(
          width = 5,
          # tags$b(style = "text-align:center;font-size:16px;","Titre des boxplots"),
          plotOutput(ns("cv_boxplot"), height = "300px"),
          tags$br(),
          fluidRow(
            column(
              width = 4,
              offset = 4,
              ""
              # materialSwitch(
              #   inputId = ns("show_custom_boxplot"),
              #   label = "Custom boxplot"
              # )
            )
          )
        )
        # )
      ),

      fluidRow(
        column(
          width = 7,
          offset = 5,
          # align = "center",
          radioButtons(
            inputId = ns("boxplot_compare_with"),
            label = "Compare with players of same ",
            choices = c("Position", "Club", "All"),
            selected = "All",
            inline = TRUE
          )
        )
      ),

      # conditionalPanel(
      #   condition = "input.show_custom_boxplot == true",
      #   ns = ns,
      #   tags$hr(),
      #   fluidRow(
      #     "make custom boxplot here"
      #   )
      # ),

      conditionalPanel(
        condition = "input.show_radars==true",
        ns = NS(id),
        tags$hr(),

        tags$div(
          style = "text-align:center;",

          lapply(
            X = paste0("skill", 1:7), # ns id
            FUN = function(x) {
              mod_radarchart_ui(id = ns(x))
            }
          )
        )
      ),


      tags$br()
    ),

    fluidRow(
      ""
    )
  )
}


#' mod_cv_joueur_server server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#' @param fifa19 fifa 2019 dataframe
#' @param fifa19_simple simplified df
#' @param filtre_inputs inputs from mod_filtre_joueur module
#'
#' @import magrittr
#' @import dplyr
#' @import shiny
#' @importFrom graphics par
#' @importFrom grDevices rgb
#'
#' @rdname mod_cv_joueur_ui
mod_cv_joueur_server <- function(input, output, session, fifa19, fifa19_simple, filtre_inputs) {
  ns <- session$ns

  current_player <- reactive({
    fifa19 %>% filter(ID == filtre_inputs$choix_joueur)
  })

  output$infos_generales <- renderUI({
    p <- current_player()
    HTML(paste(
      tags$b(paste0(p$Name), style = "font-size:20px"), # , ' (ID:', p$ID,')'
      paste(
        paste0("Age ", p$Age, ", ", p$Height, " cm, ", p$Weight, " kg"),
        paste0("Nationality : ", p$Nationality),
        paste0("Position : ", p$PositionSimplify, ", ", p$Position),
        paste0("Preferred foot : ", p$Preferred.Foot),
        sep = "<br/>"
      ),
      sep = "<br/>"
    ))
  })

  output$infos_generales_3 <- renderUI({
    p <- current_player()
    HTML(paste(
      tags$b(" ", style = "font-size:20px"),
      paste0("Wage : \u20ac", format(p$Wage, big.mark = " ", scientific = FALSE)),
      paste0("Value : \u20ac", format(p$Value, big.mark = " ", scientific = FALSE)),
      paste0("Release clause : \u20ac", format(p$Release.Clause, big.mark = " ", scientific = FALSE)),
      sep = "<br/>"
    ))
  })


  output$infos_generales_2 <- renderUI({
    p <- current_player()
    HTML(paste(
      tags$b(" ", style = "font-size:20px"),
      paste0("Overall rating : ", p$Overall),
      paste0("Potential : ", p$Potential),
      paste0("Jersey number : ", p$Jersey.Number),
      paste0("Work rate : ", p$Work.Rate),
      sep = "<br/>"
    ))
  })

  output$photo_joueur <- renderUI({
    img(src = current_player()$Photo, alt = "Cannot display image", width = "90px")
  })


  output$photo_club <- renderUI({
    img(src = current_player()$Club.Logo, alt = "Cannot display image", width = "40px")
  })

  output$infos_club <- renderUI({
    p <- current_player()
    HTML(paste(
      tags$b(paste("Club :", p$Club), style = "font-size:16px"),
      paste0("Size : ", nrow(fifa19 %>% filter(Club == p$Club)), " players"),
      paste0("Joined : ", p$Joined),
      paste0("Contract valid until : ", p$Contract.Valid.Until),
      sep = "<br/>"
    ))
  })

  data_compare <- reactive({
    p <- current_player()
    if (input$boxplot_compare_with == "All") {
      data_compare <- fifa19
      data_compare_simple <- fifa19_simple
    }
    if (input$boxplot_compare_with == "Position") {
      data_compare <- fifa19 %>% filter(PositionSimplify == p$PositionSimplify)
      data_compare_simple <- fifa19_simple %>% filter(PositionSimplify == p$PositionSimplify)
    }
    if (input$boxplot_compare_with == "Club") {
      data_compare <- fifa19 %>% filter(Club == p$Club)
      data_compare_simple <- fifa19_simple %>% filter(Club == p$Club)
    }

    data_compare <- data_compare %>% filter(ID != current_player()$ID)
    data_compare_simple <- data_compare_simple %>% filter(ID != current_player()$ID)

    list(raw = data_compare, simple = data_compare_simple)
  })

  output$cv_boxplot <- renderPlot({
    p <- current_player()
    par(mfrow = c(1, 3), oma = c(0, 0, 2, 0))
    cv_boxplot(data = data_compare()$raw, player = p, var = "Age", color = COLOR)
    cv_boxplot(data = data_compare()$raw, player = p, var = "Height", color = COLOR)
    cv_boxplot(data = data_compare()$raw, player = p, var = "Weight", color = COLOR)
    title("Age, height and weight comparison", outer = TRUE, cex.main = 2)
  })


  output$plots_legend <- renderPlot({
    par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
    plot.new()
    legend("center", c(current_player()$Name, paste0("Others (", nrow(data_compare()$raw), ")")),
      col = c(COLOR$playerborder, COLOR$othersborder),
      pch = 15, cex = 2,
      box.lty = 0
    )
  })

  output$skills_plot <- renderPlot({
    p <- current_player()
    skill_names <- sapply(all_skills, "[[", 1)

    comp <- data_compare()$simple
    data <- rbind(
      rep(100, length(skill_names)),
      rep(0, length(skill_names)),
      colMeans(comp %>% select(skill_names)),
      fifa19_simple %>% filter(ID == p$ID) %>% select(skill_names)
    )
    plot_radar(
      player = p,
      radarmean = TRUE,
      data_skills = data,
      color = COLOR
    )
  })

  lapply(
    X = 1:7,
    FUN = function(x) {
      y <- paste0("skill", x)
      callModule(
        mod_radarchart_server,
        id = y,
        joueur = current_player,
        vector_of_skills = all_skills[[x]],
        data = data_compare
      )
    }
  )
}
