#' mod_overview_ui and mod_overview_server
#'
#' Shiny module for fifa2019 dataset quick view and basic statistics
#'
#' @param id shiny id
#'
#' @import shiny
#' @import shinydashboard
#' @import shinymaterial
#' @importFrom wordcloud2 wordcloud2Output
#'
mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$br(),

    splitLayout(
      cellWidths = c("70%", "30%"),


      tagList(
        box(
          title = "Fifa 2019 Kaggle dataset",
          width = 12,
          tags$p(
            style = "white-space: pre-wrap; word-break: keep-all;",
            "The data provides lastest edition FIFA 2019 players attributes."
          ),
          tags$p(
            style = "white-space: pre-wrap;word-break: keep-all;",
            "This Shiny app aims at making football analytics over the players, represented with around 60 attributes each, such as their age, height, weight, speed ..."
          ),
          tags$br(),
          tags$a(target = "_blank", rel = "noopener noreferrer", href = "https://www.kaggle.com/karangadiya/fifa19", "Raw dataset is available here.")

        ),

        fluidRow(
          column(
            width = 6,
            valueBoxOutput(ns("numb_player"), width = 12),
            valueBoxOutput(ns("mean_age"), width = 12)
          ),
          column(
            width = 6,
            valueBoxOutput(ns("numb_natio"), width = 12),
            valueBoxOutput(ns("numb_club"), width = 12)
          )
        ),


        box(
          width = 6,
          height = "500px",
          plotOutput(ns("hist_age"), height = "450px")
        ),

        box(
          width = 6,
          height = "500px",
          plotOutput(ns("bar_decrease")),
          fluidRow(
            column(
              width = 12,
              align = "center",
              radioButtons(
                inputId = ns("bar_choice"),
                label = "",
                choices = c("Wage", "Value", "Release.Clause"),
                inline = TRUE
              )
            )
          )
        )
      ),

      tagList(
        # wordcloud2Output(ns("cloud_natio2"), height = "200px"),
        # plotOutput(ns("cloud_natio")),
        box(
          width = 12,
          tags$h4("Preferred Foot", style = "text-align:center;"),
          plotOutput(ns("pie_foot"), height = "250px"),
          tags$hr(),

          tags$h4("Nationalities", style = "text-align:center;"),
          # wordcloud2Output(ns("cloud_natio2"), height = "250px"),
          plotOutput(ns("cloud_natio"), height = "250px"),

          tags$hr(),
          tags$h4("Position on field proportions", style = "text-align:center;"),
          plotOutput(ns("pie_position"), height = "250px")
        )
      )
    ),


    fluidRow("")
  )
}

#' mod_overview_server
#'
#' @param input internal
#' @param output internal
#' @param session internal
#' @param fifa19_raw dataset with all skill attributes
#' @param fifa19_simple dataset with simplified skill attributes
#'
#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @import wordcloud2
#' @importFrom wordcloud wordcloud
#' @importFrom RColorBrewer brewer.pal
#'
#' @rdname mod_overview_ui
mod_overview_server <- function(input, output, session, fifa19_raw, fifa19_simple) {
  ns <- session$ns


  output$pie_foot <- renderPlot({
    vec <- fifa19_raw$Preferred.Foot
    df <- as.data.frame(table(vec) / length(vec))
    colnames(df) <- c("Foot", "Count")
    pie <- ggplot(df, aes(x = "", y = Count, fill = Foot)) + geom_bar(stat = "identity", width = 1)
    pie <- pie + coord_polar("y", start = 0) +
      geom_text(aes(label = paste0(round(Count * 100), "%")), position = position_stack(vjust = 0.5))
    pie <- pie + scale_fill_manual(values = c("#F6AE2D", "#999999"))
    pie <- pie + labs(x = NULL, y = NULL, fill = NULL, title = "")
    pie <- pie + theme_classic() + theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "top"
    )
    pie
  })


  output$pie_position <- renderPlot({
    vec <- table(fifa19_raw$PositionSimplify)
    par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
    pie(vec)
  })


  output$bar_decrease <- renderPlot({
    feature <- input$bar_choice
    df <- fifa19_raw %>% select(feature, Name)

    # feature <- "Release.Clause"
    # df <- fifa2019_raw %>% select(feature, Name)
    df <- df[order(df[, feature], decreasing = TRUE), ]
    df <- df[10:1, ]
    if (feature == "Wage") {
      df$chr <- paste0(df[, feature] / 1000, "K \u20ac")
    } else {
      df$chr <- paste0(df[, feature] / 1000000, "M \u20ac")
    }

    p <- ggplot(data = df, aes_string(x = "Name", y = feature)) +
      geom_bar(stat = "identity", width = 0.6, fill = "steelblue") +
      geom_text(aes_string(label = "chr"), hjust = 1.1, vjust = 0.45, color = "white", size = 5) +
      coord_flip()
    p <- p + scale_x_discrete(limits = df$Name) +
      theme_minimal() +
      theme(
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.4, color = "#000000", size = 16, face = "bold")
      ) +
      ggtitle(paste("Top 10 players with the \n  highest", feature)) +

      labs(x = "", y = "")
    p
  })


  output$cloud_natio2 <- renderWordcloud2({
    freq <- data.frame(
      word = names(table(fifa19_raw$Nationality)),
      freq = as.vector(table(fifa19_raw$Nationality))
    )
    freq <- freq[order(freq$freq, decreasing = TRUE), ]
    wordcloud2(freq, size = 0.2)
  })

  output$cloud_natio <- renderPlot({
    freq <- data.frame(
      word = names(table(fifa19_raw$Nationality)),
      freq = as.vector(table(fifa19_raw$Nationality))
    )
    freq <- freq[order(freq$freq, decreasing = TRUE), ]
    par(
      oma = c(0, 0, 0, 0),
      mar = c(0, 0, 0, 0)
    )
    wordcloud(freq$word, freq$freq,
      random.order = FALSE,
      scale = c(2, 0.5),
      colors = brewer.pal(8, "Dark2"),
      max.words = 100
    )
  })


  output$hist_age <- renderPlot({
    df <- fifa19_raw["Age"]
    p <- ggplot(df, aes(x = Age)) +
      geom_histogram(bins = 30, color = "darkblue", fill = "lightblue") +
      theme_gray() +
      labs(
        title = "Histogram of Age \n ",
        x = "Age (years)",
        y = "Count"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, color = "#000000", size = 16, face = "bold"),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14)
      )
    p
  })


  output$numb_player <- renderValueBox({
    valueBox(
      value = nrow(fifa19_raw),
      subtitle = "Number of players",
      icon = icon("running"),
      color = "light-blue"
    )
  })


  output$mean_age <- renderValueBox({
    valueBox(
      value = paste(round(mean(fifa19_raw$Age, na.rm = TRUE)), "years old"),
      subtitle = "Players average age",
      icon = icon("id-card"),
      color = "light-blue"
    )
  })

  output$numb_club <- renderValueBox({
    valueBox(
      value = length(unique(fifa19_raw$Club)),
      subtitle = "Clubs",
      icon = icon("futbol"),
      color = "light-blue"
    )
  })

  output$numb_natio <- renderValueBox({
    valueBox(
      value = length(unique(fifa19_raw$Nationality)),
      subtitle = "Nationalities",
      icon = icon("flag"),
      color = "light-blue"
    )
  })
}
