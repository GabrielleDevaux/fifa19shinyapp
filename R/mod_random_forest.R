#' mod_random_forest_ui and mod_random_forest_server
#'
#' @param id shiny id
#' @param fifa19_raw fifa19
#'
#' @import shiny
#' @import shinydashboard

mod_random_forest_ui <- function(id, fifa19_raw) {
  ns <- NS(id)
  tagList(
    box(
      title = "Training set",
      width = 6,
      status = "warning",
      solidHeader = TRUE,
      height = "450px",
      tags$h4("Number of observations"),
      verbatimTextOutput(ns("info_train")),
      tags$h4("Metrics"),
      verbatimTextOutput(ns("metrics_train")),
      tags$h4("Confusion matrix"),
      verbatimTextOutput(ns("confu_train"))
    ),
    box(
      title = "Testing set",
      width = 6,
      status = "primary",
      solidHeader = TRUE,
      height = "450px",
      tags$h4("Number of observations"),
      verbatimTextOutput(ns("info_test")),
      tags$h4("Metrics"),
      verbatimTextOutput(ns("metrics_test")),
      tags$h4("Confusion matrix"),
      verbatimTextOutput(ns("confu_test"))
    ),

    box(
      title = "Make predictions",
      width = 6,
      # status = "primary",
      # solidHeader = TRUE,
      height = "450px",

      # htmlOutput(ns("pred_select")),

      selectizeInput(
        inputId = ns("pred_selec"),
        label = "Select players whose position you want to predict (5 max)",
        choices = setNames(fifa19_raw$ID, fifa19_raw$Name),
        multiple = TRUE,
        options = list(maxItems = 5)
      ),

      DT::dataTableOutput(ns("predictions"))
    ),

    box(
      title = "Variable Importance",
      width = 6,
      # status = "primary",
      # solidHeader = TRUE,
      height = "450px",
      # background = "navy",
      plotOutput(ns("var_imp_plot"), height = "350px")
    )
  )
}

#' mod_random_forest_server
#'
#' @param input internal
#' @param output internal
#' @param session internal
#' @param data_complete complete data
#' @param ratio_train ratio
#'
#' @importFrom caret confusionMatrix
#' @importFrom randomForest randomForest varImpPlot
#' @import shiny
#' @import dplyr
#'
#' @rdname mod_random_forest_ui
mod_random_forest_server <- function(input, output, session, data_complete, ratio_train) {
  ns <- session$ns

  RF <- function(dat) {
    train <- dat %>% sample_frac(ratio_train() / 100)
    test <- anti_join(dat, train)
    model <- randomForest(PositionSimplify ~ ., data = train, ntree = 500, na.action = na.omit)

    res_train <- list(
      real = train$PositionSimplify,
      pred = model$predicted
    )

    res_test <- list(
      real = test$PositionSimplify,
      pred = predict(model, test)
    )

    return(list(
      model = model,
      train = train,
      test = test,
      res_train = res_train,
      res_test = res_test
    ))
  }

  data <- data_complete %>% select(-ID, -Name, -Nationality, -Club)

  res <- RF(data)

  output$var_imp_plot <- renderPlot({
    par(mar = c(4, 2, 0.5, 1)) # b l t r
    varImpPlot(res$model,
      n.var = min(20, ncol(data)),
      main = ""
    )
  })

  conf_train <- confusionMatrix(
    data = res$res_train$pred,
    reference = res$res_train$real
  )
  conf_test <- confusionMatrix(
    data = res$res_test$pred,
    reference = res$res_test$real
  )


  generate_nobs <- function(vector) {
    info1 <- data.frame(Total = c(length(vector), ""))
    info2 <- data.frame(rbind(table(vector)))
    info2[1, ] <- as.character(info2[1, ])

    info2 <- rbind(info2, data.frame(rbind(round(prop.table(table(vector)) * 100, 1))))
    info2[2, ] <- paste0(info2[2, ], "%")
    info <- cbind(info1, info2)
    rownames(info) <- c("", " ")

    return(info)
  }

  output$info_train <- renderPrint({
    generate_nobs(res$res_train$real)
  })

  output$info_test <- renderPrint({
    generate_nobs(res$res_test$real)
  })

  output$metrics_train <- renderPrint({
    round(conf_train$overall[1:4], 3)
  })
  output$metrics_test <- renderPrint({
    round(conf_test$overall[1:4], 3)
  })

  output$confu_train <- renderPrint({
    conf_train$table
  })
  output$confu_test <- renderPrint({
    conf_test$table
  })



  # output$pred_select <- renderUI({
  #   tagList(
  #     selectizeInput(inputId = ns("pred_selec"),
  #                 label = "Select players whose position you want to predict (5 max)",
  #                 choices = setNames(data_complete$ID, data_complete$Name),
  #                 multiple = TRUE,
  #                 options = list(maxItems = 5))
  #   )
  # })


  output$predictions <- DT::renderDT({
    data <- data_complete %>% filter(ID %in% input$pred_selec)

    predictions <- predict(res$model, data %>% select(-ID, -Name, -Nationality, -Club))

    data.frame(
      Name = data$Name, "Actual position" = data$PositionSimplify,
      "Predicted position" = predictions
    )
  }, rownames = FALSE)
}
