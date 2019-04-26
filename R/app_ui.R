#' app_ui
#'
#' UI of the application
#'
#' @import shiny
#' @import shinydashboard
#'
#' @export
app_ui <- function() {
  dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(
      disable = TRUE
    ),
    dashboardBody(
      includeCSS("www/styles.css"),

      # display load spinner when shiny is busy
      conditionalPanel(
        condition = "$(\'html\').hasClass(\'shiny-busy\')",
        tags$div(class = "loader")
      ),

      tabsetPanel(

        tabPanel(
          title = "Overview",
          mod_overview_ui("overview")
        ),

        tabPanel(
          title = "Dataset exploration",
          mod_explo_ui("explo")
        ),

        tabPanel(
          title = "Player visualisation",
          verticalLayout(
            tags$br(),
            mod_filtre_joueur_ui("filtre", fifa2019 = fifa2019_raw),
            mod_cv_joueur_ui("CV", fifa19 = fifa2019_raw)
          )
        ),

        tabPanel(
          title = "Classification",
          mod_cluster_ui("cluster", fifa19_raw = fifa2019_raw),

          fluidRow("")
        ),

        # tabPanel(
        #   title = "Time series",
        #   "content",
        #   mod_timeseries_ui("timeseries")
        # ),

        # tabPanel(
        #   title = "Cartography",
        #   mod_carto_ui("carto")
        #
        # ),
        id = "general_tabset" # ,
        # selected = "Cartography example"
      )
    )
  )
}
