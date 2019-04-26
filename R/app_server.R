

#' app_server
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @import shiny
#'
#' @export
app_server <- function(input, output, session) {

  # id_joueur <- reactive({input$choix_joueur})
  #
  # observe({cat("1",id_joueur(), "\n")})

  callModule(mod_explo_server,
    id = "explo"
  )

  filtre_inputs <- callModule(mod_filtre_joueur_server,
    id = "filtre"
  )

  callModule(mod_overview_server,
    id = "overview",
    fifa19_raw = fifa2019_raw,
    fifa19_simple = fifa2019_simple
  )


  callModule(mod_cv_joueur_server,
    id = "CV",
    fifa19 = fifa2019_raw,
    fifa19_simple = fifa2019_simple,
    filtre_inputs = filtre_inputs
  )

  callModule(mod_cluster_server,
    id = "cluster",
    fifa19_raw = fifa2019_raw,
    fifa19_simple = fifa2019_simple
  )

  # callModule(mod_timeseries_server,
  #            id = "timeseries")
  #
  # callModule(mod_carto_server,
  #            id = "carto")
}
