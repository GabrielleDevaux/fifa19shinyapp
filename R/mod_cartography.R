#' mod_carto_ui and mod_carto_server
#'
#' @param id shiny id
#'
#' @import shiny
#' @import shinydashboard
#' @import leaflet
#'
mod_carto_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$br(),

    box(
      title = "Map visualisation",
      width = 8,
      leafletOutput(ns("main_map"))
    ),

    box(
      title = "Options",
      width = 4,

      radioButtons(
        inputId = ns("providers_choice"),
        label = "Map background",
        choices = prov_choices
      ),
      # include this input in a dropdown button from shinyWidgets above the map


      # checkboxInput(
      #   inputId = ns("display_demography"),
      #   label = "Display demography per country"
      # ),


      tags$hr(),

      tags$h4("France : "),

      checkboxInput(
        inputId = ns("display_autoroute"),
        label = "Display highways"
      ),

      checkboxInput(
        inputId = ns("display_nationale"),
        label = "Display national roads"
      ),

      checkboxInput(
        inputId = ns("display_log_dense"),
        label = "Display dense zones with logistic areas"
      ),

      checkboxInput(
        inputId = ns("display_log_large"),
        label = "Display large zones with logistic areas"
      )
    ),

    fluidRow("")
  )
}


#' mod_carto_server
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @import leaflet
#' @import shiny
#' @importFrom rgdal readOGR
#'
#' @rdname mod_carto_ui
mod_carto_server <- function(input, output, session) {
  ns <- session$ns

  output$main_map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 2.5, lat = 47, zoom = 5) %>%
      addProviderTiles(providers$OpenStreetMap,
        options = providerTileOptions(
          opacity = 1,
          zIndex = 0
        )
      )
  })


  # observeEvent(input$display_demography,{
  #   # add demography polygons and legend
  #   if(input$display_demography){
  #
  #     print("afficher demo")
  #   } else{
  #     print("retirer demo")
  #   }
  # })

  observeEvent(input$providers_choice, {
    leafletProxy("main_map") %>%
      addProviderTiles(eval(parse(text = paste0("providers$", input$providers_choice))),
        options = providerTileOptions(
          opacity = 1,
          zIndex = 0
        )
      )
  })

  observeEvent(input$display_autoroute, {
    # add highways lines in France
    condition <- input$display_autoroute
    map <- "main_map"
    group <- "autoroute"
    color <- "blue"
    data <- tmja_2017_autoroutes
    n <- nrow(data)

    display_tmja(condition, map, group, color, data, n)
  })

  observeEvent(input$display_nationale, {
    # add national roads lines in France
    condition <- input$display_nationale
    map <- "main_map"
    group <- "nationale"
    color <- "red"
    data <- tmja_2017_nationales
    n <- nrow(data)

    display_tmja(condition, map, group, color, data, n)
  })

  dense_areas <- readOGR("www/data/logi_areas/Aires_logistiques_denses_wgs84.shp")
  large_areas <- readOGR("www/data/logi_areas/Aires_logistiques_elargies_wgs84.shp")

  observeEvent(input$display_log_dense, {
    # add dense zones with logistic areas
    if (input$display_log_dense) {
      leafletProxy("main_map") %>%
        addPolygons(
          data = dense_areas,
          fillColor = "blue",
          color = "black",
          layerId = ~e1, fillOpacity = 0.5,
          weight = 3,
          # options = tileOptions(zindex = 5),
          group = "denses"
        )
    } else {
      leafletProxy("main_map") %>% clearGroup("denses")
    }
  })

  observeEvent(input$display_log_large, {
    # add large zones with logistic areas
    if (input$display_log_large) {
      leafletProxy("main_map") %>%
        addPolygons(
          data = large_areas,
          fillColor = "red",
          color = "black",
          layerId = ~e1, fillOpacity = 0.5,
          weight = 3,
          group = "elargies"
        )

      if (input$display_log_dense) {
        leafletProxy("main_map") %>%
          addPolygons(
            data = dense_areas,
            fillColor = "blue",
            color = "black",
            layerId = ~e1, fillOpacity = 0.5,
            weight = 3,
            group = "denses"
          )
      }
    } else {
      leafletProxy("main_map") %>% clearGroup("elargies")
    }
  })
}
