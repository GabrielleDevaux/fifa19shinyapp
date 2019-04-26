#' display_tmja
#'
#' @param condition c
#' @param map  c
#' @param group  c
#' @param color c
#' @param data c
#' @param n c
#'
#'
#' @import leaflet
display_tmja <- function(condition, map, group, color, data, n) {
  if (condition) {
    leafletProxy(map, data = data[1:n, ]) %>%
      addCircles(
        lng = ~lngD, lat = ~latD,
        col = color, opacity = 0.6,
        weight = 3,
        group = group
      ) %>%
      addCircles(
        lng = ~lngF, lat = ~latF,
        col = color, opacity = 0.6,
        weight = 3,
        group = group
      )

    for (i in 1:n) {
      leafletProxy(map, data = data[i, ]) %>%
        addPolylines(
          lng = ~ c(lngD, lngF),
          lat = ~ c(latD, latF),
          col = color,
          opacity = 0.6,
          popup = ~ paste(
            "Section :", route,
            "<br/>",
            "TMJA :", as.character(TMJA),
            "<br/>",
            "Ratio PL :", as.character(RatioPL),
            "<br/>",
            "Type comptage trafic :", typeComptageTrafic
          ),
          weight = ~ sqrt(TMJA + 1) / 30,
          group = group
        )
    }
  } else {
    leafletProxy(map) %>% clearGroup(group)
  }
}
