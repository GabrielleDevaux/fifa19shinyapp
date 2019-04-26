#' All plot functions
#'
#' @param data data compare
#' @param player current player
#' @param var variables name
#' @param title plot title
#' @param axe axe title
#' @param legend legend
#' @param color color
#'

cv_boxplot <- function(data, player, var, title = "", axe = "", legend = NULL, color = NULL) {
  # color = list(playerbg = rgb(0.2, 0.5, 0.5, 0.4),
  #              playerborder = rgb(0.2, 0.5, 0.5, 0.8),
  #              othersbg = rgb(0.7, 0.5, 0.1, 0.4),
  #              othersborder = rgb(0.7, 0.5, 0.1, 0.8))
  values <- data[, var]
  player_values <- player[, var]
  boxplot(values,
    ylim = c( min(values, player_values) ,
              max(values, player_values)),
    cex.axis = 1.5,
    cex.lab = 1.5,
    col = color$othersbg, # rgb(0.2, 0.5, 0.5, 0.5),
    border = color$othersborder
  )
  points(1, player_values, pch = 18, col = color$playerborder, cex = 4)
  mtext(var, 1, line = 1)
}


#' plot_radar
#'
#' @param data_compare data
#' @param player current player
#' @param vector_of_skills skills
#' @param data_skills data
#' @param color color
#' @param radarmean bool
#'
#' @import magrittr
#' @import dplyr
#' @import shiny
#' @importFrom fmsb radarchart
#' @importFrom graphics par
#' @importFrom grDevices rgb
plot_radar <- function(data_compare = NULL, player, vector_of_skills = NULL,
                       data_skills = NULL, color = NULL, radarmean = FALSE) {
  # color = list(playerbg = rgb(0.2, 0.5, 0.5, 0.4),
  #              playerborder = rgb(0.2, 0.5, 0.5, 0.8),
  #              othersbg = rgb(0.7, 0.5, 0.1, 0.4),
  #              othersborder = rgb(0.7, 0.5, 0.1, 0.8))

  if (radarmean) {
    data <- data_skills
    title <- "Skills rating"
  } else {
    skills <- vector_of_skills$features
    data <- player %>% select(skills)
    data <- rbind(
      rep(100, length(skills)),
      rep(0, length(skills)),
      colMeans(data_compare %>% select(skills)),
      player %>% select(skills)
    )
    title <- paste(
      vector_of_skills$title, ":",
      round(mean(as.numeric(data[4, ])), digits = 1)
    )
  }


  par(
    mar = c(0, 0, ifelse(radarmean, 2, 1), 0),
    oma = c(0, 0, 0, 0)
  )

  radarchart(
    df = data,
    title = title,
    cex.main = ifelse(radarmean, 1.5, 1),
    # custom polygon
    pty = 19,
    pcol = c(color$othersborder, color$playerborder),
    plty = 1,
    plwd = 2,
    pfcol = c(color$othersbg, color$playerbg),

    # custom grid
    axistype = 1,
    seg = 5, # number of segment per axis
    axislabcol = "black", # grid labels color
    cglcol = "dimgrey", # grid lines color
    cglty = 1, # grid lines type
    cglwd = 1, # grid lines width
    caxislabels = seq(0, 100, 20), # grid axis labels
    calcex = 0.7,

    # custom labels
    vlcex = ifelse(radarmean, 1, 0.7) # font size of feature labels
  )
}
