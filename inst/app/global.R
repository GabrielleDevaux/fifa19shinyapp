#------------------------------------------------------------------------------
# Packages

library(dplyr)
library(leaflet)


#------------------------------------------------------------------------------
# Data

load("www/data/fifa2019_raw.RData")

load("www/data/all_skills.RData")

load("www/data/fifa2019_simple.RData")


COLOR = list(playerbg = rgb(1, 0.4, 0, 0.3),
             playerborder = rgb(1, 0.4, 0, 1),
             othersbg = rgb(0.2, 0.5, 0.5, 0.6),
             othersborder = rgb(0.2, 0.5, 0.5, 1))

#
# # leaflet map provider tiles
# data(providers, package = "leaflet")
# providers <- providers
#
# prov_choices <- c("OpenStreetMap",
#                   "OpenTopoMap",
#                   "Hydda",
#                   "Stamen.TonerLite",
#                   "Stamen.Watercolor",
#                   "Stamen.Terrain",
#                   "Esri",
#                   "Esri.DeLorme",
#                   "Esri.WorldTopoMap",
#                   "Esri.WorldImagery",
#                   "Esri.WorldTerrain",
#                   "Esri.WorldPhysical",
#                   "Esri.OceanBasemap",
#                   "Esri.NatGeoWorldMap",
#                   "Wikimedia")
#
#
#
#
# tmja_2017_coord <- read.table("www/data/tmja_2017_latlong.csv", sep = ';',
#                               stringsAsFactors = FALSE,
#                               header = TRUE)
# tmja_2017_coord <- tmja_2017_coord[order(tmja_2017_coord$TMJA),]
#
# tmja_2017_autoroutes <- tmja_2017_coord[grepl('A', tmja_2017_coord$route),]
# tmja_2017_nationales <- tmja_2017_coord[grepl('N', tmja_2017_coord$route),] %>%
#   na.omit(latD, lngD)
#
#


# dense_areas <- readOGR("www/data/logi_areas/Aires_logistiques_denses_wgs84.shp")
#
#
# large_areas <- readOGR("www/data/logi_areas/Aires_logistiques_elargies_wgs84.shp")
#





