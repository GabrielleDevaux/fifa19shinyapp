.rs.api.documentSaveAll() # close and save all opened files
suppressWarnings(lapply(
  paste('package:', names(sessionInfo()$otherPkgs), sep = ""),
  detach, character.only = TRUE, unload = TRUE))# detach all packages
rm(list = ls(all.names = TRUE))# clear environment
devtools::document('.') # generate NAMESPACE and man
devtools::load_all('.') # load package
options(app.prod = FALSE) # TRUE = production mode, FALSE = development mode
shiny::runApp('inst/app') # lance l'application

