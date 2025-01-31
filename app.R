
source("global.R")
# source("R/mod_info.R")
# source("R/mod_weights.R")
# source("R/mod_exposure.R")
# source("R/mod_exposure_map.R")
# source("R/mod_score_map.R")
# source("R/mod_frequency.R")
# source("R/mod_regional.R")
# source("R/mod_car.R")
# source("R/util_modules/mod_download.R")
source("server.R")
source("ui.R")

lapply(list.files("R", recursive = T, full.names = T), source)


shinyApp(ui = ui, server = server)
