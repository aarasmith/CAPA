
source("global.R")
source("R/mod_info.R")
source("R/mod_weights.R")
source("R/mod_exposure.R")
source("R/mod_exposure_map.R")
source("R/mod_score_map.R")
source("R/mod_frequency.R")
source("mod_server.R")
source("mod_ui.R")


shinyApp(ui = ui, server = server)
