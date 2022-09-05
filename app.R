
source("global.R")
source("R/mod_info.R")
source("R/mod_weights.R")
source("mod_server.R")
source("mod_ui.R")


shinyApp(ui = ui, server = server)
