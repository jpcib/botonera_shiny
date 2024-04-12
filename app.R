source("global.R")
source("server.R")
source("ui.R")

Sys.setlocale("LC_CTYPE", "en_US.UTF-8")


shinyApp(ui = ui, server = server)