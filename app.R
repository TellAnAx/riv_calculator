library(shiny)

# Load UI and server
source("code/ui.R")
source("code/server.R")


# Run the application 
shinyApp(ui = ui, server = server)
