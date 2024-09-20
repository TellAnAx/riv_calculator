library(shiny)
library(dplyr)


# Load UI and server
source("code/ui.R")
source("code/server.R")



# Run the application 
shinyApp(ui = ui, server = server)
