library(shiny)
library(dplyr)
library(magrittr)
library(stringr)
library(readxl)
library(DT)


# Load UI and server
source("code/ui.R")
source("code/server.R")



# Run the application
shinyApp(ui = ui, server = server)
