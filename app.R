library(shiny)
library(dplyr)
library(magrittr)
library(stringr)
library(DT)

app_version <- read.dcf("DESCRIPTION")[, "Version"]


source("code/helper_functions.R")

# Load UI and server
source("code/ui.R")
source("code/server.R")



# Run the application
shinyApp(ui = ui, server = server)
