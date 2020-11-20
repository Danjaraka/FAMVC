###################
# app.R
# 
# Main controller. 
# Used to import your ui and server components; initializes the app.
###################
library(shiny)
library(shinydashboard)
library(dplyr)
library(rmarkdown)

source('./ui.R')
source('./server.R')

#options(shiny.port = 8100)
shinyApp(ui, server)