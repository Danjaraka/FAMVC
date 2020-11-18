###################
# app.R
# 
# Main controller. 
# Used to import your ui and server components; initializes the app.
###################
library(shiny)
library(shinydashboard)
library(dplyr)

source('./ui.R')
source('./server.R')


shinyApp(ui, server)