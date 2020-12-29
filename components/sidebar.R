###################
# sidebar.R
# 
# Create the sidebar menu options for the ui.
###################
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("FAVMC CSV", tabName = "favmc-csv", icon = icon("file-csv")),
    menuItem("FAVMC Colour", tabName = "favmc-colour", icon = icon("star"))
  )
)
