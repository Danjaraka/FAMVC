###################
# sidebar.R
# 
# Create the sidebar menu options for the ui.
###################
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th")),
    menuItem("FAVMC", tabName = "favmc", icon = icon("star")),
    menuItem("FAVMC CSV", tabName = "favmc-csv", icon = icon("file-csv"))
  )
)
