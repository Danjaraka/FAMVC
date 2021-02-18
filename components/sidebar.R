###################
# sidebar.R
# 
# Create the sidebar menu options for the ui.
###################
sidebar <- dashboardSidebar(
  #collapsed = TRUE,
  #disable = TRUE,
  sidebarMenu(
      menuItem("FAVMC", tabName = "favmc-csv", icon = icon("file-csv")),
      menuItem("FAVMC Gallery", tabName = "favmc-gallery", icon = icon("image"))
  )
)
