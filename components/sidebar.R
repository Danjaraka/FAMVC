###################
# sidebar.R
# 
# Create the sidebar menu options for the ui.
###################
sidebar <- dashboardSidebar(
  collapsed = TRUE,
  disable = TRUE,
  sidebarMenu(
    menuItem("FAVMC CSV", tabName = "favmc-csv", icon = icon("file-csv")
    )
  )
)
