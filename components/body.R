###################
# body.R
# 
# Create the body for the ui. 
# If you had multiple tabs, you could split those into their own
# components as well.
###################
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "favmc",
      h2("favmc"),
      # PLOT THE THTINGS
      box( plotOutput("favmc") )
    ),
    ########################
    # FAMVC CUSTOM FILES
    ########################
    tabItem(
      tabName = "favmc-csv",
      h2("favmc-csv"),
        # App title ----
  titlePanel("File upload and parameter customisation"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),

      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),

      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      radioButtons("disp", "Display table preview", choices = c(Head = "head", All = "all", None = "none"),selected = "none"),
      # Horizontal line ----
      tags$hr(),
      # Input: Select a file ----
      fileInput("file2", "Choose clinvar csv File",
                multiple = TRUE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
      
            # Input: Checkbox if file has header ----
      checkboxInput("header2", "Header", TRUE),

      # Input: Select separator ----
      radioButtons("sep2", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = "\t"),

      # Input: Select quotes ----
      radioButtons("quote2", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = ''),

      radioButtons("disp2", "Display table preview", choices = c(Head = "head", All = "all", None = "none"),selected = "head"),
      # Horizontal line ----
      tags$hr(),
      numericInput("protein_size", "Protein Size: ",1000, min = 0),
      actionButton("add_btn", "Add Domain"),
      actionButton("rm_btn", "Remove Domain"),
      textOutput("counter"),
      uiOutput("domain_name")
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      #actionButton("make", "Make Graph"),
      h2("Graph will be made on both files submission"),
      # Output: Data file ----
      tableOutput("contents"),
      tableOutput("contents2"),
      #box( plotOutput("customPlot") ),
      box( width = 12, plotOutput("customPlot2" ) )
    )

  )
    )
  )
)