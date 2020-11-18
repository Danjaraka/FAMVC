###################
# body.R
# 
# Create the body for the ui. 
# If you had multiple tabs, you could split those into their own
# components as well.
###################
body <- dashboardBody(
  tabItems(
    
    ########################
    # First tab content
    ########################
    tabItem(
      tabName = "dashboard",
      fluidRow(
        
        # CONTROLS
        box(
          
          title = "Controls",
          
          # Choose a column
          selectInput(
            "columnChoice",
            "Choose a column:",
            choices = colnames(df),
            selected = "n"),
          
          sliderInput("slider", "Number of observations:", 1, 100, 50),
          
          # Create an eventReactive element
          actionButton(
            inputId = "submit",
            label = "Submit column")
          
        ),
        # PLOT THE THTINGS
        box( plotOutput("histPlot") )
      )
    ),
    
    ########################
    # Second tab content
    ########################
    tabItem(
      tabName = "widgets",
      h2("Widgets tab content")
    ),
    tabItem(
      tabName = "favmc",
      h2("favmc"),
      # PLOT THE THTINGS
      box( plotOutput("favmc") )
    ),
    ########################
    # Test FAVMC
    ########################
    tabItem(
      tabName = "favmc-csv",
      h2("favmc-csv"),
        # App title ----
  titlePanel("Uploading Files"),

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
       # Input: Select a file ----
      fileInput("file2", "Choose multianno TXT File",
                multiple = TRUE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),

      # Horizontal line ----
      tags$hr(),

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

      # Horizontal line ----
      tags$hr(),

      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")

    ),

    # Main panel for displaying outputs ----
    mainPanel(
      actionButton("make", "Make Graph"),
      h2("Widgets tab content")
      # Output: Data file ----
      #tableOutput("contents"),
      #tableOutput("user_csv")
    )

  )
    )
  )
)