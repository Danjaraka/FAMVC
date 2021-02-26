###################
# body.R
# 
# Create the body for the ui. 
# If you had multiple tabs, you could split those into their own
# components as well.
###################
body <- dashboardBody(
  tags$head(
    tags$style(HTML("
    
    ")),
    tags$link(rel="shortcut icon", href="https://img.icons8.com/emoji/344/dna.png",type="image/x-icon")),
  ### changing theme
  shinyDashboardThemes(
      theme = "poor_mans_flatly"
  ),
  tabItems(
    ########################
    # FAMVC CUSTOM FILES
    ########################
    tabItem(
      tabName = "favmc-csv",
        # App title ----
      titlePanel("Functional Assessment of Missense Variant Clustering"),
      tags$hr(),

      # Sidebar layout with input and output definitions ----
      sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
          # Input: Select a file ----
          textOutput("debug"),
          radioButtons("method", "Protein data", choices = c("Search GNOMAD for gene by name" = "search", "Search GNOMAD for gene by transcript" = "search_trans" ,"Upload a CSV" = "csv"),selected = "search"),
          conditionalPanel(
            condition = "input.method == 'search'",
            textInput("search", "Search gene name: ", ""),
            checkboxInput("filter_transcripts", "Filter gene transcripts",value=FALSE)
          ),
          conditionalPanel(
            condition = "input.method == 'search_trans'",
            textInput("search2", "Search gene transcript: (WIP)", ""),
          ),
          conditionalPanel(
            condition = "input.method == 'csv'",
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")),
            actionButton("toggle", "Advanced Options"),
            conditionalPanel(
              condition = "input.toggle % 2 == 1",
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
            )
          ),
          # Horizontal line ----
          tags$hr(),
          # Input: Select a file ----
          fileInput("file2", "Choose clinvar csv File (Optional)",
                    multiple = FALSE,
                    accept = c("text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")),
          conditionalPanel(
            condition = "input.method == 'search'",
            checkboxInput("gnomad_clinvar", "Include clinvar variants from gnomad",value=TRUE)
          ),
          actionButton("toggleClinVar", "Advanced Options"),
          conditionalPanel(
            condition = "input.toggleClinVar % 2 == 1",
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

            radioButtons("disp2", "Display table preview", choices = c(Head = "head", All = "all", None = "none"),selected = "none"),
          ),
          tags$hr(),
          numericInput("protein_size", "Protein Size: ",1000, min = 0),
          tags$hr(),
          uiOutput("domain_name"),
          actionButton("add_btn", "Add Domain"),
          actionButton("rm_btn", "Remove Domain"),
          tags$hr(),
          uiOutput("key_name"),
          actionButton("add_key_btn", "Add Key"),
          actionButton("rm_key_btn", "Remove Key"),
          tags$hr(),
          actionButton("makePlot", "Make Plot"),
          tags$hr(),
        ),

        # Main panel for displaying outputs ----
        mainPanel(
          h2("Frequency as colour, CADD Phred scores as height"),
          box(width = 12, plotOutput("colourPlot") ),
          tableOutput("contents"),
          tableOutput("contents2"),
          h2("Download ANNOVAR output"),
          downloadButton("downloadData", "Download"),
          h2("Download High resolution plot"),
          downloadButton('downloadPlot', 'Download High Quality Plot'),
          downloadButton('downloadPDF', 'Download as a vector PDF')
        )
      )
    ),
    tabItem(  
      tabName = "favmc-gallery",
      titlePanel("Gallery"),
      tags$hr(),
      mainPanel(
        h1("test :)")
      )
    )
  )
)