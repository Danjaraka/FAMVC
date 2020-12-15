###################
# server.R
# 
# For all your server needs 
###################

library(dplyr)
options(shiny.sanitize.errors = FALSE)

server <- function(input, output, session) {

   # Track the number of input boxes to render
  counter <- reactiveValues(n = 0)

  domain <- reactiveValues()

  # Track all user inputs
  AllInputs <- reactive({
    x <- reactiveValuesToList(input)
  })

  observeEvent(input$add_btn, {counter$n <- counter$n + 1})
  observeEvent(input$rm_btn, {
    if (counter$n > 0) counter$n <- counter$n - 1
  })

  textboxes <- reactive({

    n <- counter$n

    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          list(
            textInput(inputId = paste0("domain_name", i),
                    label = paste0("Domain Name ", i), 
                    value = AllInputs()[[paste0("domain_name", i)]]),
            numericInput(inputId = paste0("range_start", i),
                    label = paste0("Range Start ", i), 
                    value = 0,
                    min = 0),
            numericInput(inputId = paste0("range_end", i),
                    label = paste0("Range End ", i), 
                    value = 0,
                    min = 0),
            tags$label(paste0("Domain Colour ", i)),
            colourInput(paste0("colour", i), NULL, "green",returnName = TRUE, palette = "limited",closeOnClick = TRUE)
          )
        })
      })
    }
  })

  output$contents <- renderTable({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)

    if(input$disp == "head") {
      return(head(df))
    }else if(input$disp == "none"){
      return()
    }else if(input$disp == "all"){
      return(df)
    }

  })

  output$contents2 <- renderTable({
    req(input$file2)
    df <- read.csv(input$file2$datapath,
             header = input$header2,
             sep = input$sep2,
             quote = input$quote2)

    if(input$disp2 == "head") {
      return(head(df))
    }else if(input$disp2 == "none"){
      return()
    }else if(input$disp2 == "all"){
      return(df)
    }
  })

  output$customPlot2 <- renderPlot({
    library(dplyr)
    library(rmarkdown)

    req(input$file1,input$file2)

    # ADDING ANNOVAR PATHOGENICITY SCORES TO TABLE (dplyr)
    protein <- read.csv(input$file1$datapath,header = input$header,sep = input$sep,quote = input$quote)
    # Plotting ClinVar Variants
    protein_ClinVar <- read.csv(input$file2$datapath, header = input$header2 ,sep = input$sep2, quote = input$quote2)

    # ASSIGNING ALLELE FREQUENCY VALUES INTO THREE GROUPS (AF<0.00001, 0.00001<AF<0.0001, AF>0.0001)

    protein$Frequency<- paste(protein$Allele.Frequency)
    protein$Frequency[findInterval(protein$Frequency, c(0, 0.00001)) == 1L] <- 1
    protein$Frequency[findInterval(protein$Frequency, c(0.00001, 0.0001)) == 1L] <- 2
    protein$Frequency[findInterval(protein$Frequency, c(0.0001, 1)) == 1L] <- 3

    # ProteinSize is the total length of the protein 
    ProteinSize <- input$protein_size
    
    lapply(1:counter$n, function(i) {
      #domain name
      inputName <- paste("domain_name", i, sep = "")
      domain[[paste0("name", i)]] <- input[[inputName]]
      #domain colour
      inputColour <- paste("colour", i, sep ="")
      domain[[paste0("colour", i)]] <- input[[inputColour]]
      #domain range start
      inputStart <- paste("range_start", i, sep ="")
      domain[[paste0("range_start", i)]] <- input[[inputStart]]
      #domain range end
      inputEnd <- paste("range_end", i, sep ="")
      domain[[paste0("range_end", i)]] <- input[[inputEnd]]
      #calculate range mean
      domain[[paste0("mean", i)]] <- (input[[inputStart]] + input[[inputEnd]])/2
    })

    # OBSERVED VALUES OF ALL GNOMAD VARIANTS FOR EACH DOMAIN 
    protein.1 <- gsub("[a-zA-Z]", "", protein$Consequence)
    protein$NumericConsequence <- gsub("[.]", "", protein.1)

    # PLOTTING BOTH THE GNOMAD AND CLINVAR VARIANTS
    par(mar = c(8, 5, 3, 5))
    
    # Plotting gnomAD Variants 
    protein.1 <- gsub("[a-zA-Z]", "", protein$Consequence)
    protein.2 <- gsub("[.]", "", protein.1)
    protein$Number1 <- -4
    plot(protein$Frequency~protein.2, ylab = "", xlab = "", xlim=c(1,ProteinSize), ylim=c(-4, max(3)), xaxs="i",yaxs="i", yaxt="none", xaxt="none", type = 'h', col = "dodgerblue1", bty="n")

    axis(1, c(1,ProteinSize))

    # Box Dimensions 
    rect(1, -2, ProteinSize, 0, col="grey88", border="black")

    lapply(1:counter$n, function(i) {
      rect(domain[[paste0("range_start", i)]], -2, domain[[paste0("range_end", i)]], 0, col=domain[[paste0("colour", i)]])
      text(domain[[paste0("mean", i)]], -1, domain[[paste0("name", i)]], cex = 1.3)
    })

    # Legends
    par(xpd=TRUE)
    legend(-20, -6, col = c("lightcoral", "orange1"), legend = c("ZN-binding domain", "HECT domain"), pch = 15, bty = "n", cex = 1.1)
    text(ProteinSize+35, 1, "<1:100,000", cex = 0.85)
    text(ProteinSize+35, 3, ">1:10,000", cex = 0.85)

    # Plotting ClinVar Variants
    protein_ClinVar$Label <- protein_ClinVar$Change
    protein_ClinVar.1 <- gsub("[a-zA-Z]", "", protein_ClinVar$Change)
    protein_ClinVar.2 <- gsub("[.]", "", protein_ClinVar.1)
    protein_ClinVar$Number <- -2.5

    par(new=TRUE)
    plot(protein_ClinVar$Number~protein_ClinVar.2,ylab = "", xlab = "", xlim=c(1,ProteinSize), ylim=c(-3.1, max(8)), xaxs="i",yaxs="i", yaxt="none", xaxt="none", type="h", col = "red2", bty="n")
  })

}
