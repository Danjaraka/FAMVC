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

  # Track all user inputs
  AllInputs <- reactive({
    x <- reactiveValuesToList(input)
  })

  observeEvent(input$add_btn, {counter$n <- counter$n + 1})
  observeEvent(input$rm_btn, {
    if (counter$n > 0) counter$n <- counter$n - 1
  })

  output$counter <- renderPrint(print(counter$n))

  textboxes <- reactive({

    n <- counter$n

    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          list(
            textInput(inputId = paste0("domain_name", i),
                    label = paste0("Domain Name ", i), 
                    value = AllInputs()[[paste0("domain_name", i)]]),
            textInput(inputId = paste0("range_start", i),
                    label = paste0("Range Start", i), 
                    value = AllInputs()[[paste0("range_start", i)]]),
            textInput(inputId = paste0("range_end", i),
                    label = paste0("Range End ", i), 
                    value = AllInputs()[[paste0("range_end", i)]])
          )
        })
      })
    }
  })

  output$domain_name <- renderUI({ textboxes() })

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

    # ENTRY OF PROTEIN DATA

    # ProteinSize is the total length of the protein 
    ProteinSize <- 875

    # DomCol is the colour of the domain
    DomCol1 <- "lightcoral"
    DomCol2 <- "orange1"
    DomCol3 <- "blue"

    # DomName is the name of the domain 
    DomName1 <- "AZUL"
    DomName2 <- "HECT"
    DomName3 <- "DAN"

    # LengthX.1 is the starting position of the domain & LengthX.2 is the end position of the domain where X represents a domain
    Length1.1 <- 30
    Length1.2 <- 83
    Length2.1 <- 523
    Length2.2 <- 875

    Length3.1 <- 200
    Length3.2 <- 400

    # Mean is the middle length between a domain start and end position. Used for domain labels  
    Mean1 <- (Length1.1+Length1.2)/2 
    Mean2 <- (Length2.1+Length2.2)/2
    Mean3 <- (Length3.1+Length3.2)/2 

    # Amount is the amount of amino acids that occupy a domain
    #Amount1 <- (Length1.2 - Length1.1 + 1)
    #Amount2 <- (Length2.2 - Length2.1 + 1)
    #Amount3 <- (Length3.2 - Length3.1 + 1)
    # EXPECTED VALUES OF ALL GNOMAD VARIANTS FOR EACH DOMAIN  
    #ExpVal1 <- Amount1*nrow(protein)/ProteinSize
    #ExpVal2 <- Amount2*nrow(protein)/ProteinSize
    #ExpVal3 <- Amount3*nrow(protein)/ProteinSize

    # OBSERVED VALUES OF ALL GNOMAD VARIANTS FOR EACH DOMAIN 
    protein.1 <- gsub("[a-zA-Z]", "", protein$Consequence)
    protein$NumericConsequence <- gsub("[.]", "", protein.1)

    #ObsVal1 <- filter(protein, NumericConsequence  %in% Length1.1:Length1.2)
    #ObsVal2 <- filter(protein, NumericConsequence  %in% Length2.1:Length2.2)
    #ObsVal3 <- filter(protein, NumericConsequence  %in% Length3.1:Length3.2)
    #nrow(ObsVal1)
    #nrow(ObsVal2)
    #nrow(ObsVal3)

    # P-VALUE CALCULATION OF ALL GNOMAD VARIANTS FOR EACH DOMAIN
    #PAll1 <- pchisq((nrow(ObsVal1)-ExpVal1)^2/(ExpVal1), df=1, lower.tail=FALSE)
    #PAll2 <- pchisq((nrow(ObsVal2)-ExpVal2)^2/(ExpVal2), df=1, lower.tail=FALSE)

    #PAll3 <- pchisq((nrow(ObsVal3)-ExpVal3)^2/(ExpVal3), df=1, lower.tail=FALSE)

    # VARIANTS PER AMINO ACID OF ALL GNOMAD VARIANTS FOR EACH DOMAIN

    #DomFreq1 <- nrow(ObsVal1)/Amount1
    #DomFreq2 <- nrow(ObsVal2)/Amount2
    #DomFreq3 <- nrow(ObsVal3)/Amount3

    # P-VALUE ALL VARIANTS
    #if(PAll1 < 0.001){print("Extremely Significant")} else if (PAll1 < 0.01) {print("Very Significant")} else if (PAll1 < 0.05) {print("Significant")} else {print("NS")}
    #if(PAll2 < 0.001){print("Extremely Significant")} else if (PAll2 < 0.01) {print("Very Significant")} else if (PAll2 < 0.05) {print("Significant")} else {print("NS")}

    ## ----fig.width=15, fig.height=4------------------------------------------------------------------------------------------------------------------------------------
    # PLOTTING BOTH THE GNOMAD AND CLINVAR VARIANTS (rmarkdown)
    par(mar = c(8, 5, 3, 5))
    
    # Plotting gnomAD Variants 
    protein.1 <- gsub("[a-zA-Z]", "", protein$Consequence)
    protein.2 <- gsub("[.]", "", protein.1)
    protein$Number1 <- -4
    plot(protein$Frequency~protein.2, ylab = "", xlab = "", xlim=c(1,ProteinSize), ylim=c(-4, max(3)), xaxs="i",yaxs="i", yaxt="none", xaxt="none", type = 'h', col = "dodgerblue1", bty="n")

    axis(1, c(1,ProteinSize))

    # Box Dimensions 
    rect(1, -2, ProteinSize, 0, col="grey88", border="black")
    rect(Length1.1, -2, Length1.2, 0, col=DomCol1)          
    rect(Length2.1, -2, Length2.2, 0, col=DomCol2)
    rect(Length3.1, -2, Length3.2, 0, col=DomCol3)

    text(Mean1, -1, DomName1, cex = 1.3)
    text(Mean2, -1, DomName2, cex = 1.3)
    text(Mean3, -1, DomName3, cex = 1.3)

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
