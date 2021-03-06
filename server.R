###################
# server.R
# 
# For all your server needs 
###################

library(dplyr)
options(shiny.sanitize.errors = FALSE)

server <- function(input, output, session) {

  # Track the number of input boxes to render
  counter <- reactiveValues(n = 0, n_key = 0)
  domain <- reactiveValues()
  # Track all user inputs
  AllInputs <- reactive({
    x <- reactiveValuesToList(input)
  })
  #add domain counter
  observeEvent(input$add_btn, {counter$n <- counter$n + 1})
  observeEvent(input$rm_btn, {
    if (counter$n > 0) counter$n <- counter$n - 1
  })
  #add key counter
  observeEvent(input$add_key_btn, {counter$n_key <- counter$n_key + 1})
  observeEvent(input$rm_key_btn, {
    if (counter$n_key > 0) counter$n_key <- counter$n_key - 1
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
                    value = AllInputs()[[paste0("range_start", i)]],
                    min = 0),
            numericInput(inputId = paste0("range_end", i),
                    label = paste0("Range End ", i), 
                    value = AllInputs()[[paste0("range_end", i)]],
                    min = 0),
            tags$label(paste0("Domain Colour ", i)),
            colourInput(paste0("colour", i), NULL, AllInputs()[[paste0("colour", i)]],returnName = TRUE, palette = "limited",closeOnClick = TRUE)
          )
        })
      })
    }
  })

  key_textboxes <- reactive({
    n_key <- counter$n_key

    if (n_key > 0) {
      isolate({
        lapply(seq_len(n_key), function(i) {
          list(
            textInput(inputId = paste0("key_name", i),
                    label = paste0("Key Name ", i), 
                    value = AllInputs()[[paste0("key_name", i)]]),
            tags$label(paste0("Key Colour ", i)),
            colourInput(paste0("key_colour", i), NULL, "green",returnName = TRUE, palette = "limited",closeOnClick = TRUE)
          )
        })
      })
    }
  })

  output$domain_name <- renderUI({ textboxes() })

  output$key_name <- renderUI({ key_textboxes() })

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

  # Downloadable multianno txt ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("hg19_multianno", ".txt", sep = "")
    },
    content = function(file) {
      write.csv(annovar, file, row.names = FALSE)
    }
  )
  
  # Download plot
  output$downloadPlot <- downloadHandler(
    filename = function() { paste('graph', '.png', sep='') },
    content = function(file) {
      png(file,width = 1875,type = "cairo-png")
      print(plotInput())
      dev.off()
    }
  )

  #Download PDF
  output$downloadPDF <- downloadHandler(
    filename = function() { paste('graph', '.pdf', sep='') },
    content = function(file) {
      pdf(file, width=16, height=5)
      print(plotInput())
      dev.off()
    }
  )

  output$debug <- renderText({ 
    counter$debug
  })

  observeEvent(input$searchbtn, {
    command <- paste0("python3 gnomad.py -gene ", input$search)
    system(command, wait = TRUE)
    #searchOutput$text <- "LOLOLOLOL"
  })

  output$colourPlot <- renderPlot({
    print(plotInput())
  })

  plotInput <- reactive({
    library(dplyr)
    library(rmarkdown)
    library(RColorBrewer)
    library(scales)

    if (input$makePlot == 0)
      return()
    #delete previous output in /temp
    system("rm /home/dan/FAMVC/temp/*")
          # ADDING ANNOVAR PATHOGENICITY SCORES TO TABLE (dplyr)

    isolate({
      if(input$method == 'search'){
        command <- paste0("python3 gnomad.py -gene ", input$search)
        system(command, wait = TRUE)
        protein <- read.csv(paste0(paste0("temp/",input$search),".csv"))
      } else if (input$method == 'search_trans'){
          command <- paste0("python3 gnomad.py -t -gene ", input$search2)
          system(command, wait = TRUE)
          protein <- read.csv(paste0(paste0("temp/",input$search2),".csv"))
      } else if (input$method == 'csv') {
          protein <- read.csv(input$file1$datapath,header = input$header,sep = input$sep,quote = input$quote)
      }  
      # Plotting ClinVar Variants
      if(!is.null(input$file2)){
        protein_ClinVar <- read.csv(input$file2$datapath, header = input$header2 ,sep = input$sep2, quote = input$quote2)
      }

      #Code generate tsv for annovar, in future will be moved into functions.R
      annovarInput <- protein
      annovarInput$rsID <- annovarInput$Position
      #rename column
      colnames(annovarInput)[which(names(annovarInput) == "rsID")] <- "Position"
      write.table(annovarInput, file = file.path("/home/dan/FAMVC/temp", "file.txt"),row.names=FALSE,sep='\t',quote=FALSE)
      #Run annovar with the generated file
      system("../scripts/annovar/./multianno.sh", wait = TRUE)
      annovar <<- read.delim("/home/dan/FAMVC/temp/file.txt.hg19_multianno.txt")
      #remove second row of headers that annovar adds
      annovar <<- annovar[-c(2), ]
      
      protein$Height <- paste(annovar$CADD_phred)
      protein$Height <- as.numeric(protein$Height)
      protein$Height <- rescale(protein$Height, to = c(1,3), from = c(0,40))
      #protein frequency is represented as colour
      protein$Frequency <- paste(protein$Allele.Frequency)
      protein$Frequency[findInterval(protein$Frequency, c(0, 0.00001)) == 1L] <- 1
      protein$Frequency[findInterval(protein$Frequency, c(0.00001, 0.0001)) == 1L] <- 2
      protein$Frequency[findInterval(protein$Frequency, c(0.0001, 1)) == 1L] <- 3

      grey <- rgb(192, 192, 192, max = 255, alpha = 255, names = "grey50")
      blue <- rgb(51, 153, 255, max = 255, alpha = 255, names = "blue50")
      purple <- rgb(102, 0, 102, max = 255, alpha = 255, names = "purple50")
      rbPal <- colorRampPalette(c(grey,blue,purple))

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
      # Drops the characters and keeps the consequence value? 
      protein.1 <- gsub("[a-zA-Z]", "", protein$Protein.Consequence)
      protein$NumericConsequence <- gsub("[.]", "", protein.1)

      # PLOTTING BOTH THE GNOMAD AND CLINVAR VARIANTS
      par(mar = c(8, 5, 3, 5))
      
      # Plotting gnomAD Variants 
      protein.1 <- gsub("[a-zA-Z]", "", protein$Protein.Consequence)
      protein.2 <- gsub("[.]", "", protein.1)
      protein$protein.2 <- protein.2
      protein$Number1 <- -4

      plot(protein$Height[which(protein$Frequency == 1)]~protein$protein.2[which(protein$Frequency == 1)], ylab = "", xlab = "", xlim=c(1,ProteinSize), ylim=c(-7, max(3)), xaxs="i",yaxs="i", yaxt="none", xaxt="none", type = 'h', col = grey, bty="n")
      lines(protein$Height[which(protein$Frequency == 2)]~protein$protein.2[which(protein$Frequency == 2)], ylab = "", xlab = "", xlim=c(1,ProteinSize), ylim=c(-7, max(3)), xaxs="i",yaxs="i", yaxt="none", xaxt="none", type = 'h', col = blue, bty="n")
      lines(protein$Height[which(protein$Frequency == 3)]~protein$protein.2[which(protein$Frequency == 3)], ylab = "", xlab = "", xlim=c(1,ProteinSize), ylim=c(-7, max(3)), xaxs="i",yaxs="i", yaxt="none", xaxt="none", type = 'h', col = purple, bty="n")
      
      #legend(x = ProteinSize-100, y = -3.5 ,title="Frequency",legend=c("0 -> 0.00001","0.00001 -> 0.0001","0.0001 -> 1"),col =rbPal(3),pch=20)
      axis(side = 1, pos = -4.7, c(1,ProteinSize))
      axis(side = 4, at = c(1,3), labels = c("0","40"), pos = ProteinSize+ProteinSize*0.006)

      # Box Dimensions 
      rect(1, -2, ProteinSize, 0, col="grey88", border="black")
      if(counter$n > 0){
        lapply(1:counter$n, function(i) {
          rect(domain[[paste0("range_start", i)]], -2, domain[[paste0("range_end", i)]], 0, col=domain[[paste0("colour", i)]])
          text(domain[[paste0("mean", i)]], -1, domain[[paste0("name", i)]], cex = 1.3)
        })
      }

      # Legends
      par(xpd=TRUE)
      if(counter$n_key > 0){
        lapply(1:counter$n_key, function(i) {
          if(i < 5){
            x <- 0
            y <- (-6 -i *.8)
          }else{
            if(i < 9){
              x <- ProteinSize * 0.3
              y <- (-2.8 -i *.8)
            }else{
              x <- ProteinSize * 0.6
              y <- (0.4 -i *.8)
            }
          }
          #Very bad code must be a better way to do this...
          inputKeyColour <- paste("key_colour", i, sep ="")
          domain[[paste0("key_colour", i)]] <- input[[inputKeyColour]]

          inputKeyName <- paste("key_name", i, sep = "")
          domain[[paste0("key_name", i)]] <- input[[inputKeyName]]

          legend(x, y, col = domain[[paste0("key_colour", i)]], legend = domain[[paste0("key_name", i)]], pch = 15, bty = "n", cex = 1.1)
        })
      }

      # Plotting ClinVar Variants
      if(!is.null(input$file2)){
        protein_ClinVar$Label <- protein_ClinVar$Change
        protein_ClinVar.1 <- gsub("[a-zA-Z]", "", protein_ClinVar$Change)
        protein_ClinVar.2 <- gsub("[.]", "", protein_ClinVar.1)
        protein_ClinVar$Number <- -2

        par(new=TRUE)
        plot(protein_ClinVar$Number~protein_ClinVar.2,ylab = "", xlab = "", xlim=c(1,ProteinSize), ylim=c(-4.9, max(5)), xaxs="i",yaxs="i", yaxt="none", xaxt="none", type="h", col = "red2", bty="n")
      }
      #plotting GNOMAD Clinvar variants
      if(input$gnomad_clinvar){
          if(input$method == 'search_trans'){
            file_directory <- paste0("temp/",input$search2)
          }else{
            file_directory <- paste0("temp/",input$search)
          }
          file_directory <- paste0(file_directory,"_clinvar_variants.csv")
          gnomad_clinvar <- read.csv(file_directory, header = TRUE)
          gnomad_clinvar$hgvsp <- gsub("[^0-9.]", "",  gnomad_clinvar$hgvsp)
          gnomad_clinvar$hgvsp <- gsub("[.]", "", gnomad_clinvar$hgvsp)
          gnomad_clinvar$Number <- -2
          #counter$debug <- gnomad_clinvar$hgvsp
          par(new=TRUE)
          plot(gnomad_clinvar$Number~gnomad_clinvar$hgvsp,ylab = "", xlab = "", xlim=c(1,ProteinSize), ylim=c(-4.9, max(5)), xaxs="i",yaxs="i", yaxt="none", xaxt="none", type="h", col = "red2", bty="n")
        }
    })
  })  
}
