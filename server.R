###################
# server.R
# 
# For all your server needs 
###################

library(dplyr)

server <- function(input, output, session) {
  
  histPlot_df <- eventReactive(
    input$submit,
    {
      df[[ input$columnChoice ]]
    })
  
  output$histPlot <- renderPlot({
    data <- histPlot_df()[ seq_len(input$slider) ]
    hist(data)
  })

  output$contents <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)

    df <- read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)

    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }

  })

   output$user_csv <- renderPlot({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)

    UBE3A <- read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)

    UBE3A_Fin <- read.delim ("UBE3A.txt.hg19_multianno.txt")
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }

  })

  output$favmc <- renderPlot({
    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # ADDING ANNOVAR PATHOGENICITY SCORES TO TABLE (dplyr)

    UBE3A <- read.csv("UBE3A.csv")
    UBE3A_Fin <- read.delim ("UBE3A.txt.hg19_multianno.txt")
    UBE3A$SIFT_pred <- UBE3A_Fin$SIFT_pred
    UBE3A$Polyphen2_HDIV_pred <- UBE3A_Fin$Polyphen2_HDIV_pred
    UBE3A$CADD_phred <- UBE3A_Fin$CADD_phred


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # SEPARATING CADD PHRED VALUES AS EITHER DELETERIOUS D (>=20) OR TOLERANT T (<20)

    UBE3A$CADD<- paste(UBE3A$CADD_phred)
    UBE3A$CADD <- case_when(
      UBE3A$CADD >= 20 ~ "D",
      TRUE          ~ "T" 
    )


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # ASSIGNING A PATHOGENICITY SCORE BASED UPON COMBINATIONS OF SIFT, POLYPHEN2 AND CADD PHRED SCORES

    UBE3A$Predic<- paste(UBE3A$SIFT_pred, UBE3A$Polyphen2_HDIV_pred, UBE3A$CADD)
    UBE3A$Predic <- case_when(
      UBE3A$Predic == 'D D D' ~ "Pathogenic",
      UBE3A$Predic == 'D D T' ~ "Unknown",
      UBE3A$Predic == 'D P D' ~ "Pathogenic",
      UBE3A$Predic == 'D P T' ~ "Unknown",
      UBE3A$Predic == 'D B D' ~ "Unknown",
      UBE3A$Predic == 'D B T' ~ "Unknown",
      UBE3A$Predic == 'T D D' ~ "Unknown",
      UBE3A$Predic == 'T D T' ~ "Unknown",
      UBE3A$Predic == 'T P D' ~ "Unknown",
      UBE3A$Predic == 'T P T' ~ "Unknown",
      UBE3A$Predic == 'T B D' ~ "Unknown",
      UBE3A$Predic == 'T B T' ~ "Benign",
      TRUE          ~ "Absolute Unknown"
    )


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # ASSIGNING ALLELE FREQUENCY VALUES INTO THREE GROUPS (AF<0.00001, 0.00001<AF<0.0001, AF>0.0001)

    UBE3A$Frequency<- paste(UBE3A$Allele.Frequency)
    UBE3A$Frequency[findInterval(UBE3A$Frequency, c(0, 0.00001)) == 1L] <- 1
    UBE3A$Frequency[findInterval(UBE3A$Frequency, c(0.00001, 0.0001)) == 1L] <- 2
    UBE3A$Frequency[findInterval(UBE3A$Frequency, c(0.0001, 1)) == 1L] <- 3



    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # ASSIGNING COLOURS TO EACH OF THE PREDICTION VALUE COMBINATIONS BETWEEN SIFT, POLYPHEN2 AND CADD PHRED

    UBE3A$Colour<- paste(UBE3A$SIFT_pred, UBE3A$Polyphen2_HDIV_pred, UBE3A$CADD)
    UBE3A$Colour <- case_when(
      UBE3A$Colour == 'D D D' ~ "blueviolet",
      UBE3A$Colour == 'D P D' ~ "blueviolet",
      UBE3A$Colour == 'T B T' ~ "dodgerblue1",
      UBE3A$Colour == 'D D T' ~ "grey",
      UBE3A$Colour == 'D P T' ~ "grey",
      UBE3A$Colour == 'D B D' ~ "grey",
      UBE3A$Colour == 'D B T' ~ "grey",
      UBE3A$Colour == 'T D D' ~ "grey",
      UBE3A$Colour == 'T D T' ~ "grey",
      UBE3A$Colour == 'T P D' ~ "grey",
      UBE3A$Colour == 'T P T' ~ "grey",
      UBE3A$Colour == 'T B D' ~ "grey",
      TRUE          ~ "grey"
    )


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # ENTRY OF PROTEIN DATA

    # ProteinSize is the total length of the protein 
    ProteinSize <- 875

    # DomCol is the colour of the domain
    DomCol1 <- "lightcoral"
    DomCol2 <- "orange1"

    # DomName is the name of the domain 
    DomName1 <- "AZUL"
    DomName2 <- "HECT"

    # LengthX.1 is the starting position of the domain & LengthX.2 is the end position of the domain where X represents a domain
    Length1.1 <- 30
    Length1.2 <- 83
    Length2.1 <- 523
    Length2.2 <- 875

    # Mean is the middle length between a domain start and end position. Used for domain labels  
    Mean1 <- (Length1.1+Length1.2)/2 
    Mean2 <- (Length2.1+Length2.2)/2 

    # Amount is the amount of amino acids that occupy a domain
    Amount1 <- (Length1.2 - Length1.1 + 1)
    Amount2 <- (Length2.2 - Length2.1 + 1)


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    #CREATION OF PATHOGENIC VARIANT TABLE

    library(dplyr)
        UBE3A_Path <- UBE3A %>% 
          filter(Predic =='Pathogenic')


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # EXPECTED VALUES OF ALL GNOMAD VARIANTS FOR EACH DOMAIN  

    ExpVal1 <- Amount1*nrow(UBE3A)/ProteinSize
    ExpVal2 <- Amount2*nrow(UBE3A)/ProteinSize
    ExpVal1
    ExpVal2


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # EXPECTED VALUES OF PATHOGENIC GNOMAD VARIANTS FOR EACH DOMAIN 

    PathExpVal1 <- Amount1*nrow(UBE3A_Path)/ProteinSize
    PathExpVal2 <- Amount2*nrow(UBE3A_Path)/ProteinSize
    PathExpVal1
    PathExpVal2


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # OBSERVED VALUES OF ALL GNOMAD VARIANTS FOR EACH DOMAIN 

    UBE3A.1 <- gsub("[a-zA-Z]", "", UBE3A$Consequence)
    UBE3A$NumericConsequence <- gsub("[.]", "", UBE3A.1)

    ObsVal1 <- filter(UBE3A, NumericConsequence  %in% Length1.1:Length1.2)
    ObsVal2 <- filter(UBE3A, NumericConsequence  %in% Length2.1:Length2.2)
    nrow(ObsVal1)
    nrow(ObsVal2)


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # OBSERVED VALUES OF PATHOGENIC GNOMAD VARIANTS FOR EACH DOMAIN 

    UBE3A.3 <- gsub("[a-zA-Z]", "", UBE3A_Path$Consequence)
    UBE3A_Path$NumericConsequence <- gsub("[.]", "", UBE3A.3)

    PathObsVal1 <- filter(UBE3A_Path, NumericConsequence  %in% Length1.1:Length1.2)
    PathObsVal2 <- filter(UBE3A_Path, NumericConsequence  %in% Length2.1:Length2.2)
    nrow(PathObsVal1)
    nrow(PathObsVal2)


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # P-VALUE CALCULATION OF ALL GNOMAD VARIANTS FOR EACH DOMAIN

    PAll1 <- pchisq((nrow(ObsVal1)-ExpVal1)^2/(ExpVal1), df=1, lower.tail=FALSE)
    PAll2 <- pchisq((nrow(ObsVal2)-ExpVal2)^2/(ExpVal2), df=1, lower.tail=FALSE)

    PAll1
    PAll2


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # P-VALUE CALCULATION OF PATHOGENIC GNOMAD VARIANTS FOR EACH DOMAIN

    PPath1 <- pchisq((nrow(PathObsVal1)-PathExpVal1)^2/(PathExpVal1), df=1, lower.tail=FALSE)
    PPath2 <- pchisq((nrow(PathObsVal2)-PathExpVal2)^2/(PathExpVal2), df=1, lower.tail=FALSE)

    PPath1
    PPath2


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # CONFIDENCE INTERVAL CALCULATION OF ALL GNOMAD VARIANTS 

    VarFreq <- nrow(UBE3A)/ProteinSize
    VarFreqAnti <- 1-VarFreq

    StandardError <- sqrt((VarFreq*VarFreqAnti)/ProteinSize)

    CI1 <- VarFreq-(1.96*StandardError)
    CI2 <- VarFreq+(1.96*StandardError)
    CI1
    CI2


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # VARIANTS PER AMINO ACID OF ALL GNOMAD VARIANTS FOR EACH DOMAIN

    DomFreq1 <- nrow(ObsVal1)/Amount1
    DomFreq2 <- nrow(ObsVal2)/Amount2

    DomFreq1
    DomFreq2


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # CONFIDENCE INTERVAL CALCULATION OF PATHOGENIC GNOMAD VARIANTS 

    PathVarFreq <- nrow(UBE3A_Path)/ProteinSize
    PathVarFreqAnti <- 1-PathVarFreq

    PathStandardError <- sqrt((PathVarFreq*PathVarFreqAnti)/ProteinSize)

    PathCI1 <- PathVarFreq-(1.96*PathStandardError)
    PathCI2 <- PathVarFreq+(1.96*PathStandardError)
    PathCI1
    PathCI2


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # VARIANTS PER AMINO ACID OF PATHOGENIC GNOMAD VARIANTS FOR EACH DOMAIN

    PathDomFreq1 <- nrow(PathObsVal1)/Amount1
    PathDomFreq2 <- nrow(PathObsVal2)/Amount2

    PathDomFreq1
    PathDomFreq2


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # P-VALUE ALL VARIANTS

    if(PAll1 < 0.001){print("Extremely Significant")} else if (PAll1 < 0.01) {print("Very Significant")} else if (PAll1 < 0.05) {print("Significant")} else {print("NS")}
    if(PAll2 < 0.001){print("Extremely Significant")} else if (PAll2 < 0.01) {print("Very Significant")} else if (PAll2 < 0.05) {print("Significant")} else {print("NS")}


    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    # CI ALL VARIANTS 

    if(DomFreq1 < CI1 | DomFreq1 > CI2){print("Significant")} else {print("NS")}
    if(DomFreq2 < CI1 | DomFreq2 > CI2){print("Significant")} else {print("NS")}


    ## ----fig.width=15, fig.height=4------------------------------------------------------------------------------------------------------------------------------------
    # PLOTTING BOTH THE GNOMAD AND CLINVAR VARIANTS (rmarkdown)

    par(mar = c(8, 5, 3, 5))

    # Plotting gnomAD Variants 
    UBE3A.1 <- gsub("[a-zA-Z]", "", UBE3A$Consequence)
    UBE3A.2 <- gsub("[.]", "", UBE3A.1)
    UBE3A$Number1 <- -4
    plot(UBE3A$Frequency~UBE3A.2, ylab = "", xlab = "", xlim=c(1,ProteinSize), ylim=c(-4, max(3)), xaxs="i",yaxs="i", yaxt="none", xaxt="none", type = 'h', col = UBE3A$Colour, bty="n")

    axis(1, c(1,ProteinSize))


    # Box Dimensions 
    rect(1, -2, ProteinSize, 0, col="grey88", border="black")
    rect(Length1.1, -2, Length1.2, 0, col=DomCol1)          
    rect(Length2.1, -2, Length2.2, 0, col=DomCol2)

    text(Mean1, -1, DomName1, cex = 1.3)
    text(Mean2, -1, DomName2, cex = 1.3)

    # Legends
    par(xpd=TRUE)
    legend(-20, -6, col = c("lightcoral", "orange1"), legend = c("ZN-binding domain", "HECT domain"), pch = 15, bty = "n", cex = 1.1)
    text(ProteinSize+35, 1, "<1:100,000", cex = 0.85)
    text(ProteinSize+35, 3, ">1:10,000", cex = 0.85)

    # Plotting ClinVar Variants
    UBE3A_ClinVar <- read.csv("UBE3A_ClinVar.csv")
    UBE3A_ClinVar$Label <- UBE3A_ClinVar$Change
    UBE3A_ClinVar.1 <- gsub("[a-zA-Z]", "", UBE3A_ClinVar$Change)
    UBE3A_ClinVar.2 <- gsub("[.]", "", UBE3A_ClinVar.1)
    UBE3A_ClinVar$Number <- -2.5

    par(new=TRUE)
    plot(UBE3A_ClinVar$Number~UBE3A_ClinVar.2,ylab = "", xlab = "", xlim=c(1,ProteinSize), ylim=c(-3.1, max(8)), xaxs="i",yaxs="i", yaxt="none", xaxt="none", type="h", col = "firebrick1", bty="n")
  })
  
}
