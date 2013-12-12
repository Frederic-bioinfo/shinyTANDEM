shinyTandemServer <- function(input, output, session) {

  rv <- reactiveValues()

  #######
  ## Load results section
  #######

  ### Load result from RDS
  loadedResultRDS <- reactive({
    if( input$loadFromRDS > 0) {

      rv$loadStateIndicator <- NULL

      ### Test if file was uploaded:
      if( ! "data.frame" %in% class(input$resultRDS) ) {
      rv$loadStateIndicator <-
        "You must upload a file before loading it into memory"
      return(NULL)
      }
      
      ### Catch error if file cannot be read.
      if( file.access( isolate(input$resultRDS$datapath), mode=4) == -1 ){
        rv$loadStateIndicator <-
          paste("File: \"", isolate(input$resultRDS$name),"\" cannot be read.")
        return(NULL)
      }

      ## TO-DO: put this in a try-catch to recuperate the error messages
      ## yielded if the format is not recognized.
      progressRDS <- Progress$new(session, min=0, max=1)
      on.exit(progressRDS$close())
      progressRDS$set(message="Loading RDS file into memory.", value=NULL)
      
      temp <- isolate(readRDS(file=input$resultRDS$datapath))

      if(! is(temp, "rTResult")) {
        rv$loadStateIndicator <-
          paste("\"",input$resultRDS$name,"\""," is not a result object.", sep="")
        return(NULL)
      }
      
      rv$loadStateIndicator <- NULL
      rv$loadedDataset<- "Dataset successfully loaded from RDS file."
      return(temp)
    }
  })
  
  ### Load result from xml
  loadedResultXML <- reactive({
    if(  input$loadFromXML > 0) {
      rv$loadStateIndicator <- NULL
      
      ### Test if file was uploaded:
      if( ! "data.frame" %in% class(input$resultXML) ) {
      rv$loadStateIndicator <-
        "You must upload a file before parsing it into memory"
      return(NULL)
      }
      
      ### Test if file can be read.
      if( file.access(isolate(input$resultXML$datapath), mode=4) == -1){
        rv$loadStateIndicator <-
          paste("File: \"", isolate(input$resultXML$name),"\" cannot be read.")
        return(NULL)
      }

      progressXML <- Progress$new(session, min=0, max=1)
      on.exit(progressXML$close())

      progressXML$set(message="Parsing XML and loading into memory. Please wait as this could take some time...", value=NULL)
    
      ### To-do: put this in a tryCatch structure
      temp<- isolate(GetResultsFromXML(input$resultXML$datapath))
      rv$loadStateIndicator <- NULL
      rv$loadedDataset<-"Dataset successfully loaded from xml file."

      progressXML$close()
      return(temp)
    }
  })

  ### Load result from R session:
  loadedResultSession <- reactive({
    if (! is.null(dataset)) {
      rv$loadedDataset <-"The dataset was successfully loaded while starting the shiny server"
      return(dataset)
    }
  })
     
  ### Assign to reactive values:
  observe({ rv$result <- loadedResultRDS() })
  observe({ rv$result <- loadedResultXML() })
  observe({ rv$result <- loadedResultSession() })

  ### Load state indicators:
  output$loadStateIndicator <- renderUI({
    if (is.null(rv$loadStateIndicator)){
      return(invisible(NULL))
    }
    return(
      div(class="alert alert-info", rv$loadStateIndicator)
    )
  })
  
  ### Loaded dataset indicator
  output$loadedDataset <- renderUI({
    if( is.null(rv$result) ){
      return(           
        div(class="alert alert-danger", style="text-align: center;",
            "No dataset is loaded!")
      )
    } else {
      return(
        div(class="alert alert-success", style="text-align: center;",
            rv$loadedDataset
        )
      )
    }
  })

  #######
  ### Result overview section
  #######
  output$overviewAnalysis <- renderText({
    if( is.null(rv$result)) {
      return("Warning: A dataset must be loaded to access analysis overview")
    }
       
    params <- rv$result@used.parameters
      
    tableAsHTML(data.frame(
      "Property"=c(
        "x! tandem algorithm version",
        "Search start time",
        "Spectra files",
        "Taxon",
        "Sequence files",
        "Cleavage sites",
        "Number of identified proteins",
        "Number of identified peptides",
        "Assigned spectra/Total spectra",
        "Residue fixed modifications",
        "Residue potential modifications"
        ),
      "Value"=c(
        rv$result@xtandem.version,
        rv$result@start.time,
        params$"spectrum, path",
        params$"protein, taxon",
        rv$result@sequence.source.paths,
        params$'protein, cleavage site',
        length(rv$result@proteins[,uid]),
        length(rv$result@peptides[,pep.id]),
        paste(rv$result@total.spectra.assigned,
              rv$result@nb.input.spectra, sep="/"),
        params$"residue, modification mass",
        params$"residue, potential modification mass"
        )
     ))
   })

  ### Display protein overview
  output$overviewProteins <- renderText({
   if(is.null(rv$result)){
     return("Warning: A dataset must be loaded to see the identified proteins")
    }
    tableAsHTML(rv$result@proteins[,c(1,2,3,6,7),with=FALSE])
  })

  ### Display protein overview
  output$overviewPeptides <- renderText({
     if(is.null(rv$result)){
       return("Warning:A dataset must be loaded to see the identified peptides")
    }
    tableAsHTML(rv$result@peptides[,c(1,2,3,4,5,9,10,11,12,14,17), with=FALSE])
  })

  #########
  ## Proteins view section
  #########

  ## protein selection dynamic ui:
  output$protSelection <- renderUI({
    if(is.null(rv$result)){
      return("Warning: A dataset must be loaded to obtain a choice of identified proteins")
    }
    prots <- subset(rv$result@proteins, expect.value < input$maxExpectProt &
                    num.peptides >= input$minPepNum & like(label, input$protDescFilter))
    prots <- prots[,label]
    selectInput("protSelected", label="Choose a protein:",
                       choices=prots, multiple=TRUE)
  })

  output$tableSelectedProt <- renderText({
    if(is.null(rv$result)){
      return("Warning: A dataset must be loaded to obtain a choice of identified proteins")
    }
    tableAsHTML(rv$result@proteins[label==input$protSelected[[1]],
                                   c(1,2,3,6,7), with=FALSE])
  })

  ### Peptides from selected protein
  output$pepFromSelectProt <- renderText({
    if(is.null(rv$result)){
      return("Warning: A dataset must be loaded to see peptides")
    }
    if(length(input$protSelected)<1){
      return("Warning: You must select a protein to see the associated peptides.")
    }
    selectProt <- rv$result@proteins[label==input$protSelected[[1]], uid]
    tableAsHTML(rv$result@peptides[prot.uid==selectProt, c(2,3,4,5,6,7,9,10,11,12,14,15,16,17), with=FALSE])
  })

  ### Protein coverage
  output$protCoverage <- renderUI({
    if(is.null(rv$result)){
      return("Warning:A dataset must be loaded to see protein coverage.")
    }
    if(length(input$protSelected)<1){
      return("Warning: You must select a protein to see the protein coverage.")
    }
    selectedProt <- rv$result@proteins[label==input$protSelected[[1]],]
    selectedPep <- as.data.frame(rv$result@peptides[ prot.uid==selectedProt[1,uid], ])
    selectedMod <- as.data.frame(rv$result@ptm[pep.id %in% selectedPep$pep.id,])

    sequence <- selectedProt[1,sequence]
    sequence <- gsub("\\s","",sequence)
    seqLength <- nchar(sequence)

    seqVec <- strsplit(sequence, "")[[1]]
    seqKeys <- rep(0, seqLength)
        
    for( i in  1:length(selectedPep[[1]])) {
      seqKeys[ selectedPep[i,]$start.position:selectedPep[i,]$end.position ] <- 1
    }
    for( i in 1:length(selectedMod[[1]])){
      seqKeys[ selectedMod[i,]$at ] <- 2
    }

    for( i in 1:length(seqVec) ) {
      if( seqKeys[[i]] == 1 ){
        seqVec[[i]] <- paste("<a style='color:green; font-weight: bold;'>",
                             seqVec[[i]],
                             "</a>", sep="")
      } else if (seqKeys[[i]] == 2 ){
        seqVec[[i]] <- paste("<a style='color:red; font-weight: bold;'>",
                             seqVec[[i]],
                             "</a>", sep="")
      }
      seqVec[[i]] <- paste("<td>",seqVec[[i]],"</td>", sep="")
      if (i%%10 == 0) {seqVec[[i]]<- paste(seqVec[[i]], "<td style='min-width: 2%;'></td>", sep="")}
      if (i%%50 == 0) {seqVec[[i]] <- paste(seqVec[[i]], "</tr><tr>", sep="")}
     }
    seqVec[[1]] <- paste("<table><tr>",seqVec[[1]], sep="")
    seqVec[[length(seqVec)]] <- paste(seqVec[[length(seqVec)]], "</tr></table>", sep="" )

    sequence2 <- paste(seqVec, sep="")
    HTML(sequence2)
  })

  ######
  ## Stats section.
  ######
  
  output$protExpect <- renderPlot({

    if (is.null(rv$result)) { return(invisible(NULL)) }
    prot.e <- sort(-(rv$result@proteins$expect.value), decreasing=TRUE)
    spm.e <- sort(-log10(rv$result@peptides$expect.value), decreasing=TRUE)

    xaxis <- max(length(prot.e), length(spm.e))
    yaxis <- max(max(prot.e), max(spm.e))
    plot(
      prot.e,
      type="l", lwd=1.5,
      xlim=c(0,xaxis),
      ylim=c(0,yaxis),
      xlab="Number of IDs",
      ylab="-log10(expectation value)",
      col="blue")
    points(spm.e, col="red", type="l")
    max.expect <- 0.01

    if (! is.na(rv$result@used.parameters$`output, maximum valid expectation value`)){
      max.expect <- as.numeric(rv$result@used.parameters$`output, maximum valid expectation value`)
    }
    abline(col="green", h=-log10(max.expect))

    legend("topright",
      legend=c("Protein IDs", "Peptide-spectrum match", "Highest acceptable expectation value\n(as defined in search parameters)"),
      fill=c("red", "blue", "green"), bty="n"
    )
  })
  
   output$chargeDisUI<- renderUI({
    if (is.null(rv$result)) { return(invisible(NULL)) }
    tabs <- list()
    ## Find the charges for which there are at least 3 spectra.
    charges <- table(rv$result@peptides$spectrum.z)
    charges <- names(charges)[charges>2]
    for(i in charges){
      tabTitle <- paste("charge +", i, sep="")
      plotId <- paste("charge", i, sep="")
      tabs <- c(tabs,
        list(
          tabPanel(title=tabTitle,
            plotOutput(outputId=plotId))
        )
      )
     }
     warning(tabs)
     do.call(tabsetPanel,tabs)
   })

  output$charge1 <- renderPlot({
    plotChargeDis(1)
  })
  output$charge2 <- renderPlot({
    plotChargeDis(2)
  })
  output$charge3 <- renderPlot({
    plotChargeDis(3)
  })
  output$charge4 <- renderPlot({
    plotChargeDis(4)
  })
  output$charge5 <- renderPlot({
    plotChargeDis(5)
  })


  plotChargeDis <- function(x) {
    if (is.null(rv$result)) { return(invisible(NULL)) }
    charges <- rv$result@peptides[spectrum.z==x, expect.value]
    charges <- -log10(charges)
    plot(normalmixEM(charges), which=2, xlab2="-Log10(expectation value)")
    lines(density(charges), col="blue", lty=2)
    legend("topright",
      fill=c("red", "green", "blue"),
      legend=c("First fitted distribution", "Second fitted distribution", "Total distribution"),
      y.intersp=1.1,
      bty="n"
    )
  }


  #########
  ## Peptide view section
  #########

  output$pepProtFilter <- renderUI({
    if(is.null(rv$result)){
      return("Warning: A dataset must be loaded to filter peptides by protein")
    }
    selectInput("associatedProt", label="Choose by protein:",
                choices=c("No Filter", rv$result@proteins[,label]),
                selected="No Filter",
                multiple=FALSE
    )
  })

  output$pepPTMFilter <- renderUI({
    if(is.null(rv$result)){
      return("Warning: A dataset must be loaded to filter peptides by PTM")
    }
    choices.ptm <- subset(rv$result@ptm, select=c(type,modified))
    choices.ptm <- apply(choices.ptm, 1, paste, collapse=":")
    choices.ptm <- c("No Filter", unique(choices.ptm))
    selectInput("associatedPTM", label="Choose by PTM:",
                choices=choices.ptm,
                multiple=FALSE,
                selected="No Filter"
    )
  })

  ## peptide selection dynamic ui
  output$pepSelection <- renderUI({
    if(is.null(rv$result)){
      return("Warning: A dataset must be loaded to select peptide.")
    }
    pep.subset <- rv$result@peptides
    
    ## filter by ptm if one is chosen
    if( !is.null(input$associatedPTM) && input$associatedPTM != "No Filter"){
      chosen.type <- strsplit(input$associatedPTM, ":")[[1]][[1]]
      chosen.modified <- strsplit(input$associatedPTM,":")[[1]][[2]]
      chosen.modified <- as.numeric(chosen.modified) # fix problem with leading space
      ptm.subset <- subset(rv$result@ptm, type==chosen.type & modified==chosen.modified, select=pep.id)
      pep.subset <- subset(pep.subset, pep.id %in% ptm.subset[,pep.id])
    }
    
    # filter by protein if one is chosen
    if( !is.null(input$associatedProt) && input$associatedProt != "No Filter") {
      chosen.uid <- subset(rv$result@proteins, label==input$associatedProt, select=uid)[[1]]
      pep.subset <- subset(pep.subset, prot.uid==chosen.uid)
    }
    # Filter by sequence
    pep.subset <- subset(pep.subset, like(sequence, input$pepSeqFilter))
    pep.subset <- pep.subset[order(sequence)]
    pep.subset <- unique(pep.subset[,sequence])
    
    selectInput("pepSelected", label="Choose a peptide:",
                choices=pep.subset, multiple=TRUE)
  })

  output$tableSelectedPep <- renderText({
    if ( is.null(rv$result)){
      return("A dataset must be loaded to see selected peptides.")
    }
    if ( is.null(input$pepSelected) ){
      return("A peptide must be selected")
    }
    
    pep.ids <- rv$result@peptides[sequence==input$pepSelected[[1]], 2, with=FALSE]
    PTMs <- sapply(pep.ids[[1]], function(x){
      ptm.subset<- subset(rv$result@ptm, pep.id==x, select=c(at, type, modified))
      paste(apply(ptm.subset,1,paste,collapse=" "), collapse="; ")
    })
    tableAsHTML(cbind(
      rv$result@peptides[sequence==input$pepSelected[[1]], c(2,1,3,4,5,6,7,9,10,11,12,14,15,16), with=FALSE],
      PTMs
      )
    ) 
  })
  
  output$tableAssociatedProt <- renderText({
    if(is.null(rv$result)){
      return("Warning: A peptide must be selected to see associated proteins")
    }
    if ( is.null(input$pepSelected) ){
      return("A peptide must be selected")
    }
    prot.uids <- rv$result@peptides[sequence==input$pepSelected[[1]], 1, with=FALSE][[1]]
    tableAsHTML(rv$result@proteins[uid %in% prot.uids, c(1,2,3,6,7), with=FALSE])
  })

#  output$theorSpectra <- renderUI({
#    ### placeholder for theoretical spectra
#  })

  output$ms2Spectra <- renderUI({
    if(is.null(rv$result)){
      return("Warning: A dataset must be loaded.")
    }
    if( is.null(input$pepSelected) ) {
      return("A peptide must be selected.")
    }
    spectra <- subset(rv$result@peptides,
                      sequence==input$pepSelected,
                      select=spectrum.id)[[1]]
    spectra <- unique(spectra)
    # Generate a tabset with arbitrary number of panels
    spectra.tabs <-
      lapply(1:length(spectra),
        function(i){tabPanel(
          title=spectra[i], plotOutput(paste("spectra", i, sep="")))
        }
      )
    do.call(tabsetPanel, spectra.tabs)

  })

  ### Generate arbitrary number of ouput$spectra# variables.
  ### To Do: Find a way to bypass the 'eval(parse(paste...)))' syntax
  observe({
    spectra <- NULL
    if( ! is.null(input$pepSelected) ){
      spectra <- unique(subset(rv$result@peptides,
                               sequence==input$pepSelected,
                               select=spectrum.id)[[1]])
    }
    for( i in 1:length(spectra)){
      var.name <- paste("output$spectra", i, sep="")
      eval(parse(text=
        paste(var.name, " <- renderPlot(ms2.plot(", spectra[[i]], ", rv$result))")))
      ## The command evaluated should look like:
      ## output$spectra1 <- renderPlot(ms2.plot(spectra, rv$result))
    }
  })
 
} ##/shinyServer
