shinyServer( function(input, output, session) {

  rv <- reactiveValues()

  #######
  ## Load results section
  #######
  
  ### Load result from Rdata
  loadedResult1 <- reactive({
    if( input$loadFromRda > 0) {
      ### Catch error if file cannot be read.
      if( file.access( isolate(input$resultRda), mode=4) == -1 ){
        rv$loadStateIndicator <-
          paste("File: \"", isolate(input$resultRda),"\" cannot be read.")
        return(NULL)
      }

      ## TO-DO: put this in a try-catch to recuperate the error messages yielded if the format is not recognized.
      isolate(load(file=input$resultRda))

      ### TO-DO test for the existence of the named object.
      ### TO-DO test for the class of the named object. (must be rTResult)            

      temp<-isolate(get(input$resultRobj))
      rv$loadStateIndicator <- NULL
      return(temp)
    }
  })
  
  ### Load result from xml
  loadedResult2 <- reactive({
    if( input$loadFromXML > 0) {
      # Test if file can be read.
      if( file.access(isolate(input$resultXML), mode=4) == -1){
        rv$loadStateIndicator <-
          paste("File: \"", isolate(input$resultXML),"\" cannot be read.")
        return(NULL)
      }

      ### To-do: put this in a tryCatch structure
      temp<- isolate(GetResultsFromXML(input$resultXML))
      rv$loadStateIndicator <- NULL
      return(temp)
    }
  })
  
  ### Assign to reactive values:
  observe({ rv$result <- loadedResult1() })
  observe({ rv$result <- loadedResult2() })

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
    }
    div(class="alert alert-success", style="text-align: center;",
        "A dataset was sucessfully loaded!")
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
                    num.peptides > input$minPepNum & like(label, input$protDescFilter))
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
  
  output$mainSection <- renderUI({
    if(!is.null(input$section)) {    
      if(input$section == "home"){
        home.main()
      } else if (input$section == "params"){
        #.main()
      } else if (input$section == "convert"){
        convert.main(input$cbId)
      } else if (input$section == "load"){
        load.main()
      } else if (input$section == "overview"){
        overview.main()
      } else if (input$section == "stats"){
        stats.main()
      } else if (input$section == "prots"){
        prots.main()
      } else if (input$section == "peps"){
        peps.main()
      } else if (input$section == "external"){
        external.main()
      } else if (input$section == "biomart"){
        biomart.main()
      } else if (input$section == "gominer"){
        gominer.main()
      }
    }
  }) ## /output$mainSection
}) ##/shinyServer
 
