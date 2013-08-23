# This page contains the shiny server-side functions (return outputs)
source("./pagedServer.R", local=TRUE)

shinyServer( function(input, output, session) {

  ### Load result from Rdata
  loadedResult <- reactive({
    input$loadFromRda
    isolate(load(input$resulRda))
    isolate(return(input$Robj))
  })

  ### Load result from xml
  loadedResult <- reactive({
    isolate(input$loadFromXML)
    isolate(GetResultsFromXML(input$resultXML))
  })

  ### Display analysis overview
  output$overviewAnalysis <- renderTable({
    params <- loadedResult()@used.parameters
      
    data.frame(
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
        loadedResult()@xtandem.version,
        loadedResult()@start.time,
        params$"spectrum, path",
        params$"protein, taxon",
        loadedResult()@sequence.source.paths,
        params$'protein, cleavage site',
        length(loadedResult()@proteins[,uid]),
        length(loadedResult()@peptides[,pep.id]),
        paste(loadedResult()@total.spectra.assigned,
              loadedResult()@nb.input.spectra, sep="/"),
        params$"residue, modification mass",
        params$"residue, potential modification mass"
        )
     )
   })

  ### Display protein overview
  output$overviewProteins <- renderTable({
    loadedResult()@proteins[,c(1,2,3,6,7),with=FALSE]
  })
  ### Display protein overview
  output$overviewPeptides <- renderTable({
    loadedResult()@peptides[,c(1,2,3,4,5,9,10,11,12,14,17), with=FALSE]
  })

  #########
  ## Protein page
  #########

  ## protein selection dynamic ui:
  output$protSelection <- renderUI({
    prots <- subset(loadedResult()@proteins, expect.value < input$maxExpectProt &
                    num.peptides > input$minPepNum & like(label, input$protDescFilter))
    prots <- prots[,label]
    selectInput("protSelected", label="Choose a protein:",
                       choices=prots, multiple=TRUE)
  })

  output$tableSelectedProt <- renderTable({
    loadedResult()@proteins[label==input$protSelected[[1]],
                            c(1,2,3,6,7), with=FALSE]
  })

  output$pepFromSelectProt <- renderTable({
    selectProt <- loadedResult()@proteins[label==input$protSelected[[1]], uid]
    loadedResult()@peptides[prot.uid==selectProt, c(2,3,4,5,6,7,9,10,11,12,14,15,16,17), with=FALSE]
  })

  output$protCoverage <- renderUI({
    selectedProt <- loadedResult()@proteins[label==input$protSelected[[1]],]
    selectedPep <- as.data.frame(loadedResult()@peptides[ prot.uid==selectedProt[1,uid], ])
    selectedMod <- as.data.frame(loadedResult()@ptm[pep.id %in% selectedPep$pep.id,])

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
 
