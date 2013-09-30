dataset.bool <- FALSE
dataset.obj <- NULL

shinyTANDEM <- function(dataset=NULL, port=8100,...) {
  
  if( !is.null(dataset)) {
    if (! "rTResult" %in% class(dataset)) {
      stop("The dataset object must be a rTANDEM result object of class='rTResult'.")
    } else {
      
      ## Use tempfile to be plateform independant.
      filename <- tempfile("sessionDataset.Rds")
      ### But remove the randomized ending so the shinyApp can access the same file.
      filename <- gsub("sessionDataset.Rds.*", "sessionDataset.Rds", filename)
      saveRDS(dataset, filename)
      on.exit(unlink(filename), add=TRUE)
    }
  }
  
  runApp(appDir=system.file("extdata", package="shinyTANDEM"),
    port=port
  )
}

