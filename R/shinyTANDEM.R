shinyTANDEM <- function(dataset=NULL, port=8100,...) {
  options(shiny.maxRequestSize=1024^3) #allows uploading 1Gb file.
  addResourcePath(prefix="www", directoryPath=system.file("extdata/www/", package="shinyTANDEM"))
  
  if( !is.null(dataset)) {
    if (! is(dataset, "rTResult") ) {
      stop("The dataset object must be a rTANDEM result object of class='rTResult'.")
    }
  }

  #redefine the env of shinyTandemServer to the calling environment so that
  #shinyTandemServer() has access to the 'dataset' object.
  environment(shinyTandemServer) <- environment() 

  app <- list(
    ui= shinyUI(),
    serverFunct=shinyTandemServer,
    port=port)
    
  runApp(app)
}
