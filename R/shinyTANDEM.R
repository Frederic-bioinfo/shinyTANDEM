shinyTANDEM <- function(port=8100,...) {
  runApp(appDir=system.file("extdata", package="shinyTANDEM"),
         port=port
         )
}
