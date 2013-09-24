shinyTANDEM <- function(port,...) {
  runApp(appDir=system.file("extdata", package="shinyTANDEM"),
         port=port
         )
}
