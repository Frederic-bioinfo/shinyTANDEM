home.main <- function() {
}

launch.main <- function() {
  h2("Build or load input.xml / Launch analysis from homepage.R")
}

convert.main <- function(cbId) {
  conditionalPanel(TRUE,
                   h1("Convert main panel from pagedServer.R"),
                   h2("This page will hold functions to convert between xml and R objects"),
                   conditionalPanel(cbId, h2("The checkbox is checked!")),
                   conditionalPanel(!cbId, h2("You unchecked it :( ")),
                   h4("And this is a last row.")
                   )

}
load.main <- function(){
  h2("This page will load a rTANDEM result object.")
}

overview.main <- function() {
  h2("This page will show an overview of the results.")
}

stats.main <- function() {
  h2("If we find meaningful statistics to show about the result, this will be the place to show them")
}
prots.main <- function() {
  h2("This will be one of the main page of the GUI: it should show the proteins, with their score, sequence, covering, etc.")
}

peps.main <- function() {
  h2("For a closer look at some peptides.")
}

external.main <- function() {
  h2("Let's get out of rTANDEM!")
}

biomart.main <- function() {
  h2("Use biomart to get more info on proteins.")
}
gominer.main <- function() {
  h2("Find something to do with GO terms.")
}
 
 
