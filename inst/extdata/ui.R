### This file contains the shiny UI elements.
require(rTANDEM)

navSection <- function(inputId) {      # the navbar is "section", inputId="section"
  tagList(
    singleton(tags$head(tags$script(src = "js/navbar.js"))),
    tags$script(src="js/tooltips.js"),
    tags$script(src="js/bootstrap-fileupload.min.js"),
    tags$link(rel="stylesheet", type="text/css",
              href="css/bootstrap-fileupload.min.css"),
    tags$html(includeHTML('www/navbar.html'))
  )
}

source("./pagedUI.R", local=TRUE)

shinyUI(
  basicPage(
    progressInit(),     
    navSection("section"), ### navbar
      ## Refer all UI components to pagedUI.R with function calls.
      conditionalPanel("input.section == 'home'", home.ui() ),
      conditionalPanel("input.section == 'analysis'", analysis.ui() ),
      conditionalPanel("input.section == 'param'", param.ui() ),
      conditionalPanel("input.section == 'launch'", launch.ui() ),
      conditionalPanel("input.section == 'convert'", convert.ui() ),
      conditionalPanel("input.section == 'load'", load.ui() ),
      conditionalPanel("input.section == 'overview'", overview.ui() ),
      conditionalPanel("input.section == 'stats'", stats.ui() ),
      conditionalPanel("input.section == 'prots'", prots.ui() ),
      conditionalPanel("input.section == 'peps'", peps.ui() ),
      conditionalPanel("input.section == 'external'", external.ui() ),
      conditionalPanel("input.section == 'biomart'", biomart.ui() ),
      conditionalPanel("input.section == 'gominer'", gominer.ui() )
  ) # /pageWithSidebar
) # /shinyUI 
