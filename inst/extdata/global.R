# This file contains variables and fonctions that are accessable through all sessions for the GUI.

## This function was suggested by Joe Cheng
## It is used to bypass the difficulty of rendering a text message in a renderTable.
## Instead of a renderTable, we use an htmlOutput with renderText, and
## we add tableAsHTML(return_value) when needed to get the behaviour of renderTable.
tableAsHTML <- function(data) {
  require('xtable')
  classNames <- 'data table table-bordered table-condensed'
  
  if (is.null(data) || identical(data, data.frame()))
    return("")
  
  return(paste(
    capture.output(
      print(xtable(data), 
        type='html', 
        html.table.attributes=paste('class="',
          classNames, 
          '"',
          sep='')
        )
      ),
      collapse="\n")#/paste
  )#/return
}#/tableAsHTML




### Code for progress from shiny-incubator (2013-10-04)

#' Initialize progress
#' Call this function in your \code{shinyUI} definition if you intend
#' to use progress in \code{server.R}.
progressInit <- function() {
#  addResourcePath('progress', system.file('progress',
#                                          package='shinyIncubator'))
  tagList(
    singleton(
      tags$head(
        tags$script(src='js/progress.js'),
        tags$link(rel='stylesheet', type='text/css',
                  href='css/progress.css')
      )
    )
  )
}

# Progress calss
Progress <- setRefClass(
  'Progress',
  fields = list(
    .session = 'ANY',
    .id = 'character',
    .min = 'numeric',
    .max = 'numeric',
    .closed = 'logical'
  ),
  methods = list(
    initialize = function(session, min = 0, max = 1) {
      .closed <<- FALSE
      .session <<- session
      .id <<- paste(as.character(as.raw(runif(8, min=0, max=255))), collapse='')
      .min <<- min
      .max <<- max
      
      .session$sendCustomMessage('shiny-progress-open', list(id = .id))
    },
    set = function(message = NULL, detail = NULL, value = NULL) {
      if (.closed) {
        # TODO: Warn?
        return()
      }

      data <- list(id = .id)
      if (!missing(message))
        data$message <- message
      if (!missing(detail))
        data$detail <- detail
      if (!missing(value)) {
        if (is.null(value) || is.na(value))
          data$value <- NULL
        else {
          data$value <- min(1, max(0, (value - .min) / (.max - .min)))
        }
      }

      .session$sendCustomMessage('shiny-progress-update', data)
    },
    close = function() {
      if (.closed) {
        # TODO: Warn?
        return()
      }

      .session$sendCustomMessage('shiny-progress-close',
                                 list(id = .id))
    }
  )
)
.currentProgress <- new.env()

