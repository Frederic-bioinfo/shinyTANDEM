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




## Original function suggested by Joe Cheng.
## tableAsHTML <- function(data) {
##     require('xtable')
##     classNames <- 'data table table-bordered table-condensed'

##     if (is.null(data) || identical(data, data.frame()))
##       return("")
    
##     return(paste(
##       capture.output(
##         print(xtable(data, ...), 
##               type='html', 
##               html.table.attributes=paste('class="',
##                                           htmlEscape(classNames, TRUE),
##                                           '"',
##                                           sep=''), ...)),
##       collapse="\n"))
##   }

