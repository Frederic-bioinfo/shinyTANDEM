\name{shinyTANDEM}
\alias{shinyTANDEM}
\title{
  Launch a shiny GUI for rTANDEM
}
\description{
  The function \code{shinyTANDEM(port)} launch a shiny GUI running on
  the given port. The shiny GUI will use the default browser.
  An example dataset is distributed with shinyTANDEM and can be loaded
  by going in the 'Load result' tab and either entering "result.xml" in the
  xml section, or "result.Rda" and "result" in the R-data section.
}

\usage{
shinyTANDEM(port=8100,...)
}

\arguments{
  \item{port}{
    Shiny's default port is 8100. The port parameter lets you specify a
    different one.
  }
  \item{...}{
    Arguments can be passed to shiny::runApp().
  }
}
\value{
  This function does not return. Interrupt R to stop the GUI (usually by
  pressing Ctrl+C or Esc).
}
\author{
  Authors: Frederic Fournier <frederic.fournier@crchuq.ulaval.ca>
  Maintainer: Frederic Fournier <frederic.fournier@crchuq.ulaval.ca>
}
\examples{
# To start the GUI:
\dontrun{shinyTANDEM()}
# To see an example, go to the 'Load result' tab and either
# enter 'result.xml' in the xml section, or 'result.Rda' and 'result'
# in the R-data section.
}
