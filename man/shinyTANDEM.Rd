\name{shinyTANDEM}
\alias{shinyTANDEM}
\title{
  Launch a shiny GUI for rTANDEM
}
\description{
  The function \code{shinyTANDEM(dataset, port)} launch a shiny GUI running on
  the given port. The shiny GUI will use the default browser. A rTANDEM
  result object can be passed as an argument. In this case, the dataset
  will be loaded as the GUI opens.
  The dataset can be changed later on by loading another dataset either
  from an xml or an Rdata file. If a dataset is loaded from an Rda file
  that contains more than one object, the name of the object to be
  loaded must be specified. 
}

\usage{
shinyTANDEM(dataset=NULL, port=8100,...)
}

\arguments{
  \item{dataset}{An optional rTANDEM result object (class rTResult) that will be
    loaded as the GUI opens.
  }
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
  Authors: Frederic Fournier <frederic.fournier@crchuq.ulaval.ca>,
  Arnaud Droit <arnaud.droit@crchuq.ulaval.ca>
  Maintainer: Frederic Fournier <frederic.fournier@crchuq.ulaval.ca>
}
\examples{
# The package contains a Rda file with a rTResult object
# simply named 'result'.
# Let's load this object and pass it to shinyTANDEM GUI. To exit the
# GUI, press your interupt command (usually Ctrl+C or Esc) in the R window.

\donttest{load(system.file("extdata/result.Rda", package="shinyTANDEM")}
\donttest{if(interactive()){shinyTANDEM(dataset=result)}}

# Once the GUI is launched, datasets can be loaded in the 'Load results' tab.
# The example dataset that is packaged with shinyTANDEM can be loaded
# through the xml chanel (use xml file='result.xml') or through the
# Rda chanel (use rda file='result.Rda' and object='result').
}
