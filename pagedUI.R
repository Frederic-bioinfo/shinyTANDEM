home.ui <- function() {
  div(
    h3("This page will contain a rTANDEM logo. In the meantime..."),
    tags$div(style="max-width:50%",
      tags$img(src="images/Rlogo.png",
               alt="Rlogo")
    )
  )
}

analysis.ui <- function() {
  h3("This page will hold the welcome page for the analysis section")
}

param.ui <- function() {
  div(
    h3("Create Parameter Objects",
       style="text-align: center;", rel="tooltip",
       title="Test in tooltip"
    ),
    div(class="row-fluid",
      div(class="span3",
        div(class="well",  
          radioButtons("typeOfParam",
                       "Type of parameter object:",
                       choices=c("Analysis centered", "Instrument defaults",
                         "Full", "Taxonomy"),  
                       selected=("Analysis centered")
                       )
        ) ## /well (first column)
      ), ## /span3 (first column)

      div(class="span9",
        div(class="well",

          ### Analysis centered parameter object  
          conditionalPanel("input.typeOfParam == 'Analysis centered'",
            h4("Build an analysis centered parameter object", style="text-align: center;"),
            div(rel="tooltip",
                title="Input a coma-separated list of the spectra files (with their full path).",
                textInput(inputId="spectrum, path",
                          label="Spectra file(s):")
            ),
            div(rel="tooltip",
                title="Input the path of the taxonomy.xml file",
                textInput(inputId="list path, taxonomy information",
                          label="Taxonomy file:")
            ),
            div(rel="tooltip",
                title="Input the taxon to be used (as it is named in the taxonomy file).",
                textInput(inputId="protein, taxon",
                          label="Taxon:")
            ),
            div(rel="tooltip",
                title="Input the path to the output folder.",
                textInput(inputId="output, path",
                          label="Path to output folder:")
            ),
            div(rel="tooltip",
                title="Input the path to the default parameter object.",
                textInput(inputId="list path, default parameters",
                          label="Path to the default parameter object:")
            ),
            div(rel="tooltip",
                title="Give a name to the R object or the .xml file to be created.",
                textInput(inputId="paramName",
                          label="Name of the parameter:")
            )         
               
                 
          ),##/conditionalPan (analysis centered)    

          ### Instrument centered parameter object.
          conditionalPanel("input.typeOfParam == 'Instrument defaults'",
            h4("Instrument defaults parameter object", style="text-align: center;")
          ),    

          ### Full parameter object
          conditionalPanel("input.typeOfParam == 'Full'",
            h4("Full parameter objects", style="text-align: center;")
          ),

          ### Taxonomy
          conditionalPanel("input.typeOfParam == 'Taxonomy'",
            h4("Taxonomy", style="text-align: center;")
          )
            
        ), ### /well (second column)
        div(class="well",
            textOutput("paramSummary"),
            textOutput("paramSummary2")
            ) ##/well (output)
      ) ### /span9
    ), ### /row-fluid
    
    div(style="text-align: center;",div(style="display: inline-block;",
      actionButton("createRParam", "Create R parameter object"),
      actionButton("createXMLParam", "Save as an xml object")
    ))
  ) ## /div (main)
}

launch.ui <- function() {
  h3("This page will hold the launch functions for running analysis")
 }

convert.ui <- function() {
  h3("This page will hold the various conversion functions...")
}

load.ui <- function(){
  div(
    div(class="well", style="text-align: center;",
      h4("Select either an XML result file or an .Rda file and the name of the result object to load a search.")
    ),
    div(class="row-fluid",
      div(class="span8",
        div(class="well",
          h5("Load a search from a R object from a Rdata file.", style="text-align: center;"),
          div(class="row-fluid",
            div(class="span6",
              textInput(inputId="resultRda",               
                        label="Choose a Rdata file (.Rda):"
              )
            ),
            div(class="span6",
              textInput(inputId="resultRobj",
                        label="Choose the rTResult object:"
              )
            )  
          ),#/row
          div(style="text-align: center;", div(style="display: inline-block;",
            actionButton("loadFromRda", "Load result from Rdata")
          ))
        )#/well 
      ),#/span8
      div(class="span4",
        div(class="well",
          h5("Load a search from an XML result file.", style="text-align: center;"),  
          textInput(inputId="resultXML",
                    label="Choose an XML result file:"
          ),
          div(style="text-align: center;", div(style="display: inline-block;",
             actionButton("loadFromXML", "Load result from XML")
          ))
        )#/well
      )#/span4
    )#/row
  )#/main div
}

overview.ui <- function() {
  div(
    div(class="well",
      h3("Analysis overview"),
      div( style="max-height: 350px; overflow: auto;",
        tableOutput("overviewAnalysis")
      )
    ),
    div(class="well",
      h3("Identified proteins"),
      div(style="max-height: 350px; overflow: auto;",
        tableOutput("overviewProteins")
      )
    ),
    div(class="well",
      h3("Identified peptides"),
      div(style="max-height: 350px; overflow: auto;",
        tableOutput("overviewPeptides")
      )
    )
  )
}

stats.ui <- function() {
  h3("This page will provide statistics about the loaded results.")
}

prots.ui <- function() {
  div(
    div(class="well",
      h3("Protein selection:"),
      div(class="row-fluid",
        div(class="span4",
          numericInput(inputId="maxExpectProt",
                    label="Max. expectation value:", value=100)
        ),
        div(class="span4",
          numericInput(inputId="minPepNum",
                    label="Minimum # of peptides:", value=1, min=1, max=1000, step=1)
        ),
        div(class="span4",
          textInput(inputId="protDescFilter",
                    label="Filter by description of the protein:")
        )
      ),#/row
      uiOutput("protSelection"),
      tags$style(type='text/css', "#protSelected { height: 200px; width: 100%;}")
    ),#/well protein selection
    div(class="well",
      h3("Selected protein"),
      tableOutput("tableSelectedProt"),
      h3("Peptides from selected protein"),
      div(style="max-height: 350px; overflow: auto;",
          tableOutput("pepFromSelectProt")
      ),
      h3("Protein coverage"),
      div(style="max-height: 350px; overflow: auto;",
        htmlOutput("protCoverage")
      )

    )#/well Selected Prot
  )#/div prots.ui
}

peps.ui <- function() {
  h3("This page will allow to get a closer look at specific peptide IDs.")
}

external.ui <- function() {
  h3("This will be the homepage for external functions")
}

biomart.ui <- function() {
  sidebarPanel(
    h4("Use biomaRt for cross references"),
    textInput("idXref", label="ID cross-reference")
  )
}

gominer.ui <- function() {
  h3("This page will provide a GO-information on selected proteins.")
}

