home.ui <- function() {
  div(
      style="min-height=100%",
      tags$img(src="www/images/rTANDEM_logo.png",
               style="position:absolute;width:100%;margin-left:-20px;padding:7% 0 0 0;",
               alt="rTANDEM logo")
  )
}

analysis.ui <- function() {
  div(
    h2("... Work in progress ..."),
    p("This page will hold the welcome page for the analysis section. Please look for the next version of shinyTANDEM in the development section of Bioconductor.")
    )
}

param.ui <- function() {
  div(
    h2("... Work in progress ..."),
    p("This page will hold the welcome page for the analysis section.
Please look for the next version of shinyTANDEM in the development section of Bioconductor.")
  )
  ## div(
  ##   h3("Create Parameter Objects",
  ##      style="text-align: center;", rel="tooltip",
  ##      title="Test in tooltip"
  ##   ),
  ##   div(class="row-fluid",
  ##     div(class="span3",
  ##       div(class="well",  
  ##         radioButtons("typeOfParam",
  ##                      "Type of parameter object:",
  ##                      choices=c("Analysis centered", "Instrument defaults",
  ##                        "Full", "Taxonomy"),  
  ##                      selected=("Analysis centered")
  ##                      )
  ##       ) ## /well (first column)
  ##     ), ## /span3 (first column)

  ##     div(class="span9",
  ##       div(class="well",

  ##         ### Analysis centered parameter object  
  ##         conditionalPanel("input.typeOfParam == 'Analysis centered'",
  ##           h4("Build an analysis centered parameter object", style="text-align: center;"),
  ##           div(rel="tooltip",
  ##               title="Input a coma-separated list of the spectra files (with their full path).",
  ##               textInput(inputId="spectrum, path",
  ##                         label="Spectra file(s):")
  ##           ),
  ##           div(rel="tooltip",
  ##               title="Input the path of the taxonomy.xml file",
  ##               textInput(inputId="list path, taxonomy information",
  ##                         label="Taxonomy file:")
  ##           ),
  ##           div(rel="tooltip",
  ##               title="Input the taxon to be used (as it is named in the taxonomy file).",
  ##               textInput(inputId="protein, taxon",
  ##                         label="Taxon:")
  ##           ),
  ##           div(rel="tooltip",
  ##               title="Input the path to the output folder.",
  ##               textInput(inputId="output, path",
  ##                         label="Path to output folder:")
  ##           ),
  ##           div(rel="tooltip",
  ##               title="Input the path to the default parameter object.",
  ##               textInput(inputId="list path, default parameters",
  ##                         label="Path to the default parameter object:")
  ##           ),
  ##           div(rel="tooltip",
  ##               title="Give a name to the R object or the .xml file to be created.",
  ##               textInput(inputId="paramName",
  ##                         label="Name of the parameter:")
  ##           )         
               
                 
  ##         ),##/conditionalPan (analysis centered)    

  ##         ### Instrument centered parameter object.
  ##         conditionalPanel("input.typeOfParam == 'Instrument defaults'",
  ##           h4("Instrument defaults parameter object", style="text-align: center;")
  ##         ),    

  ##         ### Full parameter object
  ##         conditionalPanel("input.typeOfParam == 'Full'",
  ##           h4("Full parameter objects", style="text-align: center;")
  ##         ),

  ##         ### Taxonomy
  ##         conditionalPanel("input.typeOfParam == 'Taxonomy'",
  ##           h4("Taxonomy", style="text-align: center;")
  ##         )
            
  ##       ), ### /well (second column)
  ##       div(class="well",
  ##           textOutput("paramSummary"),
  ##           textOutput("paramSummary2")
  ##           ) ##/well (output)
  ##     ) ### /span9
  ##   ), ### /row-fluid
    
  ##   div(style="text-align: center;",div(style="display: inline-block;",
  ##     actionButton("createRParam", "Create R parameter object"),
  ##     actionButton("createXMLParam", "Save as an xml object")
  ##   ))
  ## ) ## /div (main)
}

launch.ui <- function() {
  div(
    h2("... Work in progress ..."),
    p("This page will allow you to launch rTANDEM analysis. Please look for the next version of shinyTANDEM in the development section of Bioconductor.")
    )
}

convert.ui <- function() {
  div(
    h2("... Work in progress ..."),
    p("This page will allow you to convert R object to xml file and vice versa. Please look for the next version of shinyTANDEM in the development section of Bioconductor.")
    )
}

load.ui <- function(){
  div(
    div(class="well", style="text-align: center;",
      h4("Select either an XML result file or an .RDS file and the name of the result object to load a search.")
    ),
    div(class="row-fluid",
      div(class="span6",
        div(class="well",
          h5("Load a result object from a RDS file.", style="text-align: center;"),
          #### jasny bootstrap file selector:
          div(class="fileupload fileupload-new", 'data-provides'="fileupload",
            div(class="input-append",
              div(class="uneditable-input span5",
                tag("i", list(class="icon-file fileupload-exists")),
                span(class="fileupload-preview")
              ),
              span(class="btn btn-file",
                span(class="fileupload-new", "Select file"),
                span(class="fileupload-exists", "Change"),
                tag("input", list(id="resultRDS", type="file"))
                ),
              tag("a", list(href="#", class="btn fileupload-exists",
                   'data-dismiss'="fileupload", "Remove")),
              ### shiny progress bar.
              div(id="resultRDS_progress",
                  class="progress progress-striped shiny-file-input-progress",
                div(class="bar"), tags$label()
              )
            )
          ), 
          div(style="text-align: center;", div(style="display: inline-block;",
            actionButton("loadFromRDS", "Charge object into memory")
          ))
        )#/well 
      ),#/span6
      div(class="span6",
        div(class="well",
          h5("Load a search from an XML result file.", style="text-align: center;"),  
          #### jasny bootstrap file selector:
          div(class="fileupload fileupload-new", 'data-provides'="fileupload",
            div(class="input-append",
              div(class="uneditable-input span5",
                tag("i", list(class="icon-file fileupload-exists")),
                span(class="fileupload-preview")
              ),
              span(class="btn btn-file",
                span(class="fileupload-new", "Select file"),
                span(class="fileupload-exists", "Change"),
                tag("input", list(id="resultXML", type="file"))
                ),
              tag("a", list(href="#", class="btn fileupload-exists",
                   'data-dismiss'="fileupload", "Remove")),
              ### shiny progress bar.
              div(id="resultXML_progress",
                  class="progress progress-striped shiny-file-input-progress",
                div(class="bar"), tags$label()
              )
            )
          ), 
          div(style="text-align: center;", div(style="display: inline-block;",
             actionButton("loadFromXML", "Parse XML file into memory")
          ))
        )#/well
      )#/span6
    ),#/row
    uiOutput("loadStateIndicator"),
    uiOutput("loadedDataset")
  )#/main div
}

overview.ui <- function() {
  div(
    div(class="well",
      h3("Analysis overview"),
      div( style="max-height: 350px; overflow: auto;",
        htmlOutput("overviewAnalysis")
      )
    ),
    div(class="well",
      h3("Identified proteins"),
      div(style="max-height: 350px; overflow: auto;",
        htmlOutput("overviewProteins")
      )
    ),
    div(class="well",
      h3("Identified peptides"),
      div(style="max-height: 350px; overflow: auto;",
        htmlOutput("overviewPeptides")
      )
    )
  )
}

prots.ui <- function() {
  div(
    div(class="well",
      h3("Protein selection:"),
      div(class="row-fluid",
        div(class="span4",
          numericInput(inputId="maxExpectProt",
                    label="Max. expectation value:", value=0.05)
        ),
        div(class="span4",
          numericInput(inputId="minPepNum",
                    label="Minimum # of peptides:", value=2, min=1, max=1000, step=1)
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
      htmlOutput("tableSelectedProt"),
      h3("Protein coverage"),
      div(style="max-height: 350px; overflow: auto;",
        htmlOutput("protCoverage")
      ),
      h3("Peptides from selected protein"),
      div(style="max-height: 350px; overflow: auto;",
          htmlOutput("pepFromSelectProt")
      )
        
    )#/well Selected Prot
  )#/div prots.ui
}

peps.ui <- function() {
  div(
    div(class="well",              # Peptide selection
      h3("Peptide selection"),
      div(class="row-fluid",       # Row for filters
        div(class="span4",
          textInput(inputId="pepSeqFilter",
                    label="Filter by sequence of peptide:")
        ),
        div(class="span4",
          uiOutput("pepProtFilter")
        ),
        div(class="span4",
          uiOutput("pepPTMFilter")
        )
      ), #/row for filters
      uiOutput("pepSelection"),
      tags$style(type='text/css', "#pepSelected {height:200px; width: 100%;}")
    ), #/well peptide selection
    div(class="well", # Peptide visualization
      h3("Selected peptide"),
      htmlOutput("tableSelectedPep"),
      h3("Associated proteins"),
      htmlOutput("tableAssociatedProt"),
      div(class="row-fluid",
       # div(class="span6",              ## Place holder for theoretical spectra
       #   h3("Theoretical spectra"),
       #   uiOutput("theorSpectra")
       # ),
        div(class="span6",
          h3("MS2 spectra"),
          uiOutput("ms2Spectra")
        )
      ) #/ row-spectra
    ) #/well Peptide visualization
  )
}

external.ui <- function() {
  h3("This will be the homepage for external functions")
}

biomart.ui <- function() {
  div(
    h2("... Work in progress ..."),
    p("This page will allow you to use biomaRt for quick cross-references.
Please look for the next version of shinyTANDEM in the development section of Bioconductor.")
  )

}

gominer.ui <- function() {
  div(
    h2("... Work in progress ..."),
    p("This page will allow you to get GO annotation on selected groups of proteins.
Please look for the next version of shinyTANDEM in the development section of Bioconductor.")
  )
}
