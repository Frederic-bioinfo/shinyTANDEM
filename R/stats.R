stats.ui <- function() {
  div(
    div(class="span6",
        h3("Protein IDs and peptide-spectrum matches ranked by expectation value"),
        tabsetPanel(tabPanel(title="IDs",
        plotOutput("protExpect"),
        helpText("")
        ))
    ),
    div(class="span6",
      h3("Density curves of matches by score according to spectra charges"),
      uiOutput("chargeDisUI"), # reactive UI of the distributions by charge
      helpText("NormalMixEM algorithm was used to fit two distributions on the density curve of peptide-spectrum-matches by score.") 
    )
  ) #/ end encompassing div
}
