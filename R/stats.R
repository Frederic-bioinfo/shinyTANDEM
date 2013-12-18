stats.ui <- function() {
  div(
    div(class="span6",
        h3("Protein IDs and peptide-spectrum matches ranked by expectation value"),
        uiOutput("protExpectUI")
    ),
    div(class="span6",
      h3("Density curves of matches by score according to spectra charges"),
      uiOutput("chargeDisUI")#, # reactive UI of the distributions by charge
    )
  ) #/ end encompassing div
}
