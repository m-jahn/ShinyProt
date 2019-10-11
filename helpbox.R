helpbox <- function(width = 6) {
  column(width = width, 
    h4('INFO & HELP'),
    wellPanel(
      h4('HOW TO'),
      p('Just use the tree to select subsets of genes and pathways.
        Not every button works as input for every plot, just play around.'),
      h4('DATA AND REFERENCES'),
      p('Proteomics data obtained by labelfree quantification of LC-MS-MS measurements.
        Model organism: Synechocystis PCC6803.
        The data underlying this app was collected for a study by ',
        a(href = 'https://www.cell.com/cell-reports/fulltext/S2211-1247(18)31485-2', target ='_blank', 'Jahn et al., Cell Reports, 2018')
      ),
      p('The design of this app was largely inspired by', 
        a(href = 'http://neuroexpresso.org', target = '_blank', 'neuroexpressor.org')
      ),
      p('The source code for this R shiny app is available on ', 
        a(href = 'https://github.com/m-jahn', target = '_blank', 'github/m-jahn')
      ),
      #
      h4('CONTACT'),
      p('For questions or reporting issues, contact 
        Michael Jahn, Science For Life Lab - Royal Technical University (KTH), Stockholm'), 
      p(
        a(href ='mailto:michael.jahn@scilifelab.se', target = '_blank', 'email: Michael Jahn')
      )
    )
  )
}

fundbox <- function(width = 6) {
  column(width = width, 
    wellPanel(
      h4('FUNDING AND OTHER RESOURCES'),
      p('We gratefully acknowledge funding of this study by the following organisations:'),
      tags$ul(
        tags$li('Swedish Foundation For Strategic Research - SSF'), 
        tags$li('Swedish Research Council for Environment, Agricultural Sciences and Spatial Planning - Formas')
      ),
      p(''),
      fluidRow(img(src = 'formas_logo.png', width = '200px')),
      p(''),
      fluidRow(img(src = 'SSF_logo.png', width = '120px'))
    )
  )
}

methbox <- function(width = 6) {
  column(width = width, 
    wellPanel(
      h4('METHODS DETAILS'),
      h4('How protein quantifications were obtained'),
      p('Protein quantity was aggregated from MS-based label-free peptide measurements using the sum
         of all peptide abundances. This relative number was divided by the sum of all protein abundances
         resulting in protein mass fraction (not mol fraction), the main metric used in 
         Jahn et al., Cell Reports, 2018. However this metric can be used, together with some helper
         parameters to convert protein mass in to mol fraction, molecules per cell, 
         molecules oper volume, and others.'
      ),
      h4('How other protein metrics were calculated'),
      
      tags$ul(
        
        tags$li('Protein mass in g/gDCW. This is simply the protein mass fraction (g/g) multiplied by
          protein content per DCW (on average 65%, Touloupakis et al., Biotechnology for Biofuels, 2015)'),
      
        tags$li('Mol fraction of proteins (number of protein as fraction of total protein number).
          Divide mass fraction (g/g) by molar mass (g/mol), and divide every protein mol by sum of mol.'),
      
        tags$li('Protein mass in g/L cell volume. Here is an extra parameter required, the cell volume in 
          L/gDCW (NB: not culture volume!).'),
        
        tags$li('Protein concentration (mol protein / cell). That is protein mass fraction in g/gDCW multiplied
          with gDCW/cell, divided by molar mass in g/mol'),
        
        tags$li('Protein concentration (mol protein / L cell volume). Multiply mol protein per cell with N
          cells per L'),
        
        tags$li('Protein concentration (copies protein / cell). Simply multiply mol/cell by Avogadro constant
          (1/mol)'),
        
        tags$li('Conversion factors needed for transformation:
          1 gDCW/L culture = OD 4 (in-house measurement, Anfelt et al., Mircob Cell Fact, 2015); 
          OD 4 = 25 * 10^10 cells/L (µ = 0.08; Du et al., Algal Research, 2016), so that:
          N (1 gDCW) = 25 * 10^10 cells; 
          V (1 cell) = 9 * 10^-15 L (µ = 0.08; Du et al., Algal Research, 2016); 
          N (1 L cell volume) = 1/(9 * 10^-15) = 1.11111 * 10^14 cells; 
          V (1 gDCW cells) = 25*10^10 cells * 9*10^-15 L/cell = 0.00225 L/gDCW = 2.25*10^-3 L/gDCW'
        )
      )
    )
  )
}

