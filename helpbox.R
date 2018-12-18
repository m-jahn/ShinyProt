helpbox <- function(width=6) {
  column(width=width, 
    h4('INFO & HELP'),
    wellPanel(
      h4('HOW TO'),
      p('Just use the tree to select subsets of genes and pathways.
        Not every button works as input for every plot, just play around.'),
      h4('DATA AND REFERENCES'),
      p('Proteomics data obtained by labelfree quantification of LC-MS-MS measurements.
        Model organism: Synechocystis PCC6803.
        The data underlying this app was collected for a study by ',
        a(href = 'https://www.cell.com/cell-reports/fulltext/S2211-1247(18)31485-2', target='_blank', 'Jahn et al., Cell Reports, 2018')
      ),
      p('The design of this app was largely inspired by', 
        a(href='http://neuroexpresso.org', target= '_blank', 'neuroexpressor.org')
      ),
      p('The source code for this R shiny app is available on ', 
        a(href='https://github.com/m-jahn', target= '_blank', 'github/m-jahn')
      ),
      #
      h4('CONTACT'),
      p('For questions or reporting issues, contact 
        Michael Jahn, Science For Life Lab - Royal Technical University (KTH), Stockholm'), 
      p(
        a(href='mailto:michael.jahn@scilifelab.se', target= '_blank', 'email: Michael Jahn')
      )
    )
  )
}

fundbox <- function(width=6) {
  column(width=width, 
    wellPanel(
      h4('FUNDING AND OTHER RESOURCES'),
      p('We gratefully acknowledge funding of this study by the following organisations:'),
      tags$ul(
        tags$li('Swedish Foundation For Strategic Research - SSF'), 
        tags$li('Swedish Research Council for Environment, Agricultural Sciences and Spatial Planning - Formas')
      ),
      p(''),
      fluidRow(img(src='formas_logo.png', width='200px')),
      p(''),
      fluidRow(img(src='SSF_logo.png', width='120px'))
    )
  )
}