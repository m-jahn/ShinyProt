# 
# LOADING LIBRARIES
# ***********************************************
library(shiny)
library(lattice)
library(latticeExtra)
library(tidyr)
library(plyr)
library(shinyTree)
library(shinythemes)
library(dendextend)


# LOADING EXTERNAL FUNCTIONS AND DATA
# ***********************************************
for (Rfile in list.files(
  pattern = "(functions|themes|box)\\.R$", full.names = TRUE)) {
  source(Rfile)
}

# list of data files
datalistfiles <- list.files("data", pattern = "\\.csv$", full.names = TRUE)


# SHINY UI
# ***********************************************
# Define user interface for application
ui <- navbarPage(
  
  # Title on NavBar Header
  title = "ShinyProt - interactive gene expression analysis",
  
  # Use one of different shiny themes
  theme = shinytheme("cosmo"),
  #shinythemes::themeSelector(),
  
  tabPanel("App",
    
    # Sidebar
    sidebarLayout(
      
      # HERE COME ALL CONTROLS FOR THE SIDEBAR PANEL
      sidebarPanel(width = 5,
        
        # SELECT DATA
        selectInput("UserDataChoice",
          "Choose data:", datalistfiles, 
          selected = datalistfiles[1]),
        
        # SELECT PLOT OPTIONS
        hr(),
        h4("PLOT OPTIONS"),
        fluidRow(
          
          # PANEL LAYOUT AND PLOT DIMESNIONS
          column(width = 4, 
            selectInput("UserPanelLayout", 
              "Panel layout:", choices = list("automatic", "manual"),
              selected = "automatic")
          ),
          column(width = 2, 
            conditionalPanel(condition = "input.UserPanelLayout == 'manual'",
              numericInput("UserPanelLayoutCols", 
                "Columns:", value = 4)
            )
          ),
          column(width = 2, 
            conditionalPanel(condition = "input.UserPanelLayout == 'manual'",
              numericInput("UserPanelLayoutRows", 
              "Rows:", value = 4)
            )
          ),
          column(width = 2, 
            selectInput("UserPrintHeight",
              "Plot height:", choices = c(1:10*100), selected = 700)
          ),
          column(width = 2, 
            selectInput("UserPrintWidth",
              "Plot width:", choices = c("auto", 1:10*100), selected = "auto")
          )
        ),
        
        fluidRow(
          
          # FACTORIZATION AND ORDERING OPTIONS
          column(width = 4, 
            selectInput("UserXVariable", 
              "X variable:", choices = c("protein","condition","growthrate",
                "psortB_localization","Process","Pathway","Protein"),
              selected = "condition")
          ),
          column(width = 4, 
            selectInput("UserYVariable", 
              "Y variable:", choices = list("rel_intensity", "mean_intensity", "median_intensity", 
                  "mean_mass_fraction_norm", "mass_g_per_gDCW", "mol_fraction", "conc_g_per_L_cell_vol",
                  "conc_mol_per_cell", "conc_mol_per_L_cell_vol", "conc_copies_per_cell"),
              selected = "rel_intensity")
          ),
          column(width = 4, 
            selectInput("UserCondVariable", 
              "Conditioning variable:", choices = list("protein","condition","growthrate",
                "psortB_localization","Process","Pathway","Protein"),
              selected = "protein")
          )
        ),
        
        fluidRow(
          
          # OTHER GRAPHICAL PLOT OPTIONS
          column(width = 3, 
            selectInput("UserTheme", 
              "Theme:", choices = list("lattice grey", "lattice blue", "ggplot1", "ggplot2"),
              selected = "lattice grey")
          ),
          column(width = 3, 
            selectInput("UserGrouping", 
              "Color coding:", choices = list("none", "by conditioning", "by X variable", "by Y variable"),
              selected = "by conditioning")
          ),
          column(width = 3, 
            selectInput("UserPlotType", 
              "Plot type:", choices = list("points", "lines", "points and lines"),
              selected = "points")
          ),
          column(width = 3, 
            selectInput("UserLogY", 
              "Y scale:", choices = list("linear","log 2","log 10", "log e"),
              selected = "log 2")
          )
        ),
        
        
        hr(),
        fluidRow(
          
          # SELECT GENES OR PROTEINS FROM TREE
          column(width = 6, 
            h4("PROTEIN SELECTION"),
            shinyTree("tree", search = TRUE, checkbox = TRUE)
          ),
          
          # INFO BOX WITH CONTACT AND REFERENCES (EXTERNAL)
          helpbox(width = 6)
          
        )
      ),
      
      
      # MAIN PANEL WITH OUTPUT PLOTS
      # Each tab has individual Download buttons
      column(width = 7,
        wellPanel(
          tabsetPanel(
            tabPanel("DOT PLOT", uiOutput("dotplot.ui"),
              downloadButton("UserDownloadDotplot", "Download svg")
            ),
            tabPanel("BOX PLOT", uiOutput("barchart.ui"),
              downloadButton("UserDownloadBoxplot", "Download svg")
            ),
            tabPanel("VIOLIN PLOT", uiOutput("violinplot.ui"),
              downloadButton("UserDownloadViolinplot", "Download svg")
            ),
            tabPanel("HEAT MAP", uiOutput("heatmap.ui"),
              downloadButton("UserDownloadHeat", "Download svg")
            ),
            tabPanel("CLUSTERING", uiOutput("clustering.ui"),
              numericInput("UserNClust", label = "N cluster", value = 4, step = 1)
            ),
            tabPanel("TABLE", uiOutput("table.ui"),
              downloadButton("UserDownloadTable", "Download table")
            )
          )
        )
      )
    )
  ),
  
  # THE ABOUT PAGE
  tabPanel("About", 
    # THE SAME OR A BIT EXTENDED HELP BOX AS IN SIDEBAR
    helpbox(width = 8),
    fundbox(width = 8),
    methbox(width = 8)
  )

)