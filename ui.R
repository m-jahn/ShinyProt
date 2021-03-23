#
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
        # -------------------
        # select data file
        h4("DATA OPTIONS"),
        
        fluidRow(
          column(width = 6,
            selectInput("UserDataChoice",
              "Choose data:", data_list, 
              selected = data_list[1])
          ),
          column(width = 6,
            uiOutput("UserFilters")
          )
        ),
        
        # SELECT PLOT OPTIONS
        # -------------------
        
        hr(),
        h4("PLOT OPTIONS"),
        fluidRow(
          
          # FACTORIZATION AND ORDERING OPTIONS
          column(width = 4,
            uiOutput("UserXVariable")
          ),
          column(width = 4, 
            uiOutput("UserYVariable")
          ),
          column(width = 4,
            uiOutput("UserCondVariable")
          )
        ),
        
        fluidRow(
          
          # DATA GROUPING OPTIONS
          column(width = 3,
            uiOutput("UserTheme")
          ),
          column(width = 3,
            uiOutput("UserGrouping")
          ),
          column(width = 3,
            uiOutput("UserPlotType")
          ),
          column(width = 3,
            uiOutput("UserLogY")
          )
        ),
        
        # OTHER GRAPHICAL PLOT OPTIONS
        fluidRow(
          
          # PANEL LAYOUT AND PLOT DIMENSIONS
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
        
        hr(),
        fluidRow(
          
          # SELECT GENES OR PROTEINS FROM TREE
          column(width = 6, 
            h4("GENE SELECTION"),
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
              downloadButton("UserDownloadHeatmap", "Download svg")
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
    column(width = 4,
      helpbox(width = 12),
      fundbox(width = 12),
    ),
    column(width = 8,
      methbox(width = 12)
    )
  )

)