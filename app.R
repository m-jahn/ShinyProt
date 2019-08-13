# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above. Find out more about building applications here:
# http://shiny.rstudio.com/



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
source("custom.themes.R")
source("custom.panel.functions.R")
source("helpbox.R")


# define some global variables like
# directory of raw data
datadir <- "data"

# make list of database files in data folder
datalistfiles <- list.files(datadir, pattern = "\\.csv$", full.names = TRUE)


# SHINY UI
# ***********************************************
# Define UI for application that draws a histogram

ui <- shinyUI(navbarPage(
  
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
                  "mean_mass_fraction_norm"),
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
    fundbox(width = 8)
  )

))


# SHINY SERVER
# ***********************************************
server <- shinyServer(function(input, output) {
  
  # MAIN DATA IS LOADED
  # the reactive environment makes sure all widgets can use the data
  # without re-reading every time
  data <- reactive({
    
    # read csv tables of user selection
    data <- read.csv(input$UserDataChoice, stringsAsFactors = FALSE)
    # filter columns by a set of allowed regular expressions
    data <- data[grep(pattern = "protein|condition|growthrate|mean_|median_|
      sd_|CV|CI|rel_intensity|psortB_loc|Process|Pathway|Protein|Length|
      Helices|MolWeight|model.category", colnames(data))]
    data
    
  })
  
  # SOME GLOBAL FUNCTIONS THAT ALL PLOTS USE
  # filter data by user choices
  filtGenes <- reactive({
    get_selected(input$tree, format = "names") %>% 
      unlist %>%
      subset(., grepl("3*[a-z]4*[0-9]", .))
  })
  
  # apply log or lin transformation to orig data
  logfun <- function(x) {
    if (input$UserLogY == "linear") x
    else if(input$UserLogY == "log 2") log2(x)
    else if(input$UserLogY == "log 10") log10(x)
    else log(x)
  }
  
  # select type of plot (points or lines)
  type <- reactive({
    if (input$UserPlotType == "points") "p"
    else if(input$UserPlotType == "lines") "l"
    else if(input$UserPlotType == "points and lines") "b"
  })
  
  # select theme
  theme <- reactive({
    if (input$UserTheme == "ggplot1") ggplot2like()
    else if (input$UserTheme == "ggplot2") custom.ggplot
    else if (input$UserTheme == "lattice grey") custom.lattice
    else if (input$UserTheme == "lattice blue") theEconomist.theme()
  })
  
  # generic download handler for all download buttons
  getDownload <- function(filename, plot) {
    downloadHandler(
      filename = filename,
      content = function(file) {
        svg(file, 
          width = {if (input$UserPrintWidth == "auto") 7
            else as.numeric(input$UserPrintWidth)/100}, 
          height = as.numeric(input$UserPrintHeight)/100)
        print(plot)
        dev.off()
      },
      contentType = "image/svg"
    )
  }
  
  # SHINY TREE
  output$tree <- renderTree({
     
    # select only some columns for construction of tree
    cols <- c("Process.abbr", "Pathway.abbr", "protein", "Protein")
    # remove duplicated proteins
    prot <- data()[!duplicated(data()$protein), cols]
    
    # generate list for tree using this recursive function
    makeTree <-function(rows, col, numcols) {
      if(col == numcols) prot[rows, col] else {
        spl <- split(rows, prot[rows, col])
        lapply(spl, function(rows) makeTree(rows, col+1, numcols))
      }
    }
    
    # apply function to make nested list of the tree
    listTree <- makeTree(seq_len(nrow(prot)), 1, ncol(prot))
    # change attributes of some nodes so that they are selected right from the start
    # if nothing is selected the tree returns NULL
    listTree[[1]][[1]] <- lapply(listTree[[1]][[1]], function(x) {
      attr(x, which = "stselected") <- TRUE; x})
    listTree
  })
  
   
  # PLOT AND TABLE UI OUTPUTS
  #
  # To control size of the plots, we need to wrap plots
  # into additional renderUI function that can take height argument
  output$dotplot.ui <- renderUI({
    plotOutput("dotplot", height = input$UserPrintHeight, width = input$UserPrintWidth)
  })
  output$barchart.ui <- renderUI({
    plotOutput("box_chart", height = input$UserPrintHeight, width = input$UserPrintWidth)
  })
  output$violinplot.ui <- renderUI({
    plotOutput("violinplot", height = input$UserPrintHeight, width = input$UserPrintWidth)
  })
  output$heatmap.ui <- renderUI({
    plotOutput("heatmap", height = input$UserPrintHeight, width = input$UserPrintWidth)
  })
  output$clustering.ui <- renderUI({
    plotOutput("clustering", height = input$UserPrintHeight, width = input$UserPrintWidth)
  })
  output$table.ui <- renderUI({
    tableOutput("table")
  })
  
  
  # PLOT DATA USING XYPLOT FROM LATTICE
  # that is made for multifactorial data
  output$dotplot <- renderPlot(res = 120, {
    
    # plot of gene expression is drawn
    plot <- xyplot(logfun(get(input$UserYVariable)) ~ factor(get(input$UserXVariable)) | 
        factor(get(input$UserCondVariable)), 
      subset(data(), protein %in% filtGenes()),
      groups = {
        if (input$UserGrouping == "none") NULL
        else if(input$UserGrouping == "by conditioning") get(input$UserCondVariable)
        else if(input$UserGrouping == "by X variable") get(input$UserXVariable)
        else if(input$UserGrouping == "by Y variable") {
          get(input$UserYVariable) %>% logfun %>%
          .bincode(., pretty(.))
        }
      },
      auto.key = FALSE, type = type(),
      par.settings = theme(),
      layout = {
        if (input$UserPanelLayout == "manual") {
        c(input$UserPanelLayoutCols, input$UserPanelLayoutRows)} 
        else NULL},
      as.table = TRUE,
      scales = list(alternating = FALSE, x = list(rot = 45)),
      xlab = input$UserXVariable,
      ylab = paste0(input$UserYVariable, " (", input$UserLogY, ")"),
      panel = function(x, y, ...) {
        if (input$UserTheme == "ggplot2") 
          panel.grid(h = -1, v = -1, col = "white")
        else 
          panel.grid(h = -1, v = -1, col = grey(0.9))
        panel.xyplot(x, y, ...)
      }
    )
    
    # optional addition of error margins, for mean, median, and mean mass 
    # fraction we use the relative standard deviation, 
    # for fold change we use confidence interval
    if (input$UserYVariable == "rel_intensity") error = 'CI'
    else error = 'CV'
    
    plot <- plot +
    as.layer(
      xyplot(
        logfun(get(input$UserYVariable)*(1+get(error))) +
        logfun(get(input$UserYVariable)*(1-get(error))) ~
        factor(get(input$UserXVariable)) |
        factor(get(input$UserCondVariable)),
        subset(data(), protein %in% filtGenes()),
        panel = function(x, y, ...) {
          panel.segments(x0 = as.numeric(x), x1 = as.numeric(x), 
            y0 = y[1:(length(y)/2)], y1 = y[(length(y)/2+1):length(y)], 
            col = grey(0.6, alpha = 0.3), lwd = 1.5)
          panel.key(
            {if (input$UserYVariable == "rel_intensity") "+/- 95% CI" else "+/- STDEV"}, 
            which.panel = 1, corner = c(0.05, 0.05), 
            lines = FALSE, points = FALSE, col = grey(0.6), cex = 0.7
          )
        }
      )
    )
    
    # print plot to output panel
    print(plot)
    # download function
    output$UserDownloadDotplot <- getDownload(filename = "dotplot.svg", plot = plot)
    
  })
  
  # PLOT DATA USING BWPLOT FROM LATTICE
  output$box_chart <- renderPlot(res = 120, {
    
    # plot of gene expression is drawn
    plot <- bwplot(logfun(get(input$UserYVariable)) ~ factor(get(input$UserXVariable)) | 
        factor(get(input$UserCondVariable)), 
      subset(data(), protein %in% filtGenes()),
      auto.key = FALSE, pch = "|",
      par.settings = theme(),
      layout = {
        if (input$UserPanelLayout == "manual") {
        c(input$UserPanelLayoutCols, input$UserPanelLayoutRows)}
        else NULL},
      as.table = TRUE,
      scales = list(alternating = FALSE, x = list(rot = 45)),
      xlab = input$UserXVariable,
      ylab = paste0(input$UserYVariable, " (", input$UserLogY, ")"),
      panel = function(x, y, ...) {
        if (input$UserTheme == "ggplot2")
          panel.grid(h = -1, v = -1, col = "white")
        else
          panel.grid(h = -1, v = -1, col = grey(0.9))
        panel.stripplot(x, y, jitter = TRUE, factor = 0.75, pch = 19, col = grey(0.75))
        panel.bwplot(x, y, ...)
      }
    )
    
    # print plot to output panel
    print(plot)
    # download function
    output$UserDownloadDotplot <- getDownload(filename = "boxplot.svg", plot = plot)
    
  })
  
  
  # PLOT DATA USING VIOLINPLOT FROM LATTICE
  output$violinplot <- renderPlot(res = 120, {
    
    # plot of gene expression is drawn
    plot <- bwplot(logfun(get(input$UserYVariable)) ~ factor(get(input$UserXVariable)) | 
        factor(get(input$UserCondVariable)), 
      subset(data(), protein %in% filtGenes()),
      auto.key = FALSE, par.settings = theme(),
      layout = {
        if (input$UserPanelLayout == "manual") {
        c(input$UserPanelLayoutCols, input$UserPanelLayoutRows)}
        else NULL},
      as.table = TRUE,
      scales = list(alternating = FALSE, x = list(rot = 45)),
      xlab = input$UserXVariable,
      ylab = paste0(input$UserYVariable, " (", input$UserLogY, ")"),
      panel = function(x, y, ...) {
        if (input$UserTheme == "ggplot2")
          panel.grid(h = -1, v = -1, col = "white")
        else
          panel.grid(h = -1, v = -1, col = grey(0.9))
        panel.violin(x, y, alpha = 0.5, bw = 0.2, ...)
      }
    )
    
    # print plot to output panel
    print(plot)
    # download function
    output$UserDownloadDotplot <- getDownload(filename = "violinplot.svg", plot = plot)
    
  })
  
  
  # PLOT DATA AS HEATMAP WITH LEVELPLOT
  # some options for dotplot do not apply here
  output$heatmap <- renderPlot(res = 120, {
    
    # plot of gene expression is drawn
    plot <- levelplot(logfun(get(input$UserYVariable)) ~ 
        as.factor(get(input$UserXVariable)) * 
        as.factor(protein),
      subset(data(), protein %in% filtGenes()),
      auto.key = FALSE,
      par.settings = theme(), 
      as.table = TRUE,
      scales = list(alternating = FALSE, x = list(rot = 45)),
      xlab = input$UserXVariable,
      ylab = "protein"
    )
    
    # print plot to output panel
    print(plot)
    # download function
    output$UserDownloadDotplot <- getDownload(filename = "heatmap.svg", plot = plot)
    
  })
  
  
  # PLOT CLUSTERING
  # some options for dotplot do not apply here
  output$clustering <- renderPlot(res = 120, {
    
    # filter data by selected genes and
    # coerce data to matrix that is required for clustering
    mat <- subset(data(), protein %in% filtGenes())
    mat <- spread(mat[c("protein", "condition", input$UserYVariable)], condition, get(input$UserYVariable))
    
    # adjust rownames to genes and apply optional logging
    rownames(mat) <- mat[, 1]
    mat <- logfun(mat[,-1])
    
    # compute dissimilarity matrix using hclust
    # for the clustering algorithm, see hclust manual
    prot.cluster <- hclust(dist(mat), method = "ward.D")
    # plot the cluster
    plot(color_branches(
      prot.cluster,
      k = input$UserNClust,
      groupLabels = TRUE,
      col = colorRampPalette(custom.lattice$superpose.polygon$col)(input$UserNClust)
    ))
    
  })
  
  output$table <- renderTable(digits = 4, {
    
    # RENDER TABLE WITH QUANTITIES OF SELECTED PROTEINS
    # ***********************************************
    
    # filter data by user choices
    data <- subset(data(), protein %in% filtGenes())
    
    # download handler for table
    output$UserDownloadTable <- downloadHandler(
      filename = "data.csv",
      content = function(file) {
        write.csv(data, file)
      },
      contentType = "text/csv"
    )
    
    # call table to be rendered
    data
    
    })
  
})

# Run the application 
shinyApp(ui = ui, server = server)

