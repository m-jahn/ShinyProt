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
library(devtools)
library(shinyTree)
library(shinythemes)
library(shinydashboard)
source("custom.themes.R")


# define some global variables like
# directory of raw data
datadir <- "~/Documents/Code/Shiny/ShinyProt"

# make list of database files in data folder
datalistfiles <- list.files(datadir, pattern="\\.csv$", full.names=TRUE)


# SHINY UI
# ***********************************************
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Use one of different shiny themes
  theme=shinytheme("lumen"),
   
  # Application title
  titlePanel("ShinyProt - interactive gene expression analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    # HERE COME ALL CONTROLS FOR THE SIDEBAR PANEL
    sidebarPanel(width=5,
      
      # SELECT DATA
      selectInput("UserDataChoice",
        "Choose data:", datalistfiles, 
        selected=tail(datalistfiles,3)),
      
      # SELECT PLOT OPTIONS
      hr(),
      h4("PLOT OPTIONS"),
      fluidRow(
        
        # PANEL LAYOUT AND PLOT DIMESNIONS
        column(width=4, 
          selectInput("UserPanelLayout", 
            "Panel layout:", choices=list("automatic", "manual"),
            selected="automatic")
        ),
        column(width=2, 
          conditionalPanel(condition="input.UserPanelLayout=='manual'",
            numericInput("UserPanelLayoutCols", 
              "Columns:", value=4)
          )
        ),
        column(width=2, 
          conditionalPanel(condition="input.UserPanelLayout=='manual'",
            numericInput("UserPanelLayoutRows", 
            "Rows:", value=4)
          )
        ),
        column(width=2, 
          selectInput("UserPrintHeight",
            "Plot height:", choices=c(1:10*100), selected=800)
        ),
        column(width=2, 
          selectInput("UserPrintWidth",
            "Plot width:", choices=c("auto", 1:10*100), selected="auto")
        )
      ),
      
      fluidRow(
        
        # FACTORIZATION AND ORDERING OPTIONS
        column(width=4, 
          selectInput("UserXVariable", 
            "X variable:", choices=c("protein","condition","growthrate",
              "psortB_localization","Process","Pathway","Protein"),
            selected="condition")
        ),
        column(width=4, 
          selectInput("UserYVariable", 
            "Y variable:", choices=list("rel_intensity", "mean_intensity", "median_intensity", 
                "mean_mass_fraction_norm"),
            selected="rel_intensity")
        ),
        column(width=4, 
          selectInput("UserCondVariable", 
            "Conditioning variable:", choices=list("protein","condition","growthrate",
              "psortB_localization","Process","Pathway","Protein"),
            selected="protein")
        )
      ),
      
      fluidRow(
        
        # OTHER GRAPHICAL PLOT OPTIONS
        column(width=3, 
          selectInput("UserTheme", 
            "Theme:", choices=list("lattice grey", "lattice blue", "ggplot1", "ggplot2"),
            selected="lattice grey")
        ),
        column(width=3, 
          selectInput("UserGrouping", 
            "Color coding:", choices=list("none", "by conditioning", "by X variable", "by Y variable"),
            selected="by condition")
        ),
        column(width=3, 
          selectInput("UserPlotType", 
            "Plot type:", choices=list("points", "lines", "points and lines"),
            selected="points")
        ),
        column(width=3, 
          selectInput("UserLogY", 
            "Y scale:", choices=list("linear","log 2","log 10", "log e"),
            selected="log 2")
        )
      ),
      
      # SELECT GENES OR PROTEINS FROM TREE
      hr(),
      h4("PROTEIN SELECTION"),
      shinyTree("tree", search=TRUE, checkbox=TRUE)
    ),
    
    
    # MAIN PANEL WITH OUTPUT PLOTS
    # Each tab has individual Download buttons
    column(width=7,
      tabsetPanel(
        tabPanel("DOT PLOT", uiOutput("plot.ui"),
          downloadButton("UserDownloadPlot", "Download svg")
        ),
        tabPanel("TABLE", uiOutput("table.ui"),
          downloadButton("UserDownloadTable", "Download table")
        )
      )
    )
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
    data <- read.csv(input$UserDataChoice, stringsAsFactors=FALSE)
    # filter columns by a set of allowed regular expressions
    data <- data[grep(pattern="protein|condition|growthrate|mean_|median_|
      sd_|CV|rel_intensity|psortB_loc|Process|Pathway|Protein|Length|
      Helices|MolWeight|model.category", colnames(data))]
    data
    
  })
  
  # SHINY TREE
  output$tree <- renderTree({
     
    # select only some columns for construction of tree
    cols <- c("Process.abbr", "Pathway.abbr", "protein","Protein")
    # remove duplicated proteins
    prot <- data()[!duplicated(data()$protein), cols]
    
    # generate list for tree using this recursive function
    makeTree <-function(rows, col, numcols) {
      if(col==numcols) prot[rows, col] else {
        spl <- split(rows, prot[rows, col])
        lapply(spl, function(rows) makeTree(rows, col+1, numcols))
      }
    }
    
    # apply function to make nested list of the tree
    listTree <- makeTree(seq_len(nrow(prot)), 1, ncol(prot))
    # chnage attributes of some nodes so that they are selected right from the start
    # if nothing is selected the tree returns NULL
    listTree[[1]][[1]] <- lapply(listTree[[1]][[1]], function(x) {
      attr(x, which="stselected") <- TRUE; x})
    listTree
  })
  
   
  # PLOT AND TABLE OUTPUT
  #
  # To control size of the plots, we need to wrap plots
  # into additional renderUI function that can take height argument
  output$plot.ui <- renderUI({
    plotOutput("plot", height=input$UserPrintHeight, width=input$UserPrintWidth)
  })
  output$table.ui <- renderUI({
    tableOutput("table")
  })
  
  
  # PLOT DATA USING XYPLOT FROM LATTICE
  # that is made for multifactorial data
  output$plot <- renderPlot(res=120, {
    
    # filter data by user choices
    filtGenes <- get_selected(input$tree, format="names") %>% 
      unlist %>%
      subset(., grepl("3*[a-z]4*[0-9]", .))
    
    
    # set log or lin flag and adjust scales accordingly
    scaleoptions=list(
       alternating=FALSE, 
       x=list(rot=45),
       y=list(log={
         if (input$UserLogY=="linear") FALSE 
         else if(input$UserLogY=="log 2") 2
         else if(input$UserLogY=="log 10") 10
         else "e"
      })
    )
    
    # select type of plot (points or lines)
    type={
      if (input$UserPlotType=="points") "p"
      else if(input$UserPlotType=="lines") "l"
      else if(input$UserPlotType=="points and lines") "b"
    }
    
    
    # select theme
     if (input$UserTheme=="ggplot1") theme <- ggplot2like()
     else if (input$UserTheme=="ggplot2") theme <- custom.ggplot
     else if (input$UserTheme=="lattice grey") theme <- custom.lattice
     else if (input$UserTheme=="lattice blue") theme <- theEconomist.theme()
    
    
    # Actual plot of gene expression is drawn
    ExpPlot <- xyplot(get(input$UserYVariable) ~ factor(get(input$UserXVariable)) | 
        factor(get(input$UserCondVariable)), 
      subset(data(), protein %in% filtGenes),
      groups={
        if (input$UserGrouping=="none") NULL
        else if(input$UserGrouping=="by conditioning") get(input$UserCondVariable)
        else if(input$UserGrouping=="by X variable") get(input$UserXVariable)
        else if(input$UserGrouping=="by Y variable") .bincode(get(input$UserYVariable), pretty(get(input$UserYVariable)))
      }, 
      auto.key=FALSE, type=type,
      par.settings=theme, 
      layout={
        if (input$UserPanelLayout=="manual") {
        c(input$UserPanelLayoutCols, input$UserPanelLayoutRows)} 
        else NULL},
      as.table=TRUE,
      scales=scaleoptions,
      xlab=input$UserXVariable,
      ylab=input$UserYVariable,
      panel=function(x, y, ...) {
       panel.grid(h=-1, v=-1, col=grey(0.9))
       panel.xyplot(x, y, ...)
      }
    )
    
    # print plot to output panel
    print(ExpPlot)
     
    output$UserDownloadOD <- downloadHandler(
      filename="ODplot.svg",
      content = function(file) {
        svg(file, width=input$UserPrintWidth, height=input$UserPrintHeight)
        print(ExpPlot)
        dev.off()
      },
      contentType="image/svg"
    )
    
  })

  output$table <- renderTable(digits=4, {
    
    # RENDER TABLE WITH QUANTITIES OF SELECTED PROTEINS
    # ***********************************************
    
    # filter data by user choices
    filtGenes <- get_selected(input$tree, format="names") %>% 
      unlist %>%
      subset(., grepl("3*[a-z]4*[0-9]", .))
    data <- subset(data(), protein %in% filtGenes)
    
    # download handler triggered by button
    output$UserDownloadTable <- downloadHandler(
      filename="data.csv",
      content=function(file) {
        write.csv(data, file)
      },
      contentType="text/csv"
    )
    
    # call table to be rendered
    data
    
    })
  
})

# Run the application 
shinyApp(ui=ui, server=server)

