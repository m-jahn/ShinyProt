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

# define some global variables like
# directory of raw data
datadir <- getwd()

# make list of database files in data folder
datalistfiles <- list.files(datadir, pattern=".csv", full.names=TRUE, 
  recursive=TRUE)


# SHINY UI
# ***********************************************
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
  # Application title
  titlePanel("ShinyProt - interactive gene expression analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    # HERE COME ALL CONTROLS FOR THE SIDEBAR PANEL
    sidebarPanel(width=6,
      
      # SELECT DATA
      selectInput("UserDataChoice",
        "Choose data:", datalistfiles, 
        selected=tail(datalistfiles,3)),
      
      # SELECT GENES OR PROTEINS
      fluidRow(
        column(width=4, 
          selectInput("UserPanelLayout", 
            "Panel layout:", choices=list("c(1,4)", "c(2,2)", "c(4,1)", "c(1,8)", "c(2,4)", "c(4,2)", "c(8,1)"),
            selected="c(4,2)")
        ),
        column(width=4, 
          numericInput("UserPrintWidth",
            "Plot width:", value=8)
        ),
        column(width=4, 
          numericInput("UserPrintHeight",
            "Plot height:", value=6)
        )
      ),
      
      shinyTree("tree", search=TRUE, checkbox=TRUE)
    ),
    
    
    # Show plots on extra tabs
    # Each tab has individual Download buttons
    column(width=6,
      tabsetPanel(
        tabPanel("GRAPHICS", uiOutput("plot.ui"),
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
  
  # SHINY TREE
   output$tree <- renderTree({
     
    # read csv tables of user selection, only some categories
    prot <- read.csv(input$UserDataChoice, stringsAsFactors=FALSE)[1:100, c("Process.abbr", "Pathway.abbr", "Protein")]
    # reorder protein list
    #prot <- prot[with(prot, order(Process.abbr, Pathway.abbr, Protein)), ]
    
    # generate list for tree using this recursive function
    makeTree <-function(x, i, n) {
      if(i==n) prot[x, i] else {
        spl <- split(x, prot[x,i])
        lapply(spl, function(x) makeTree(x,i+1,n))
      }
    }
    # apply function
    print(makeTree(seq_len(nrow(prot)), 1, ncol(prot)))
    makeTree(seq_len(nrow(prot)), 1, ncol(prot))
    

  })
  
  # PLOT AND TABLE OUTPUT
  # To control size of the plots, we need to wrap the ODplot and Muplot
  # into additional renderUI function that can take height argument
  output$plot.ui <- renderUI({
    plotOutput("plot", height=input$UserPrintHeight*100)
  })
  output$table.ui <- renderUI({
    tableOutput("table")
  })
  
  # plot data using xyplot from lattice package, 
  # that is made for multifactorial data
  output$plot <- renderPlot(res=120, {
    
    # read csv tables of user selection
    data <- read.csv(input$UserDataChoice)
    # filter columns by a set of allowed regular expressions
    data <- data[grep(pattern="protein|condition|growthrate|mean_|median_|
      sd_|CV|rel_intensity|psortB_loc|Process|Pathway|Protein|Length|
      Helices|MolWeight", colnames(data))]
    
    # filter data by user choices
    
    
    # set log or lin flag and adjust scales accordingly
    # if (input$UserLogY=="linear")
    #   scaleoptions=list(
    #     alternating=FALSE, 
    #     x=list(limits=input$UserXlim),
    #     y=list(limits=input$UserYlim)
    #   ) else
    #   scaleoptions=list(
    #     alternating=FALSE,
    #     x=list(limits=input$UserXlim),
    #     y=list(log=10, limits=input$UserYlimLog)
    #   )
    # 
    # 
    # # select theme
    # if (input$UserThemeCheck=="ggplot2 theme")
    #   theme <- ggplot2like() else
    #   theme <- theEconomist.theme()
    # 
    # 
    # # select OD correction
    # if(input$UserODCorrect) 
    #   od_select <- 'od_corr' else
    #   od_select <- 'od_value'
    # 
    # 
    # # actual plot is drawn
    # ODplot <- xyplot(get(od_select) ~ as.numeric(batchtime_h) | factor(channel_id), data,
    #   groups=od_led, par.settings=theme, 
    #   layout=eval(parse(text=input$UserPanelLayout)), 
    #   auto.key=list(columns=2), 
    #   as.table=TRUE,
    #   scales=scaleoptions,
    #   xlab="time [h]", ylab="OD",
    #   type=input$UserODType, lwd=2,
    #   panel=function(x, y, ...) {
    #     lims <- round(input$UserXlim, -1)
    #     panel.abline(v=seq(lims[1], lims[2], by=10), col=grey(0.95))
    #     panel.grid(h=-1, v=-1, col=grey(0.95))
    #     if (input$UserShowRatio) {
    #     panel.xyplot(unique(x), y[seq(1, length(y), 2)]/y[seq(2, length(y), 2)], 
    #       col=grey(0.7), cex=0.2)
    #     }
    #     if (panel.number()==1) {
    #       panel.text(input$UserXlim[1], input$UserYlim[2]*0.85, cex=0.5, col=grey(0.4),
    #         pos=4, labels=paste0("last measurement: \n", data[nrow(data), "time"]))
    #     }
    #     panel.superpose(x, y, ...)
    #     },
    #   panel.groups=function(x, y, ...){
    #     panel.xyplot(x, y, ...)
    #   }
    # )
    # 
    # print(ODplot)
    # 
    # output$UserDownloadOD <- downloadHandler(
    #   filename="ODplot.svg",
    #   content = function(file) {
    #     svg(file, width=input$UserPrintWidth, height=input$UserPrintHeight)
    #     print(ODplot)
    #     dev.off()
    #   },
    #   contentType="image/svg"
    # )
  })

  output$table <- renderTable(digits=4, {
    
    # RENDER TABLE WITH QUANTITIES OF SELECTED PROTEINS
    # ***********************************************
    
    # read csv tables of user selection
    data <- read.csv(input$UserDataChoice)
    # filter columns by a set of allowed regular expressions
    data <- data[grep(pattern="protein|condition|growthrate|mean_|median_|
      sd_|CV|rel_intensity|psortB_loc|Process|Pathway|Protein|Length|
      Helices|MolWeight", colnames(data))]
    
    # filter data by user choices
    
    
    output$UserDownloadTable <- downloadHandler(
      filename="data.csv",
      content = function(file) {
        write.csv(dat, file)
      },
      contentType="text/csv"
    )
    
    # call table to be rendered
    data
    
    })
  
})

# Run the application 
shinyApp(ui=ui, server=server)

