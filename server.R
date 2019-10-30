# 
# SHINY SERVER
# ***********************************************
server <- function(input, output) {
  
  # MAIN DATA IS LOADED
  # the reactive environment makes sure all widgets can use the data
  # without re-reading every time
  data <- reactive({
    
    # read user selected data set
    get(input$UserDataChoice) %>%
      # coerce to base data.frame
      ungroup %>% as.data.frame
  })
  
  # GENERIC DATA FILTERING
  data_filt <- reactive({
    
    # filter data set
    data() %>% filter(
      protein %in% filtGenes()
    )
  
  })
  
  
  # SOME GLOBAL FUNCTIONS THAT ALL PLOTS USE
  # filter data by user choices
  filtGenes <- reactive({
    get_selected(input$tree, format = "names") %>% 
      unlist
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
    else if (input$UserTheme == "ggplot2") custom.ggplot()
    else if (input$UserTheme == "lattice grey") custom.lattice()
    else if (input$UserTheme == "lattice blue") theEconomist.theme()
  })
  
  # select layout
  layout <- reactive({
    if (input$UserPanelLayout == "manual") {
      c(input$UserPanelLayoutCols, input$UserPanelLayoutRows)}
    else NULL
  })
  
  # select grouping variable
  grouping <- reactive({
    if (input$UserGrouping == "none") NULL
    else if(input$UserGrouping == "by cond. variable") input$UserCondVariable
    else if(input$UserGrouping == "by X variable") input$UserXVariable
    else if(input$UserGrouping == "by Y variable") input$UserYVariable
    else gsub("by ", "", input$UserGrouping)
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
    
    # make plot and print
    plot <- plot_dotplot(
      x = input$UserXVariable,
      y = input$UserYVariable,
      cond_var = input$UserCondVariable,
      groups = grouping(),
      data = data_filt(),
      logfun = logfun,
      theme = theme(),
      layout = layout(),
      type = type(),
      input = input,
      plot_error = TRUE,
      error = {if (input$UserYVariable %in% c("fold_change", "rel_intensity")) "CI"
        else "CV"}
    )
    
    # print plot to output panel
    print(plot)
    # download function
    output$UserDownloadDotplot <- getDownload(filename = "dotplot.svg", plot = plot)
    
  })
  
  # PLOT DATA USING BWPLOT FROM LATTICE
  output$box_chart <- renderPlot(res = 120, {
    
    # plot of gene expression is drawn
    plot <- plot_boxplot(
       x = input$UserXVariable,
      y = input$UserYVariable,
      cond_var = input$UserCondVariable,
      groups = grouping(),
      data = data_filt(),
      logfun = logfun,
      theme = theme(),
      layout = layout(),
      type = "boxplot",
      input = input
    )
    
    # print plot to output panel
    print(plot)
    # download function
    output$UserDownloadBoxplot <- getDownload(filename = "boxplot.svg", plot = plot)
    
  })
  
  
  # PLOT DATA USING VIOLINPLOT FROM LATTICE
  output$violinplot <- renderPlot(res = 120, {
    
    # plot of gene expression is drawn
    plot <- plot_boxplot(
       x = input$UserXVariable,
      y = input$UserYVariable,
      cond_var = input$UserCondVariable,
      groups = grouping(),
      data = data_filt(),
      logfun = logfun,
      theme = theme(),
      layout = layout(),
      type = "violin",
      input = input
    )
    
    # print plot to output panel
    print(plot)
    # download function
    output$UserDownloadViolinplot <- getDownload(filename = "violinplot.svg", plot = plot)
    
  })
  
  
  # PLOT DATA AS HEATMAP WITH LEVELPLOT
  # some options for dotplot do not apply here
  output$heatmap <- renderPlot(res = 120, {
    
    # make plot and print
    plot <- plot_heatmap(
      x = input$UserXVariable,
      y = "protein",
      z = input$UserYVariable,
      cond_var = NULL,
      data = data_filt(),
      logfun = logfun,
      theme = theme(),
      layout = layout(),
      input = input,
      rotate_x = 35
    )
    
    # print plot to output panel
    print(plot)
    # download function
    output$UserDownloadHeatmap <- getDownload(filename = "heatmap.svg", plot = plot)
    
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
      col = colorRampPalette(custom.lattice()$superpose.polygon$col)(input$UserNClust)
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
  
}