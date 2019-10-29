#' Calculate and draw p-values in lattice plots
#' 
#' Custom panel functions for lattice plots
#' 
#' @import grid
#' @import lattice
#' @import latticeExtra
#' @importFrom grDevices grey
#' @importFrom methods new
#' 
#' @param x,y (numeric, character) variables to be plotted. The x variable 
#'   is treated as a grouping varibale, i.e. p-values are calculated between groups
#'   of unique x values.
#' @param std (character, numeric) the value of x that is used as standard. If NULL, the 
#'   first value is used as standard. If a numeric scalar i, std is determined 
#'   as the i'th element of unique(x). If character, it must be one of x. If 
#'   std = 'all_values', each unique group of x is compared to the total population
#' @param symbol (logical) if '*' symbols are to be drawn for significance
#' @param cex_symbol (numeric) character size of the symbol
#' @param offset (numeric) offset added to the the vertical position of the p-value
#' @param fixed_pos (numeric) vertical position of the p-value, 
#'   if NULL determined from the data
#' @param verbose (logical) if a summary of the p-value calculation should be 
#'   printed to the terminal
#' @param pval_digits (numeric) Integer specifying number of digits for p-value
#' @param alternative (character) Passed to t.test(), one of "two.sided", "less", or "greater"
#' @param col (character) the color to be used
#' @param ... other arguments passed to the function
#' 
#' @export
# ------------------------------------------------------------------------------
panel.pvalue <- function(x, y, std = NULL, symbol = TRUE, cex_symbol = 1.5, offset = 1, 
  fixed_pos = NULL, verbose = FALSE, pval_digits = 4, alternative = "two.sided",
  col = trellis.par.get()$superpose.polygon$col[1], ...
  ) 
{ 
  # if x is a factor, reorder x and y based on factor levels
  if (is.factor(x)) {
    ord <- order(as.numeric(x))
    x <- x[ord]; y <- y[ord]
  }

  # if no standard is passed to function, just take the first unique x
  if (is.null(std)) {
    std = unique(x)[1]
  } else if (is.numeric(std)) {
    std = unique(x)[std]
  } else if (std == "all_values") {
    std = x
  } else if (is.character(std)) {
    stopifnot(std %in% x)
  }
  
  # calculate p-value between all x-variable groups and standard
  pval <- tapply(y, x, function(z) {
    t.test(z, y[x == std], alternative = alternative)$p.value
  })
  if (verbose) {
    cat("p-value for comparison of ", std, " with ", 
      as.character(unique(x)), " = ", pval, "\n")
  }
  
  if (symbol) {
    pval <- sapply(pval, function(x) {
      if (is.na(x)) "" else
      if (x <= 0.001) "***" else
      if (x <= 0.01 & x > 0.001) "**" else
      if (x <= 0.05 & x > 0.01) "*" else ""
    })
  } else {
    pval = paste0("p = ", round(pval, pval_digits))
  }
  
  if (is.null(fixed_pos)) {
    ypos = tapply(y, x, function(x) median(x, na.rm = TRUE))
  } else if (is.numeric(fixed_pos)){
    ypos = fixed_pos
  } else {
    stop("fixed_pos must be NULL for automatic y positioning, or a numeric value.")
  }
  panel.text(1:length(pval), ypos, labels = pval, 
    cex = cex_symbol, pos = 3, offset = offset, col = col, ...)
}


#' Calculate and draw error bars in lattice plots
#' 
#' Custom panel functions for lattice plots
#' 
#' @param x,y (numeric, character) variables to be plotted. The x variable 
#'   is treated as a grouping varibale, i.e. error bars are calculated between groups
#'   of unique x values.
#' @param ewidth width of the error bar whiskers
#' @param FUN_mean the function used to calculate group (x-variable) means
#' @param FUN_errb the function used to calculate group (x-variable) errors
#' @param ... other arguments passed to the function
#' 
#' @export
# ------------------------------------------------------------------------------
panel.errbars <- function (x, y, ewidth = 0.08, 
  FUN_mean = function(x) mean(x, na.rm = TRUE),
  FUN_errb = function(x) sd(x, na.rm = TRUE), ...)
{
  means <- tapply(y, x, FUN_mean)
  stdev <- tapply(y, x, FUN_errb)
  x <- unique(x)
  if (is.factor(x)) x <- sort(as.numeric(x))
  Y <- as.matrix(cbind(means, means-stdev, means+stdev))
  y <- Y[x, 1]
  y0 <- Y[x, 2]
  y1 <- Y[x, 3]
  offs <- ewidth/2
  panel.segments(x0 = x, x1 = x, y0 = y0, y1 = y1, ...)
  panel.segments(x0 = x - offs, x1 = x + offs, y0 = y0, y1 = y0, ...)
  panel.segments(x0 = x - offs, x1 = x + offs, y0 = y1, y1 = y1, ...)
  panel.xyplot(x, y, ...)
}


#' Draw barplot with error bars in lattice plots
#' 
#' Custom panel functions for lattice plots
#' 
#' @param x,y (numeric, character) variables to be plotted. The x variable 
#'   is treated as a grouping varibale, i.e. error bars are calculated between groups
#'   of unique x values.
#' @param ewidth width of the error bar whiskers
#' @param FUN_mean the function used to calculate group (x-variable) means
#' @param FUN_errb the function used to calculate group (x-variable) errors
#' @param ... other arguments passed to the function
#' 
#' @export
# ------------------------------------------------------------------------------
panel.barplot <- function (x, y, ewidth = 0.08, 
  FUN_mean = function(x) mean(x, na.rm = TRUE),
  FUN_errb = function(x) sd(x, na.rm = TRUE), ...)
{
  means <- tapply(y, x, FUN_mean)
  stdev <- tapply(y, x, FUN_errb)
  x <- unique(x)
  if (is.factor(x)) x <- sort(as.numeric(x))
  Y <- as.matrix(cbind(means, means-stdev, means+stdev))
  y <- Y[x, 1]
  y0 <- Y[x, 2]
  y1 <- Y[x, 3]
  offs <- ewidth/2
  ybottom <- current.panel.limits()$ylim[1]
  
  panel.rect <- function (xleft, ybottom, xright, ytop, x = (xleft + xright)/2, 
    y = (ybottom + ytop)/2, width = xright - xleft, height = ytop - ybottom, 
    col = NULL, fill = NULL, lty = 1, lwd = 1, alpha = 1, font, fontface, ..., 
    identifier = NULL, name.type = "panel")
  {
    grid::grid.rect(x = x, y = y, width = width, height = height, default.units = "native",
      gp = grid::gpar(fill = fill, col = col, lty = lty, lwd = lwd, alpha = alpha, ...)
    )
  }
  
  panel.segments(x0 = x, x1 = x, y0 = y0, y1 = y1, ...)
  panel.segments(x0 = x - offs, x1 = x + offs, y0 = y0, y1 = y0, ...)
  panel.segments(x0 = x - offs, x1 = x + offs, y0 = y1, y1 = y1, ...)
  panel.rect(xleft = x - ewidth, ybottom = ybottom, xright = x + ewidth, 
    ytop = y, ...)
}


#' Draw custom keys in lattice plots
#' 
#' Custom panel functions for lattice plots
#' 
#' @param labels (character) list of the labels to draw
#' @param which.panel (numeric) the panel(s) where key(s) should be drawn
#' @param points (logical) if points should be drawn
#' @param lines (logical) if lines should be drawn
#' @param rectangles (logical) if rectangles should be drawn
#' @param corner (numeric) vector of length 2 indicating the position of the key,
#'   in Normalised Parent Coordinates (0 to 1)
#' @param col,lwd,lty,pch,cex,point.cex graphical parameters to draw key labels
#'   and symbols
#' @param ... other arguments passed to the function
#' 
#' @export
# ------------------------------------------------------------------------------
panel.key <- function (labels, which.panel = 1, pch = 1, cex = 0.8, 
  point.cex = NULL, points = TRUE, lines = FALSE, rectangles = FALSE,
  col = trellis.par.get()$superpose.polygon$col[1:length(labels)],
  lwd = trellis.par.get()$superpose.line$lwd[1],
  lty = trellis.par.get()$superpose.line$lty[1],
  corner = c(0.1, 0.9), ...)
{
  if (panel.number() %in% which.panel) {
    key <- simpleKey(labels, points = points, lines = lines, rectangles = rectangles,...)
    key$text$col <- col
    key$text$cex <- cex
    if (points == TRUE) {
      key$points$col <- col
      key$points$pch <- pch
      key$points$cex <- ifelse(!is.null(point.cex), point.cex, cex)
    }
    if (lines == TRUE) {
      key$lines$col <- col
      key$lines$lwd <- lwd
      key$lines$lty <- lty
    }
    key.gf <- draw.key(key, draw = FALSE)
    vp <- grid::viewport(
      x = unit(corner[1], "npc") + unit(0.5 - corner[1], "grobwidth", list(key.gf)), 
      y = unit(corner[2], "npc") + unit(0.5 - corner[2], "grobheight", list(key.gf))
    )
    grid::pushViewport(vp)
    grid::grid.draw(key.gf)
    grid::upViewport()
  }
}


#' Draw quadrants and quadrant statistics in lattice plots
#' 
#' Custom panel functions for lattice plots
#' 
#' @param x,y (numeric) variables to be plotted
#' @param h,v (numeric) position of the horizontal or vertical threshold for dividing the
#'   data into quadarants. Defaults to the median.
#' @param labels (character) One of 'percent', 'events', or 'none'. Controls what
#'   type of summary is plottted per quadrant
#' @param col,lwd graphical parameters for lines and labels
#' @param margin (numeric) margin of labels to the plot edges in Normalised Parent 
#'   Coordinates, default is 0.1
#' @param ... other arguments passed to the function
#' 
#' @export
# ------------------------------------------------------------------------------
panel.quadrants <- function (x, y, h = NULL, v = NULL, 
  labels = "percent", col = grey(0.5), margin = 0.1,
  lwd = trellis.par.get()$superpose.polygon$lwd[1], ...)
{ 
  # remove inf or NA values
  index = !{is.infinite(x*y) | is.na(x*y)}
  x = x[index]; y = y[index]
  
  # drawing the horizontal and vertical lines
  if (is.null(h))
    h = median(y)
  if (is.null(v))
    v = median(x)
  panel.abline(h = h, v = v, lwd = lwd, col.line = col)
  
  # plot percentages of the 4 quadrants as text
  quadrant <- list(
    Q1 = sum({x < v & y > h}),
    Q2 = sum({x > v & y > h}),
    Q3 = sum({x > v & y < h}),
    Q4 = sum({x < v & y < h})
  )
  
  # can either plot events or percentage, or no labels
  if (labels == "percent") {
    quadrant = sapply(quadrant, function(i) {
      paste0(round(i/length(x)*100, 1), "%")
    })
  } else if (labels == "events") {
    quadrant = paste0("n=", quadrant)
  } else if (labels == "none") {
    warning("no quadrant labels are plotted")
  } else {
    stop("labels must be one of 'percent', 'events', or 'none'")
  }
  
  # actual plotting of labels
  if (labels %in% c("percent", "events")) {
    with(current.panel.limits(), {
      xmargin = (xlim[2]-xlim[1])*margin
      ymargin = (ylim[2]-ylim[1])*margin
      panel.text(xlim[1]+xmargin, ylim[2]-ymargin, pos = 4, labels = quadrant[1], col = col)
      panel.text(xlim[2]-xmargin, ylim[2]-ymargin, pos = 2, labels = quadrant[2], col = col)
      panel.text(xlim[2]-xmargin, ylim[1]+ymargin, pos = 2, labels = quadrant[3], col = col)
      panel.text(xlim[1]+xmargin, ylim[1]+ymargin, pos = 4, labels = quadrant[4], col = col)
    })
  }
}
