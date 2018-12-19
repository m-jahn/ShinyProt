# LOAD ADDITIONAL PACKAGES
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(grid)


# NEW PANEL FUNCTION FOR P-VALUE ANNOTATION
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# attention: y is measurement, x is conditional variable, std is the factor level
# that the other levels are compared to
panel.pvalue <- function(x, y, std, cex.symbol=1.5,
  col=trellis.par.get()$superpose.polygon$col[1], ...
  ) 
{
  pval <- tapply(y, x, function(z) t.test(z, y[x==std])$p.value)
  pval <- sapply(pval, function(x) {
    if (is.na(x)) "" else
    if (x <= 0.001) "***" else
    if (x <= 0.01 & x > 0.001) "**" else
    if (x <= 0.05 & x > 0.01) "*" else ""
  })
  panel.text(1:length(pval), tapply(y, x, mean), 
    labels=pval, cex=cex.symbol, pos=3, col=col, ...)
}


# NEW PANEL FUNCTION FOR XYPLOT WITH ERROR BARS
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
panel.errbars <- function (x, y, ewidth=0.08, 
  lwd=trellis.par.get()$superpose.polygon$lwd[1],
  groups=groups, subscripts=subscripts, ...)
{ 
  group <- as.numeric(as.factor(groups))[subscripts]
  col <- tapply(group, x, function(x) {
    trellis.par.get()$superpose.polygon$col[unique(x)]
    })
  means <- tapply(y, x, function(x) mean(x, na.rm=TRUE))
  stdev <- tapply(y, x, function(x) sd(x, na.rm=TRUE))
  x <- unique(x)
  if (is.factor(x)) x <- sort(as.numeric(x))
  Y <- as.matrix(cbind(means, means-stdev, means+stdev))
  y <- Y[x, 1]
  y0 <- Y[x, 2]
  y1 <- Y[x, 3]
  offs <- ewidth/2
  ybottom <- current.panel.limits()$ylim[1]
  panel.segments(x0 = x, x1 = x, y0 = y0, y1 = y1, col=col, lwd=lwd)
  panel.segments(x0 = x - offs, x1 = x + offs, y0 = y0, y1 = y0, col=col, lwd=lwd)
  panel.segments(x0 = x - offs, x1 = x + offs, y0 = y1, y1 = y1, col=col, lwd=lwd)
  panel.xyplot(x, y, col=col, ...)
}


# NEW PANEL FUNCTION FOR BARPLOT WITH ERROR BARS
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
panel.barplot <- function (x, y, ewidth=0.08, 
  border = trellis.par.get()$superpose.polygon$col[1], 
  fill = "white", ...) 
{
  means <- tapply(y, x, function(x) mean(x, na.rm=TRUE))
  stdev <- tapply(y, x, function(x) sd(x, na.rm=TRUE))
  x <- unique(x)
  if (is.factor(x)) x <- sort(as.numeric(x))
  Y <- as.matrix(cbind(means, means-stdev, means+stdev))
  y <- Y[x, 1]
  y0 <- Y[x, 2]
  y1 <- Y[x, 3]
  offs <- ewidth/2
  ybottom <- current.panel.limits()$ylim[1]
  panel.segments(x0 = x, x1 = x, y0 = y0, y1 = y1, col=border, ...)
  panel.segments(x0 = x - offs, x1 = x + offs, y0 = y0, y1 = y0, col=border, ...)
  panel.segments(x0 = x - offs, x1 = x + offs, y0 = y1, y1 = y1, col=border, ...)
  panel.rect(xleft=x - ewidth, ybottom = ybottom, xright = x + ewidth, ytop = y, 
    col=fill, border=border, ...)
}


# NEW PANEL FUNCTION DRAWING KEYS INSIDE PANELS
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
panel.key <- function (labels, which.panel=1, pch=1, cex=0.8,
  points=TRUE, lines=FALSE, rectangles=FALSE,
  col=trellis.par.get()$superpose.polygon$col[1:length(labels)],
  lwd=trellis.par.get()$superpose.line$lwd[1],
  lty=trellis.par.get()$superpose.line$lty[1],
  corner=c(0.1, 0.9), x=corner[1], y=corner[2], ...)
{
  if (panel.number() %in% which.panel) {
    key <- simpleKey(labels, points=points, lines=lines, rectangles=rectangles,...)
    key$text$col <- col
    key$text$cex <- cex
    if (points==TRUE) {
      key$points$col <- col
      key$points$pch <- pch
      key$points$cex <- cex
    }
    if (lines==TRUE) {
      key$lines$col <- col
      key$lines$lwd <- lwd
      key$lines$lty <- lty
    }
    key.gf <- draw.key(key, draw = FALSE)
    vp <- viewport(x = unit(x, "npc") + unit(0.5 - corner[1], 
      "grobwidth", list(key.gf)), y = unit(y, "npc") + unit(0.5 - 
      corner[2], "grobheight", list(key.gf)))
    pushViewport(vp)
    grid.draw(key.gf)
    upViewport()
  }
}
