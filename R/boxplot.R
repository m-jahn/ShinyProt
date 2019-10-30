# PLOT DATA AS DOTPLOT
#
plot_boxplot <- function(
  x, y, cond_var, groups, data,
  logfun, theme, layout, type, input
) {
  
  # check that data is loaded
  req(nrow(data) != 0)
  
  if (!is.null(groups)) {
    
    # in the rare case we group by Y variable
    # we have to add a binned pseudo Y variable
    if (groups == input$UserYVariable) {
      data <- mutate(data,
        binned_Y = get(groups) %>% logfun %>% .bincode(., pretty(.)))
      groups <- "binned_Y"
    }
    
    # determine number of columns for group legend
    if (length(unique(data[[groups]])) <= 16) {
      ncol_legend <- length(unique(data[[groups]])) %>%
        replace(., . > 4, 4)
    } else {
      ncol_legend <- NULL
    }
    
  } else {
    ncol_legend <- NULL
  }
  
  # rotate X scale if too long
  rot_X_label <- ifelse(
    median(nchar(data[[x]]), na.rm = TRUE) >= 4, 35, 0
  )
  
    # make dot plot
  plot <- bwplot(
    logfun(get(y)) ~ factor(get(x)) | factor(get(cond_var)),
    data,
    groups = {if (is.null(groups)) NULL else factor(get(groups))},
    par.settings = theme, 
    layout = layout,
    as.table = TRUE,
    scales = list(alternating = FALSE, x = list(rot = rot_X_label)),
    xlab = x, ylab = paste0(y, " (", input$UserLogY, ")"),
    auto.key = {if (is.null(ncol_legend)) NULL else list(columns = ncol_legend)},
    panel = function(x, y, ...) {
      panel.grid(h = -1, v = -1, 
        col = ifelse(theme == "ggplot2", "white", grey(0.9)))
      panel.stripplot(x, y, jitter = TRUE, factor = 0.75, 
        pch = 19, cex = 0.4, ...)
      if (type == "boxplot") {
        panel.bwplot(x, y, pch = "|", do.out = FALSE, ...)
      } else if (type == "violin") {
        panel.violin(x, y, alpha = 0.5, bw = 0.2, 
          lwd = 1.5, border = grey(0.3), col = grey(0.7), ...)
        
      }
    } 
  )

  # return final plot
  plot
  
}
