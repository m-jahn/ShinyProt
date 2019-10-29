# PLOT DATA AS HEATMAP WITH LEVELPLOT
# some options for dotplot do not apply here
plot_heatmap <- function(
  x, y, z, cond_var, data,
  logfun, theme, layout, input,
  rotate_x = 0
) {
  
  # check that data is loaded
  req(nrow(data) != 0)
  
  # in the rare case we condition by Y variable
  # we have to add a binned pseudo Y variable
  if (!is.null(cond_var)) {
    if (cond_var == z) {
      data <- mutate(data,
        binned_Y = get(cond_var) %>% logfun %>% .bincode(., pretty(.)))
      cond_var <- "binned_Y"
    }
  }
  
  # prepare data: heatmap can not display replicate points per
  # condition -> need to aggregate replicate values as mean or median
  data <- group_by_at(data, vars(c(x, y, cond_var))) %>%
    summarize_at(vars(z), .funs = function(x) mean(x, na.rm = TRUE))
  
  
  # use formula interface to plot with and without conditioning
  form <- "logfun(get(z)) ~ factor(get(x)) * factor(get(y))"
  if (!is.null(cond_var)) {
    form <- paste(form, "| factor(get(cond_var))")
  }
  
  
  # plot heat map
  levelplot(
    as.formula(form),
    data,
    par.settings = theme, 
    layout = layout,
    as.table = TRUE,
    scales = list (alternating = FALSE, x = list(rot = rotate_x)),
    xlab = x,
    ylab = z
  )

}

