# 
# LOADING LIBRARIES
# ***********************************************
library(shiny)
library(lattice)
library(latticeExtra)
library(latticetools)
library(tidyr)
library(dplyr)
library(ggplot2)
library(shinyTree)
library(shinythemes)
library(dendextend)
library(directlabels)
library(configr)


# LOADING EXTERNAL FUNCTIONS AND DATA
# ***********************************************
for (Rfile in list.files("R", "(plot|map|box|theme).R", full.names = TRUE)) {
  source(Rfile)
}

# list of data files
data_list <- list.files("data", pattern = "\\.Rdata$", full.names = TRUE)
# load all of them
for (path in data_list) load(path)
# clean names for selectInput
data_list <- gsub("^data/|.Rdata$", "", data_list)

# load corresponding YAML configuration file
data_config <- lapply(data_list, function(dat) {
  configr::read.config(paste0("data/", dat, ".yml")) %>%
    suppressWarnings()
}) %>% setNames(data_list)
