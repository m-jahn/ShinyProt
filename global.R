# 
# LOADING LIBRARIES
# ***********************************************
library(shiny)
library(grid)
library(lattice)
library(latticeExtra)
library(tidyr)
library(dplyr)
library(shinyTree)
library(shinythemes)
library(dendextend)


# LOADING EXTERNAL FUNCTIONS AND DATA
# ***********************************************
for (Rfile in list.files("R", "(plot|map|box|swarm|theme|functions).R", full.names = TRUE)) {
  source(Rfile)
}

# list of data files
datalistfiles <- list.files("data", pattern = "\\.Rdata$", full.names = TRUE)
# load all of them
for (path in datalistfiles) load(path)
# clean names for selectInput
datalistfiles <- gsub("^data/|.Rdata$", "", datalistfiles)


