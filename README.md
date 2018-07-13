# ShinyProt

Available on Shinyapps.io!
https://m-jahn.shinyapps.io/ShinyProt/

### R Shiny based browser for gene expression data

ShinyProt is a small app for exploration of gene expression, proteomics, or other gene-centered biological data.
Features:

- Displays dot plots of gene expression data
- Heatmaps, box and whisker plots, and clustering of proteins/genes by expression
- The original data table can be filtered by pathways or single genes
- Different variables can be plotted on X and Y axis, or used as conditioning variable (panel-view)
- All charts are interactive R Shiny modules and can be adjusted by many parameters

### Structure

ShinyProt consists of a set of R scripts that determine the functionality.

- app.R contains the main body of functions. It is devided into a GUI and a server part. The GUI contains the interactive modules such as sliders and check boxes. The server obtains input parameters from the GUI and adjusts the graphical output accordingly (changes charts on the fly)
- custom.themes.R contains a set of customized lattice themes

### Input data

- ShinyProt uses protein quantifications obtained from LC-MS-MS experiments
- Can be easily customized for use with other gene-expression type data
- Can be deployed on a shiny server for web-access

### Getting started

To start ShinyMC, you need to have R (optionally also Rstudio) and some of its libraries installed, mainly:

- shiny
- shinythemes
- lattice
- latticeExtra
- tidyr
- plyr

Open app.R in RStudio and push the 'Run App' button

OR

https://m-jahn.shinyapps.io/ShinyMC/
