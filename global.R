library(shiny)
library(shinyjs)
library(rhandsontable)
library(jsonlite)
library(ggplot2)
library(parallel)
library(R.utils)
library(plotly)

# Check if submodule folder is present 
tryCatch( {
  # load package w/o installing
  library(devtools)
  if (dir.exists('/srv/shiny-server/ddPCRvis/ddPCRclust')) {
    load_all('/srv/shiny-server/ddPCRvis/ddPCRclust')
  } else {
    load_all('ddPCRclust/')
  }
}, error = function(e) {
  # load ddPCRclust package
  library(ddPCRclust)
})

myNames <<- c("Empties","1","2","3","4","1+2","1+3","1+4","2+3","2+4","3+4","1+2+3","1+2+4","1+3+4","2+3+4","1+2+3+4","Removed","Total")
