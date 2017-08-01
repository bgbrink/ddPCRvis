library(shiny)
library(shinyjs)
library(rhandsontable)
library(jsonlite)
library(ggplot2)
library(parallel)
library(R.utils)

# Check if submodule folder is present 
tryCatch( {
  # load package w/o installing
  library(devtools)
  if (dir.exists('/srv/shiny-server/dropVis/dropClust')) {
    load_all('/srv/shiny-server/dropVis/dropClust')
  } else {
    load_all('dropClust/')
  }
}, error = function(e) {
  # load dropClust package
  library(dropClust)
})

myNames <<- c("Empties","1","2","3","4","1+2","1+3","1+4","2+3","2+4","3+4","1+2+3","1+2+4","1+3+4","2+3+4","1+2+3+4","Removed","Total")
