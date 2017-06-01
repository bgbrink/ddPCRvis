library(shiny)
library(shinyjs)
library(rhandsontable)
library(ggplot2)
library(parallel)

# Check if submodule folder is present 
tryCatch( {
  # load package w/o installing
  library(devtools)
  load_all('dropClust/')
}, error = function() {
  # load dropClust package
  library(dropClust)
})
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll set limit to 1MB.
options(shiny.maxRequestSize = 1*1024^2)

myNames <<- c("Empties","1","2","3","4","1+2","1+3","1+4","2+3","2+4","3+4","1+2+3","1+2+4","1+3+4","2+3+4","1+2+3+4","Removed","Total")

shinyServer(function(input, output, session) {
  
  #### Reseting Parameters for each session ####
  responses <<- list()
  makeReactiveBinding("responses")
  tmpResult <<- list()
  makeReactiveBinding("tmpResult")
  filesAnalyzed <<- list()
  makeReactiveBinding("filesAnalyzed")
  superResults <<- list()
  makeReactiveBinding("superResults")
  files <<- list()
  plots <<- list()
  template <<- NULL
  ##############################################
  
  # reactive table for rhandsontable
  values <- reactiveValues() 
  setHot <- function(x) values[["hot"]] = x
  
  observe({
    if (!is.null(input$template)) {
      DF <- hot_to_r(input$template)
      setHot(DF)
    }
  })
  
  output$template <- renderRHandsontable({
    if (!is.null(values[["hot"]])) {
      DF <- values[["hot"]]
      if (any(DF[,8])) {
        enable('run')
      } else {
        disable('run')
      }
    } else {
      disable('run')
      return(NULL)
    }
    
    rhandsontable(DF, rowHeaders = NULL) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  observeEvent(input$templateFile, {
    DF <- NULL
    ######
    Header <- readLines(input$templateFile$datapath[1], n = 1)
    Header <- gsub("[\\\"]", "", Header)
    if (substr(Header, start = 1, stop = 1) == ">") {
      ### validate stuff
      template <<- read.csv(input$templateFile$datapath[1], skip = 1)
      
      DF <- template
      Analyze <- rep(F, nrow(DF))
      DF = cbind(DF, Analyze)
      
      setHot(DF)
    } else {
      info(paste("Invalid Template file! This file starts with:\n", substr(Header, start = 1, stop = 10)))
    }
    
    #   enable('files')
    ######
  }   )
  
  observeEvent(input$files, {
    numberOfFiles <- length(input$files$datapath)
    if (is.null(template)) {
      template <<- data.frame()
      columnNames <- c('Well','Sample name','# of markers','Marker 1','Marker 2','Marker 3','Marker 4')
      df <- list()
      for (i in 1:numberOfFiles) {
        filename <- input$files$name[i]
        explode <- unlist(strsplit(filename, "_"))
        id <- grep("^[[:upper:]][[:digit:]][[:digit:]]$", explode, value = T)
        newRow <- cbind(id, filename, '4', 'M1','M2','M3','M4')
        colnames(newRow) <- columnNames
        template <<- rbind(template, newRow)
        file <- input$files$datapath[i]
        names(file) <- input$files$name[i]
        files[[id]] <<- file
      }
      DF <- NULL
      DF <- template
      Analyze <- rep(T, nrow(DF))
      DF = cbind(DF, Analyze)
      setHot(DF)
    } else {
      DF <- values[["hot"]]
      for (i in 1:numberOfFiles) {
        filename <- input$files$name[i]
        explode <- unlist(strsplit(filename, "_"))
        id <- grep("^[[:upper:]][[:digit:]][[:digit:]]$", explode, value = T)
        pos <- match(id, DF[,1])
        if (!is.na(pos)) {
          file <- input$files$datapath[i]
          names(file) <- input$files$name[i]
          files[[id]] <<- file
          ## make green check V
          DF[pos,8] <- T
        }
      }
      setHot(DF)
    }
  })
  
  observeEvent(input$run, {
    
    DF <- values[["hot"]]
    filesToAnalyze <- subset(DF, DF[,8] == T)
    
    check <- which(!filesToAnalyze[,1] %in% names(files))
    
    if (length(check) > 0) {
      info(paste("No files uploaded for the selected well(s)", filesToAnalyze[,1][check]))
      return(NULL)
    }
    if (Sys.info()['sysname'] == "Windows") {
      nrOfCores <- 1
    } else {
      nrOfCores <- detectCores()
    }
    
    numberOfAlgorithms <- input$dens + input$sam + input$peaks
    if (numberOfAlgorithms > 0) {
      sens <- input$sensitivity
      csvFiles <- mclapply(files, read.csv, mc.cores = nrOfCores)
      selectedRows <- subset(DF, DF[,8] == TRUE)
      filesAnalyzed <<- as.character(selectedRows[[1]])
      plex <- sapply(names(csvFiles), function(x) {as.numeric(as.character(selectedRows[which(filesAnalyzed == x), 3]))})
      markerNames <- lapply(names(csvFiles), function(x) {unlist(selectedRows[which(filesAnalyzed == x), 4:7])})
      dens_result <- sam_result <- peaks_result <- rep(0, length(csvFiles))
      names(dens_result) <- names(sam_result) <- names(peaks_result) <- filesAnalyzed
      withProgress(message = "Calculating:", value = 0, {
        if(input$dens) {
          incProgress(1/(numberOfAlgorithms+1), detail = "flowDensity")
          dens_result <- mcmapply(dens_wrapper, File=csvFiles, NumOfMarkers=plex, sensitivity=sens, markerNames=markerNames, SIMPLIFY = F, mc.cores = nrOfCores)
        }
        if(input$sam) {
          incProgress(1/(numberOfAlgorithms+1), detail = "SamSPECTRAL")
          sam_result <- mcmapply(sam_wrapper, File=csvFiles, NumOfMarkers=plex, sensitivity=sens, markerNames=markerNames, SIMPLIFY = F, mc.cores = nrOfCores)
        }
        if(input$peaks) {
          incProgress(1/(numberOfAlgorithms+1), detail = "flowPeaks")
          peaks_result <- mcmapply(peaks_wrapper, File=csvFiles, NumOfMarkers=plex, sensitivity=sens, markerNames=markerNames, SIMPLIFY = F, mc.cores = nrOfCores)
        }
        incProgress(1/(numberOfAlgorithms+1), detail = "cluster ensemble")
        superResults <<- mcmapply(ensemble_wrapper, dens_result, sam_result, peaks_result, csvFiles, SIMPLIFY = F, mc.cores = nrOfCores)
        updateNavbarPage(session, 'mainPage', selected = 'Clustering')
      })
    } else {
      info("No algorithm selected!")
      return(NULL)
    }
  })
  
  createOriginalPlots <- reactive ({
    numberOfFiles <- length(files)
    
    if (numberOfFiles == 0)
      return(NULL)
    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    for (i in filesAnalyzed) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        plotname <- paste("originalPlot", my_i, sep="")
        File <- read.csv(files[[my_i]])
        File <- File[,c(2,1)]
        output[[plotname]] <- renderPlot({
          p <- ggplot(data = File, mapping = aes(x = Ch2.Amplitude, y = Ch1.Amplitude))
          p <- p + geom_point(color = "dimgrey", size = .5) + ggtitle(my_i) + theme_bw()+ theme(legend.position="none")
          p
        })
        
      })
    }
  })
  
  
  # Insert the right number of plot output objects into the web page
  output$originalPlot <- renderUI({
    
    numberOfFiles <- length(files)
    
    if (numberOfFiles == 0) {
      column(5,
             p(strong("Original Data:"))
      )
    } else {
      column(5,
             p(strong("Original Data:")),{
               
               createOriginalPlots()
               
               plot_output_list <- lapply(1:length(filesAnalyzed), function(i) {
                 plotname <- paste("originalPlot", filesAnalyzed[i], sep="")
                 plotOutput(plotname)
               })
               
               
               # Convert the list to a tagList - this is necessary for the list of items
               # to display properly.
               do.call(tagList, plot_output_list)
             })
    }
  })
  
  createClusterPlots <- reactive ({
    numberOfFiles <- length(superResults)
    plots <<- list()
    if (numberOfFiles == 0)
      return(NULL)
    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    for (i in filesAnalyzed) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        plotname <- paste("clusterPlot", my_i, sep="")
        result <- superResults[[my_i]]
        if (is.null(result)) {
          File <- read.csv(files[[my_i]])
          File <- File[,c(2,1)]
          output[[plotname]] <- renderPlot({
            p <- ggplot(data = File, mapping = aes(x = Ch2.Amplitude, y = Ch1.Amplitude))
            p <- p + geom_point(color = "dimgrey", size = .5) + ggtitle(paste(my_i, "[Error!]")) + theme_bw()+ theme(legend.position="none")
            plots[[length(plots)+1]] <<- p
            p
          })
        } else {
          if (length(result$counts) == 18) {
            cbPalette <- c("#999999", "#f272e6","#e5bdbe","#bf0072","#cd93c5","#5e7f65","#1fba00","#2c5d26","#bdef00","#529900","#ffe789","#575aef","#a3b0fa","#005caa","#019df8", "#bc8775")
          } else if (length(result$counts) == 10) {
            cbPalette <- c("#999999", "#d800c4","#fca3a7","#bb004e", "#70cf56","#8b9d61","#ccd451", "#bc8775")
          } else if (length(result$counts) == 6) {
            cbPalette <- c("#999999", "#8d5286","#b42842", "#c2ff79","#076633", "#bc8775")
          } else {
            cbPalette <- c("#999999", "#bc8775")
          }
          output[[plotname]] <- renderPlot({
            p <- ggplot(data = result$data, mapping = aes(x = Ch2.Amplitude, y = Ch1.Amplitude))
            p <- p + geom_point(aes(color = factor(Cluster)), size = .5, na.rm = T) + ggtitle(my_i) + theme_bw()+ theme(legend.position="none") + scale_colour_manual(values=cbPalette, limits = 1:(length(result$counts)-2))
            #             for (j in 1:nrow(result$firstClusters)) {
            #               if(result$firstClusters[j] == 0) next
            #               p <- p + annotate("text", x = result$firstClusters[j,1], y = result$firstClusters[j,2], label = j)
            #             }
            plots[[length(plots)+1]] <<- p
            p
          })
        }
      })
    }
  })
  
  
  # Insert the right number of plot output objects into the web page
  output$clusterPlot <- renderUI({
    
    numberOfFiles <- length(superResults)
    
    if (numberOfFiles == 0) {
      column(5,
             p(strong("Clustering:"))
      )
    } else {
      column(5,
             p(strong("Clustering:")),{
               
               createClusterPlots()
               
               plot_output_list <- lapply(1:length(filesAnalyzed), function(i) {
                 plotname <- paste("clusterPlot", filesAnalyzed[i], sep="")
                 plotOutput(plotname)
               })
               
               # Convert the list to a tagList - this is necessary for the list of items
               # to display properly.
               do.call(tagList, plot_output_list)
             })
    }
  })
  
  
  createConfidence <- reactive ({
    numberOfFiles <- length(superResults)
    
    if (numberOfFiles == 0)
      return(NULL)
    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    for (i in filesAnalyzed) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        plotname <- paste("confidenceText", my_i, sep="")
        result <- superResults[[my_i]]
        if (is.null(result)) {
          output[[plotname]] <- renderUI({
            paste("test" , my_i)
          })
        } else {
          output[[plotname]] <- renderUI({
            str1 <- paste(my_i, "<br/>The agreement between the algorithms is:")
            str2 <- paste(round(result$confidence*100, 2), " %")
            HTML(paste(str1, str2, sep = '<br/>'))
          })
        }
      })
    }
  })
  
  
  # Insert the right number of plot output objects into the web page
  output$confidence <- renderUI({
    
    numberOfFiles <- length(superResults)
    
    if (numberOfFiles == 0) {
      column(2,
             p(strong("Confidence:"))
      )
    } else {
      column(2,
             p(strong("Confidence:")),{
               
               createConfidence()
               
               text_output_list <- lapply(1:length(filesAnalyzed), function(i) {
                 plotname <- paste("confidenceText", filesAnalyzed[i], sep="")
                 htmlOutput(plotname, style="height:400px;")
               })
               
               # Convert the list to a tagList - this is necessary for the list of items
               # to display properly.
               do.call(tagList, text_output_list)
             })
    }
  })
  
  observeEvent(input$editButton, {
    updateNavbarPage(session, 'mainPage', selected = 'Edit Clustering')
  })
  
  observeEvent(input$continueButton, {
    responses <<- list()
    template <- as.matrix(template)
    countedSuper <- calculateCPDs(results = superResults, template = template)
    for (i in filesAnalyzed) {
      local({
        my_i <- i
        myCounts <- c(my_i, superResults[[my_i]]$counts)
        myCountedMarkers <- countedSuper[[my_i]]
        markers <- names(myCountedMarkers)
        sampleName <- template[template[,1] == my_i,2]
        for (j in markers) {
          tmp <- myCountedMarkers[[j]]
          if (length(tmp) == 1) next
          markerRow <- c(my_i, sampleName, j, tmp$counts, tmp$cpd)
          names(markerRow) <- c("Well","Sample name", "Marker", "droplet count", "CPD")
          isolate(saveData(markerRow, "Marker"))
        }
        isolate(saveData(myCounts, "Cluster"))
      })
    }
    updateNavbarPage(session, 'mainPage', selected = 'Results')
  })
  
  observeEvent(input$continueButton2, {
    responses <<- list()
    template <- as.matrix(template)
    countedSuper <- calculateCPDs(results = superResults, template = template)
    for (i in filesAnalyzed) {
      local({
        my_i <- i
        myCounts <- c(my_i, superResults[[my_i]]$counts)
        myCountedMarkers <- countedSuper[[my_i]]
        markers <- names(myCountedMarkers)
        sampleName <- template[template[,1] == my_i,2]
        for (j in markers) {
          tmp <- myCountedMarkers[[j]]
          if (length(tmp) == 1) next
          markerRow <- c(my_i, sampleName, j, tmp$counts, tmp$cpd)
          names(markerRow) <- c("Well","Sample name", "Marker", "droplet count", "CPD")
          isolate(saveData(markerRow, "Marker"))
        }
        isolate(saveData(myCounts, "Cluster"))
      })
    }
    updateNavbarPage(session, 'mainPage', selected = 'Results')
  })
  
  output$clueResults <- renderDataTable({
    loadData("Cluster")
  })
  
  output$markerResults <- renderDataTable({
    loadData("Marker")
  })
  
  output$downloadPlots <- downloadHandler(
    filename = function(){
      # Time-stamp tar filename
      paste0("plots-", gsub("\\D", "_", Sys.time()), ".zip")
    },
    content = function(file){
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      if(is.null(filesAnalyzed)){
        # Protect against empty data.
        return(NULL)
      }
      tempdir = paste0(tempdir(),"/", as.integer(Sys.time()), "/")
      dir.create(tempdir, showWarnings = F)
      for (i in 1:length(filesAnalyzed)) {
        if (length(plots) >= i) ggsave(paste0(tempdir, "Plot_", filesAnalyzed[i], ".png"), plots[[i]], device = device)
      }
      zip(zipfile = file, files = tempdir, flags = "-rj9X")
    })
  
  output$downloadRawData <- downloadHandler(
    filename = function(){
      # Time-stamp zip filename
      paste0("data-", gsub("\\D", "_", Sys.time()), ".zip")
    },
    content = function(file){
      if(is.null(filesAnalyzed)){
        # Protect against empty data.
        return(NULL)
      }
      tempdir = paste0(tempdir(),"/", as.integer(Sys.time()), "/")
      dir.create(tempdir, showWarnings = F)
      for (i in 1:length(filesAnalyzed)) {
        if (length(superResults) >= i) write.csv(superResults[[i]]$data, file = paste0(tempdir, "Data_", filesAnalyzed[i], ".csv"))
      }
      zip(zipfile = file, files = tempdir, flags = "-rj9X")
    })
  
  output$downloadData <- downloadHandler(
    filename = function(){
      # Time-stamp zip filename
      paste0("results-", gsub("\\D", "_", Sys.time()), ".zip")
    },
    content = function(file){
      if(length(responses) == 0){
        # Protect against empty data.
        return(NULL)
      }
      tempdir = paste0(tempdir(),"/", as.integer(Sys.time()), "/")
      dir.create(tempdir, showWarnings = F)
      for (i in 1:length(responses)) {
        if (length(responses) >= i) write.csv(responses[[i]], file = paste0(tempdir, "Results_", names(responses)[i], ".csv"))
      }
      zip(zipfile = file, files = tempdir, flags = "-rj9X")
    })
  
  observeEvent(input$resetData, {
    responses <<- list()
  })
  
  saveData <- function(data, algorithm) {
    data <- as.data.frame(t(data))
    if (is.null(responses[[algorithm]])) {
      responses[[algorithm]] <<- rbind(responses[[algorithm]], data)
    } else if (ncol(responses[[algorithm]]) < ncol(data)) {
      responses[[algorithm]] <<- merge(responses[[algorithm]], data, all.x=T)
      responses[[algorithm]] <<- rbind(responses[[algorithm]], data[match(colnames(responses[[algorithm]]), colnames(data))])
    } else {
      data <- merge(responses[[algorithm]], data, all.y=T)
      responses[[algorithm]] <<- rbind(responses[[algorithm]], data[match(colnames(responses[[algorithm]]), colnames(data))])
    }
  }
  
  loadData <- function(algorithm) {
    responses[[algorithm]]
  }
  
  observe({
    updateSelectInput(session, 'well', choices = filesAnalyzed)
  })
  
  
  output$originalPlotEdit <- renderUI({
    column(5,
           p(strong("Original Data:")),{
             File <- read.csv(files[[input$well]])
             File <- File[,c(2,1)]
             output[['original_Plot']] <- renderPlot({
               p <- ggplot(data = File, mapping = aes(x = Ch2.Amplitude, y = Ch1.Amplitude))
               p <- p + geom_point(color = "dimgrey", size = .5, na.rm = T) + ggtitle(input$well) + theme_bw()+ theme(legend.position="none")
               p
             })
             plotOutput('original_Plot', brush = brushOpts("selected_brush"))
           })
  })
  
  output$clusterPlotEdit <- renderUI({
    
    tmpResult <<- superResults[[input$well]]
    
    column(5,
           p(strong("Clustering")),{
             output[['selected_Plot']] <- renderPlot({
               if (length(tmpResult$counts) == 18) {
                 cbPalette <- c("#999999", "#f272e6","#e5bdbe","#bf0072","#cd93c5","#5e7f65","#1fba00","#2c5d26","#bdef00","#529900","#ffe789","#575aef","#a3b0fa","#005caa","#019df8", "#bc8775")
               } else if (length(tmpResult$counts) == 10) {
                 cbPalette <- c("#999999", "#d800c4","#fca3a7","#bb004e", "#70cf56","#8b9d61","#ccd451", "#bc8775")
               } else if (length(tmpResult$counts) == 6) {
                 cbPalette <- c("#999999", "#8d5286","#b42842", "#c2ff79","#076633", "#bc8775")
               } else {
                 cbPalette <- c("#999999", "#bc8775")
               }
               p <- ggplot(data = tmpResult$data, mapping = aes(x = Ch2.Amplitude, y = Ch1.Amplitude))
               p <- p + geom_point(aes(color = factor(Cluster)), size = .5, na.rm = T) + ggtitle(input$well) + theme_bw()+ theme(legend.position="none") + scale_colour_manual(values=cbPalette, limits = 1:(length(tmpResult$counts)-2))
               p
             })
             plotOutput('selected_Plot')
           })
  })
  
  
  observe({
    result <- superResults[[input$well]]
    
    if (!is.null(input$selected_brush)) {
      selectionDF <- brushedPoints(result$data, input$selected_brush, xvar = 'Ch2.Amplitude', yvar = 'Ch1.Amplitude', allRows = T)
      selectionDF[selectionDF$selected_ == T, 3] <- as.numeric(input$clusters)
      result$data <- selectionDF
    }
    tmpResult <<- result
  })
  
  observeEvent(input$cancel,{
    tmpResult <<- superResults[[input$well]]
  })
  
  observeEvent(input$save,{
    plex <- as.numeric(as.character(template[template[1] == input$well,][[3]]))
    newCounts <- table(c(tmpResult$data[,3], 1:2^plex))-1
    newCounts <- c(newCounts, tail(tmpResult$counts, n = 2))
    switch(as.character(length(newCounts)),
           '4' = names(newCounts) <- c("Empties","1","Removed","Total"),
           '6' = names(newCounts) <- c("Empties","1","2","1+2","Removed","Total"),
           '10' = names(newCounts) <- c("Empties","1","2","3","1+2","1+3","2+3","1+2+3","Removed","Total"),
           '18' = names(newCounts) <- c("Empties","1","2","3","4","1+2","1+3","1+4","2+3","2+4","3+4","1+2+3","1+2+4","1+3+4","2+3+4","1+2+3+4","Removed","Total"))
    tmpResult$counts <- newCounts
    superResults[[input$well]] <<- tmpResult 
  })
  
  # observe({
  #   choices <- c(2:16, 1)
  #   plex <- as.character(template[template[1] == input$well,][[3]])
  #   if (length(plex) < 1) plex <- 0
  #   switch(as.character(plex),
  #          '1' = {choices <- c(2,1)
  #          names(choices) <- myNames[c(1,16)]},
  #          '2' = {choices <- c(2:4,1)
  #          names(choices) <- myNames[c(1,2,5,16)]},
  #          '3' = {choices <- c(2:8,1)
  #          names(choices) <- myNames[c(1,2,3,5,6,8,11,16)]},
  #          '4' = {choices <- c(2:16, 1)
  #          names(choices) <- myNames[1:16]})
  #   updateRadioButtons(session, 'clusters', 'Edit Cluster:', choices, inline = T)
  # })
  
  observeEvent(input$rerunButton, {
    id <- input$well
    names(id) = 'ID'
    numberOfAlgorithms <- input$dens + input$sam + input$peaks
    if (numberOfAlgorithms > 0) {
      plex <- as.numeric(as.character(subset(template, template[,1] == id)[[3]]))
      markerNames <- unlist(subset(template, template[,1] == id)[4:7])
      if (is.null(plex)) stop()
      dens_result <- sam_result <- peaks_result <- 0
      sens <- input$sensitivity
      csvFiles <- read.csv(files[[id]])
      withProgress(message = "Calculating:", value = 0, {    
        if(input$dens) {
          incProgress(1/(numberOfAlgorithms+1), detail = "flowDensity")
          dens_result <- dens_wrapper(File=csvFiles, NumOfMarkers=plex, sensitivity=sens, markerNames = markerNames)
        }
        if(input$sam) {
          incProgress(1/(numberOfAlgorithms+1), detail = "SamSPECTRAL")
          sam_result <- sam_wrapper(File=csvFiles, NumOfMarkers=plex, sensitivity=sens, markerNames = markerNames)
        }
        if(input$peaks) {
          incProgress(1/(numberOfAlgorithms+1), detail = "flowPeaks")
          peaks_result <- peaks_wrapper(File=csvFiles, NumOfMarkers=plex, sensitivity=sens, markerNames = markerNames)
        }
        incProgress(1/(numberOfAlgorithms+1), detail = "cluster ensemble")
        superResults[[id]] <<- ensemble_wrapper(dens_result, sam_result, peaks_result, csvFiles)
      })
    } else {
      info("No algorithm selected!")
    }
  })
})


dens_wrapper <- function(File, sensitivity=1, NumOfMarkers, markerNames) {
  missingClusters <- which(markerNames == "")
  result <- tryCatch(runDensity(File[,c(2,1)], sensitivity, NumOfMarkers, missingClusters), error = function(e) {
    print(e)
  })
}

sam_wrapper <- function(File, sensitivity=1, NumOfMarkers, markerNames) {
  missingClusters <- which(markerNames == "")
  result <- tryCatch(runSam(File[,c(2,1)], sensitivity, NumOfMarkers, missingClusters), error = function(e) {
    print(e)
  })
}

peaks_wrapper <- function(File, sensitivity=1, NumOfMarkers, markerNames) {
  missingClusters <- which(markerNames == "")
  result <- tryCatch(runPeaks(File[,c(2,1)], sensitivity, NumOfMarkers, missingClusters), error = function(e) {
    print(e)
  })
}

ensemble_wrapper <- function(dens_result, sam_result, peaks_result, file) {
  if (is.numeric(dens_result)) {
    dens_result <- NULL
  }
  if (is.numeric(sam_result)) {
    sam_result <- NULL
  }
  if (is.numeric(peaks_result)) {
    peaks_result <- NULL
  }
  result <- tryCatch(createEnsemble(dens_result, sam_result, peaks_result, file[,c(2,1)]), error = function(e) {
    print(e)
  })
}
