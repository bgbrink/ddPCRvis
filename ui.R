library(shiny)
library(shinyjs)
library(rhandsontable)
myNames <<- c("Empties","1","2","3","4","1+2","1+3","1+4","2+3","2+4","3+4","1+2+3","1+2+4","1+3+4","2+3+4","1+2+3+4","Removed","Total")

shinyUI(tagList(useShinyjs(),
                navbarPage("dropVis v0.92", id = "mainPage",
                           tabPanel("Upload Files", id='panel1',
                                    sidebarLayout(
                                      sidebarPanel(
                                        fileInput('templateFile', 'Choose template to upload',
                                                  multiple = F,
                                                  accept = c(
                                                    'text/csv',
                                                    'text/comma-separated-values',
                                                    'text/tab-separated-values',
                                                    'text/plain',
                                                    '.csv',
                                                    '.tsv'
                                                  )
                                        ),
                                        p("First, upload a single template file, which specifies the setup of the ddPCR reactions.
                                          For specifications on how this file needs to be formatted, please check the ", a(href="https://github.com/bgbrink/dropClust", target="_blank", "dropClust"), "page on Github."),
                                        br(),
                                        fileInput('files', 'Choose file(s) to upload',
                                                  multiple = T,
                                                  accept = c(
                                                    'text/csv',
                                                    'text/comma-separated-values',
                                                    'text/tab-separated-values',
                                                    'text/plain',
                                                    '.csv',
                                                    '.tsv'
                                                  )
                                        ),
                                        p("Then, upload the raw data files from the ddPCR run.
                                          For specifications on how these files needs to be formatted, please check the ", a(href="https://github.com/bgbrink/dropClust", target="_blank", "dropClust"), "page on Github."),
                                        br(),
                                        checkboxInput('quick', 'Run fast, but less precise. Useful for very clean data or to get a quick overview.', FALSE),
                                        actionButton('run', "Run Analysis!", icon("paper-plane"), 
                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                        width = 2
                                        ),
                                      mainPanel(
                                        fluidRow(
                                          rHandsontableOutput('template')
                                        ),
                                        width=10))
                                    ),
                           tabPanel("Clustering", id='panel2',
                                    sidebarLayout(
                                      sidebarPanel(
                                        #                                         sliderInput("formatSam", "Sensitivity (SamSPECTRAL)",
                                        #                                                     min = .5, max = 2, value = 1, step = .5),
                                        #                                         sliderInput("formatPeaks", "Sensitivity (flowPeaks)",
                                        #                                                     min = .5, max = 2, value = 1, step = .5),
                                        #                                         p("Adjust the sensitivity to find more (higher value) or less (lower value) cluster."),
                                        #                                         br(),
                                        downloadButton('downloadRawData', "Download raw data"),
                                        downloadButton('downloadPlots', "Download all plots"),
                                        p("Download the raw data with a cluster number assigned to each point and the respective plots (this may take a while!)"),
                                        p("If you are unhappy with the automatic clustering, you can re-run it or change it manually, using link and brush:"),
                                        actionButton('editButton', "Edit clustering", icon("pencil-square-o")),
                                        p("When you are satisfied, make sure you hit the following button to count the droplets and save the results:"),
                                        actionButton('continueButton', "Count droplets!", icon("check-square"), 
                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                        width = 2
                                      ),
                                      mainPanel(
                                        fluidRow(
                                          uiOutput('originalPlot'),
                                          uiOutput('clusterPlot'),
                                          htmlOutput('confidence')
                                        ),
                                        width=10))
                           ),
                           tabPanel("Edit Clustering", id='panel4',
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("well", "Select file:", NULL),
                                        #checkboxGroupInput('markers', 'Show Marker(s):', c('1', '2', '3', '4'), c(1,2,3,4), inline = T),
                                        p("Select the cluster you want to edit and draw a rectangle around the droplets that should be assigned to it in the black-and-white plot. Click 'save' to save your changes and continue with the next cluster or well."),
                                        radioButtons('clusters', NULL, choiceNames = myNames[1:16], choiceValues = c(1:16), inline = T),
                                        actionButton('cancel', "Revert changes", icon("undo")),
                                        actionButton('save', "Save changes", icon("floppy-o")),
                                        sliderInput("sensitivity", "Sensitivity",
                                                    min = .25, max = 2, value = 1, step = .25),
                                        p("Adjust the sensitivity to find more (higher value) or less (lower value) cluster. Click the Re-Run button to run dropClust again for the selected file."),
                                        p("When you are satisfied, make sure you hit the following button to count the droplets and save the results:"),
                                        actionButton('rerunButton', "Re-Run Algorithm", icon("refresh")),
                                        actionButton('continueButton2', "Count droplets!", icon("check-square"), 
                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                        width = 2
                                      ),
                                      mainPanel(
                                        fluidRow(
                                          uiOutput('originalPlotEdit'),
                                          uiOutput('clusterPlotEdit'),
                                          htmlOutput('confidenceEdit')
                                        ),
                                        width=10))
                           ),
                           tabPanel("Results", id='panel3',
                                    sidebarLayout(
                                      sidebarPanel(
                                        p("This page contains the numerical results of the clustering. It shows both the raw droplet count per cluster, as well as the
                                          results according to each marker taken from the template, with its respective copies per droplet (CPD). 
                                          You can download the spreadsheets:"),
                                        downloadButton('downloadData', 'Download'),
                                        actionButton('resetData', 'Clear', icon("trash-o")),
                                        width = 2
                                        ),
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel("Results per Cluster", dataTableOutput('clueResults')),
                                          tabPanel("Results per Marker", dataTableOutput('markerResults'))
                                        ),
                                        width=10)))
                           
                           )))
