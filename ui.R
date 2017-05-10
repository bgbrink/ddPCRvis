library(shiny)
library(shinyjs)
library(rhandsontable)

shinyUI(tagList(useShinyjs(),
                navbarPage("dropVis v0.9", id = "mainPage",
                           tabPanel("Upload Files", id='panel1',
                                    sidebarLayout(
                                      sidebarPanel(
                                        fileInput('templateFile', 'Please upload the template',
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
                                          Specifications on how this file needs to be formatted can be found", a("here"), "."),
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
                                        p("After the template has been succesfully set up, please upload the raw data files from the ddPCR run.
                                          Specifications on how these files need to be formatted can be found", a("here"), "."),
                                        br(),
                                        checkboxInput('dens', 'Run flowDensity', TRUE),
                                        checkboxInput('sam', 'Run SamSPECTRAL', TRUE),
                                        checkboxInput('peaks', 'Run flowPeaks', TRUE),
                                        actionButton('run', "Run Analysis!"),
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
                                        p("(This may take a while!)"),
                                        p("If you are unhappy with the automatic clustering, you can change it manually:"),
                                        actionButton('editButton', "Edit clustering"),
                                        p("When you are satisfied, make sure you hit the following button to count the droplets and save the results:"),
                                        actionButton('continueButton', "Count droplets!"),
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
                                        selectInput("well", "Select Well:", NULL),
                                        #checkboxGroupInput('markers', 'Show Marker(s):', c('1', '2', '3', '4'), c(1,2,3,4), inline = T),
                                        p("Select the cluster you want to edit and draw a rectangle around the droplets that should be assigned to it in the black-and-white plot. Click 'save' to save your changes and continue with the next cluster or well."),
                                        radioButtons('clusters', NULL, c(2:16, 1), inline = T),
                                        actionButton('cancel', "Cancel"),
                                        actionButton('save', "Save"),
                                        sliderInput("sensitivity", "Sensitivity",
                                                    min = .25, max = 2, value = 1, step = .25),
                                        p("Adjust the sensitivity to find more (higher value) or less (lower value) cluster. Click the Re-Run button to apply the change."),
                                        p("When you are satisfied, make sure you hit the following button to count the droplets and save the results:"),
                                        actionButton('rerunButton', "Re-Run Algorithm"),
                                        actionButton('continueButton2', "Count droplets!"),
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
                                        p("Here you can see the results of the clustering. 
                                          The second tab shows results according to each marker taken from the template, with its respective copies per droplet (CPD). 
                                          You can download the spreadsheets:"),
                                        downloadButton('downloadData', 'Download'),
                                        actionButton('resetData', 'Clear'),
                                        width = 2
                                        ),
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel("Results per Cluster", dataTableOutput('clueResults')),
                                          tabPanel("Results per Marker", dataTableOutput('markerResults'))
                                        ),
                                        width=10)))
                           
                           )))
