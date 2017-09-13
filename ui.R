shinyUI(tagList(useShinyjs(),
                # Include IntroJS styling
                includeCSS("wwwIntroJS/introjs.min.css"),
                
                # Include styling for the app
                includeCSS("wwwIntroJS/app.css"),
                
                # Include IntroJS library
                includeScript("wwwIntroJS/intro.min.js"),
                
                # Include JavaScript code to make shiny communicate with introJS
                includeScript("wwwIntroJS/app.js"),
                
                # Add includes to the head of the page using the resource path
                actionButton('helpButton', 'Help', icon("question-circle"), style = "background-color:green;color:white;position:fixed;right:40px;top:10px;z-index:100000000000", onclick="startHelp();"),
                navbarPage("dropVis v0.99", id = "mainPage",
                           tabPanel("Upload Files", id='panel1',
                                    sidebarLayout(
                                      sidebarPanel(
                                        p("Welcome to dropVis, a visual interface for the dropClust package! If this is your first time here, click the green button in the top right corner."),
                                        div(id="stepTemplate",
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
                                        )),
                                        div(id="stepFiles",
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
                                        )),
                                        div(id="stepQuick",
                                        checkboxInput('quick', 'Run fast version', TRUE)),
                                        div(id="stepRun",
                                        actionButton('run', "Start Analysis!", icon("paper-plane"), 
                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                        width = 2
                                        ),
                                      mainPanel(
                                        fluidRow(
                                          div(id="stepOutput",
                                          rHandsontableOutput('template'))
                                        ),
                                        width=10))
                                    ),
                           tabPanel("Clustering", id='panel2',
                                    sidebarLayout(
                                      sidebarPanel(
                                        p(strong("Please wait for the plots to appear on the right...")),
                                        div(id="stepDownload",
                                        downloadButton('downloadRawData', "Download raw data"),
                                        downloadButton('downloadPlots', "Download all plots")),
                                        div(id="stepEdit",
                                        p("If you are unhappy with the automatic clustering, you can re-run it or change it manually, using link and brush:"),
                                        actionButton('editButton', "Edit clustering", icon("pencil-square-o"))),
                                        div(id="stepCount",
                                        p("When you are satisfied, make sure you hit the following button to count the droplets and save the results:"),
                                        actionButton('continueButton', "Count Droplets!", icon("check-square"), 
                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                        width = 2
                                      ),
                                      mainPanel(
                                        fluidRow(
                                          div(id="stepPlots",
                                          uiOutput('originalPlot'),
                                          uiOutput('clusterPlot'),
                                          htmlOutput('confidence'))
                                        ),
                                        width=10))
                           ),
                           tabPanel("Edit Clustering", id='panel3',
                                    sidebarLayout(
                                      sidebarPanel(
                                        div(id="stepSelect",
                                        selectInput("well", "Select file:", NULL),
                                        p("Select the cluster you want to edit and draw a rectangle around the droplets that should be assigned to it in the black-and-white plot. Click 'Save Changes' to save your changes and continue with the next cluster or well."),
                                        radioButtons('clusters', NULL, choiceNames = myNames[1:16], choiceValues = c(1:16), inline = T),
                                        actionButton('cancel', "Revert Changes", icon("undo")),
                                        actionButton('save', "Save Changes", icon("floppy-o"))),
                                        div(id="stepRerun",
                                        sliderInput("sensitivity", "Sensitivity",
                                                    min = .25, max = 2, value = 1, step = .25),
                                        actionButton('rerunButton', "Re-Run Algorithm", icon("refresh"))),
                                        div(id="stepCount2",
                                        p("When you are satisfied, make sure you hit the following button to count the droplets and save the results:"),
                                        actionButton('continueButton2', "Count Droplets!", icon("check-square"), 
                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                        width = 2
                                      ),
                                      mainPanel(
                                        fluidRow(
                                          div(id="stepEditPlots",
                                          uiOutput('originalPlotEdit'),
                                          uiOutput('clusterPlotEdit'),
                                          htmlOutput('confidenceEdit'))
                                        ),
                                        width=10))
                           ),
                           tabPanel("Counts", id='panel4',
                                    sidebarLayout(
                                      sidebarPanel(
                                        div(id="stepDownCounts",
                                        downloadButton('downloadData', 'Download'),
                                        actionButton('resetData', 'Clear', icon("trash-o")),
                                        p("You can download the spreadsheets or clear all results to start over.")),
                                        div(id="stepCalcCPDs",
                                        selectizeInput("cControl", "Select constant control:", NULL),
                                        actionButton('continueCPDs', "Calculate CPDs", icon("check-square"), 
                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                        width = 2
                                        ),
                                      
                                      mainPanel(div(id="stepTabCounts",dataTableOutput('clueResults')),
                                        width=10))),
                           tabPanel("CPDs", id='panel5',
                                    sidebarLayout(
                                      sidebarPanel(
                                        div(id="stepDownCPDs",
                                            downloadButton('downloadCPDs', 'Download'),
                                            actionButton('resetData', 'Clear', icon("trash-o")),
                                            p("You can download the spreadsheets or clear all results to start over.")),
                                        div(id="stepSeeResults",
                                            p("A visual representation of the CPDs for each marker can be seen on the next page:"),
                                            actionButton('continueResults', "See Results", icon("check-square"), 
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                        width = 2
                                      ),
                                      mainPanel(div(id="stepTabCPDs",dataTableOutput('markerResults')),
                                        width=10))),
                           tabPanel("Results", id='panel6',
                                    sidebarLayout(
                                      sidebarPanel(
                                        p("This page contains the final results of the clustering. Select controls in this experiment to see the difference for the targets."),
                                        div(id='stepControls',
                                        selectizeInput("control", "Select stable control(s):", NULL, multiple = T)),
                                        width = 2
                                        ),
                                      mainPanel(
                                        fluidRow(
                                          div(id='stepResults',
                                          plotlyOutput('finalViz'))
                                        ),
                                        width=10)))
                           
                           )))
