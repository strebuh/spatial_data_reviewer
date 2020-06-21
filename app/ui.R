library(shinythemes)
library(highcharter)
library(shinycssloaders)

myDownloadButton <- function(outputId, label = "Download"){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, NULL, label)
}

shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("
      .shiny-notification {
        position:fixed;
        top: calc(50%);
        left: calc(35%);
        font-size: 25px;
        color: #2E695C;
        background-color: #8CDAAE;
      }
      .shiny-output-error-validation {
        margin-top: 25px;
        margin-left: 50px;
        font-size: 25px;
        color: #449E8A;
        font-weight: bold;
      }
        #recommendation { 
        font-size: 20px;
        font-weight: bold;
        color: #40534F;
      }
        #evaluation { 
        font-size: 16px;
        font-style: italic;
      }
    "))
    ),
  
  # upper darg green bar
  navbarPage("Spatial Data Reviewer",
             
             # -------------------------------------------------- tab 1 -------------------------------------------------  
             tabPanel("Data",
                      fluidPage(theme = shinytheme("flatly")),

                        sidebarPanel(width = 3,
                                     
                                     # conditionalPanel(condition = "input.inputType == 0",)
                                     radioButtons("fileType", label = "Choose type of file to upload",
                                                  choices = list("RDS" = 0, "CSV" = 1), 
                                                  selected = 0,
                                                  inline= T
                                                  ),
                                     
                                     conditionalPanel(condition = "input.fileType == 1",
                                                      fluidRow(
                                                        column(3, 
                                                               # Input: Select separator ----
                                                               radioButtons("sep", "Sep",
                                                                            choices = c(Comma = ",",
                                                                                        Semicolon = ";",
                                                                                        Tab = "\t"),
                                                                            selected = ",")
                                                               ),
                                                        column(3, 
                                                               textInput("customSep", "Custom Sep")
                                                        ),
                                                        column(3,
                                                               radioButtons("dec", "Dec",
                                                                            choices = c(Dot = '.',
                                                                                        Comma = ","),
                                                                            selected = '.')
                                                               ),
                                                        column(3,
                                                               checkboxInput("utf8", 
                                                                             "UTF-8",
                                                                             value = FALSE)
                                                               )
                                                        )
                                                      ),
                                              
                                     fileInput("dataFile", "Choose CSV/RDS File",
                                               multiple = FALSE,
                                               accept = c(".csv", ".rds", ".RDS")
                                               ),
                                     
                                     # choice of variable, based on variables in data
                                     uiOutput("whichYear"),
                                     
                                     # choice of column which represents name of spatial unit
                                     uiOutput("whichName"),

                                     # choice of column which represents ID of spatial unit, in common with spation file
                                     uiOutput("whichSpID"),
                                     
                                     # choice of variable, based on variables in data
                                     uiOutput("variableOutput"),
                                     
                                     # choice of voivodship
                                     uiOutput("areaOutput"),

                                     uiOutput("yearsOutput"),
                                     
                                     hr(),
                                     
                                     fluidRow(
                                              column(4, align="center",
                                                     actionButton("filterData",label = "Filtered")),
                                              column(4, align="left",
                                                     actionButton("filterMissings",label = "Missings"))
                                     )

                        ),
                        
                        mainPanel(width = 9,
                          br(),
                          tabsetPanel(type = "tabs",
                                      tabPanel("Variables", DT::dataTableOutput("dataOutput")),  # DT::
                                      tabPanel("Missings", DT::dataTableOutput("missingsOutput") # DT::
                                               )
                                      ) # tabset panel
                          ) # main panel
                        ), # tab end
             
             # -------------------------------------------------- tab 2 -------------------------------------------------  
             
             tabPanel("Map",
                      fluidPage(theme = shinytheme("flatly")),

                        sidebarPanel(width = 3,
                                     
                                     fileInput("shapeFile", "Choose shp files (.shp, .prj, .shx, .dbf)",
                                               multiple = TRUE,
                                               accept = c(".shp", ".dbs", ".shx", ".prj", ".dbf")
                                               ),
                                     
                                     # choice of column which represents ID of spatial unit, in common with spation file
                                     uiOutput("whichShpID"),

                                     # choice of variable, based on variables in data
                                     uiOutput("variableOutput2"),

                                     uiOutput("yearOutput2"),

                                     uiOutput("titleOutput"),
                                     
                                     fluidRow(
                                       column(3, 
                                              uiOutput("paletteOutput")
                                       ),
                                       column(3,
                                              br(),
                                              checkboxInput("showPalettes",
                                                            label = "Show Palettes",
                                                            value = FALSE
                                                            )
                                              ),
                                       column(3,
                                              br(),
                                              checkboxInput("reverseColor",
                                                            label = "Reverse color",
                                                            value = FALSE
                                                            )
                                              ),
                                       column(3,
                                              br(),
                                              uiOutput("staticMap")
                                       )
                                     ),
                                     
                                     uiOutput("groupingType"),

                                     # additional controls for static map, sizes of title and legend
                                     conditionalPanel(condition = "input.staticMap == true",
                                                      fluidRow(
                                                        column(4,
                                                               uiOutput("titleSize")
                                                               ),
                                                        column(4,
                                                               uiOutput("legendTitleSize")
                                                               ),
                                                        column(4,
                                                               uiOutput("legendLabelSize")
                                                               )
                                                        )
                                                      ),


                                     # type of bucketing
                                     uiOutput("bucketingType"),

                                     # if kmeans or bclust enter seed | if not for name of kmean of bclust seet input could be here
                                     conditionalPanel(condition ="input.groupingTypeInput == 'kmeans' | input.groupingTypeInput == 'bclust'",
                                                      uiOutput("seedOutput")
                                                      ),

                                     # if manual, choose number of groups, type, and
                                     conditionalPanel(condition ="input.bucketingTypeInput == 1",
                                                      sliderInput("ngroupsInput",
                                                                  label = "Number of groups",
                                                                  step = 1,
                                                                  round=TRUE,
                                                                  sep="",
                                                                  min = 2,
                                                                  max = 12,
                                                                  value = 4),# ),
                                                      # nested panel for producing fixed breaks
                                                      conditionalPanel(condition ="input.groupingTypeInput == 'fixed'",
                                                                       uiOutput("fixedBreaksTexts")
                                                                       )
                                                      ),

                                     hr(),
                                     fluidRow(
                                       column(6, align="center",
                                              conditionalPanel(condition = "input.staticMap == false",
                                                               actionButton("filterAction2",label = "Prepare")),
                                              conditionalPanel(condition = "input.staticMap == true",
                                                               actionButton("filterAction3",label = "Prepare"))
                                              ),
                                       column(6, align="center",
                                              conditionalPanel(condition = "input.staticMap == false",
                                                               myDownloadButton("downloadInteractMap", "Download")
                                                               ),
                                              conditionalPanel(condition = "input.staticMap == true",
                                                               myDownloadButton("downloadggplotMap", "Download")
                                              )
                                              )
                                       )
                                     ),
                      
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Maps", 
                                             column(12, align="center",
                                             conditionalPanel(
                                               condition = "input.staticMap == false",
                                               br(),
                                               withSpinner(highchartOutput("interactiveMapOutput", height = "730px", width = "100%"))
                                             ),
                                             conditionalPanel(
                                               condition = "input.staticMap == true",
                                               br(),
                                               withSpinner(plotOutput("staticMapOutput", height = "670px", width = "70%"))
                                             )
                                             ) # column end
                                    ), # tabPanel end
                                    tabPanel("Palettes colors",
                                             conditionalPanel(condition = "input.showPalettes == true", 
                                                              plotOutput("palettes", height = "800px")
                                             )
                                    ) # tabPanel
                        ) # tabsetPanel
                      ) # mainPanel
                      ), #  tab end
             
             # -------------------------------------------------- tab 3 -------------------------------------------------  
             
             tabPanel("Analyses",
                      
                      fluidPage(theme = shinytheme("flatly")),
                      
                        sidebarPanel(
                          uiOutput("year"),
                          
                          uiOutput("ExeptVar3"),

                          uiOutput("var"),
                          
                          uiOutput("var2"),
                          
                          sliderInput("bars", "No of bars in histogram:", min = 2, max = 100, value = 20, step = 1),
                          textInput("x_lower", label = "X-axis limit in histogram (lower):", value = ''),
                          textInput("x_upper", label = "X-asix limit in histogram (upper):", value = ''),
                          actionButton("getAnalysis", label = "Get Analysis")
                        ),
                        
                        mainPanel(
                          withSpinner(plotOutput("plots"))
                        ) # mainPanel
             ), # tab end
             
             # -------------------------------------------------- tab 4 -------------------------------------------------  
             
             tabPanel("Model",
                      
                      fluidPage(theme = shinytheme("flatly")),
                        
                        sidebarPanel(width = 3,
                                     selectInput("ChosenModel", 
                                                 label = "Model",
                                                 choices = c(
                                                   'ols','manski','sac','sdem','sem','sdm','sar','slx'
                                                 ),
                                                 selected = 'ols'),
                                     
                                     uiOutput("ChosenYear"),
                                     
                                     selectInput("Distance", 
                                                 label = "Formula type",
                                                 choices = c('default: y~x',
                                                             'multinomial: y~x+x^2+x^3+x^4',
                                                             'power: log(y)~log(x)',
                                                             'exponential: log(y)~x'),
                                                 selected = 'default: y~x'),
                                     
                                     uiOutput("ExeptVar4"),
                                     
                                     uiOutput("DependentVariable"),
                                     
                                     uiOutput("IndependentVariables"),
                                     
                                     actionButton("fitModel", label = "Fit Model")
                                     
                        ),
                        
                        mainPanel(
                          withSpinner(verbatimTextOutput("recommendation")),
                          br(),
                          withSpinner(verbatimTextOutput("evaluation"))
                        )
                      ) #  tab end
             ) # navbarPage
  ) # fluidPage
  ) # shinyUI

