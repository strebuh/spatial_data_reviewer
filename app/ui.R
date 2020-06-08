library(plotly)
library(shinythemes)
library(shinyWidgets)
library(highcharter)
library(shinycssloaders)

myDownloadButton <- function(outputId, label = "Download"){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, NULL, label)
}

shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }
    "))
  ),
  
  # upper darg green bar
  navbarPage("Poviat flat prices",
             
             # -------------------------------------------------- tab 1 -------------------------------------------------  
             tabPanel("Data",
                      fluidPage(theme = shinytheme("flatly")),
                      # tags$head(
                      #   tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                      
                      # pageWithSidebar(
                        # headerPanel(h3('Apply filters')),
                        sidebarPanel(width = 3,
                                     
                                     radioButtons("fileType", label = "Choose type of file to upload",
                                                  choices = list("RDS" = 0, "CSV" = 1), 
                                                  selected = 0,
                                                  inline= T
                                                  ),
                                     
                                     fileInput("dataFile", "Choose CSV/RDS File",
                                               multiple = FALSE,
                                               accept = c(".csv", ".rds", ".RDS")
                                               ),
                                     
                                     # choice of variable, based on variables in data
                                     uiOutput("whichYear"),
                                     
                                     # choice of variable, based on variables in data
                                     uiOutput("variableOutput"),
                                     
                                     # choice of voivodship
                                     uiOutput("areaOutput"),

                                     # choice of whether single year or time span
                                     radioButtons("periodType", label = "Period Span",
                                                  choices = list("Single Year" = 0, "Multiple Years" = 1), 
                                                  selected = 0),
                                     
                                     # based on period type choice
                                     conditionalPanel(condition="input.periodType == 0", 
                                                      uiOutput("yearOutput")),
                                     conditionalPanel(condition="input.periodType == 1", 
                                                      uiOutput("yearsOutput")),
                                     hr(),
                                     
                                     fluidRow(column(4, align="center",
                                                     actionButton("allData",label = "Data")),
                                              column(4, align="center",
                                                     actionButton("filterData",label = "Filtered")),
                                              column(4, align="left",
                                                     actionButton("filterMissings",label = "Missings"))
                                     )

                        ),
                        
                        mainPanel(width = 9,
                          br(),
                          tabsetPanel(type = "tabs",
                                      tabPanel("Uploaded", DT::dataTableOutput("contents")),
                                      tabPanel("Variables", DT::dataTableOutput("dataOutput")),
                                      tabPanel("Missings", DT::dataTableOutput("missingsOutput")
                                               )
                                      )
                          )
                        ), # tab sie konczy
             
             # -------------------------------------------------- tab 2 -------------------------------------------------  
             
             tabPanel("Map",
                      fluidPage(theme = shinytheme("flatly")),
                      # tags$head(
                      #   tags$style(HTML(".shiny-output-error-validation{color: red;}"))),

                      # pageWithSidebar(
                        # headerPanel(h3('Apply filters')),
                        sidebarPanel(width = 3,

                                     # choice of variable, based on variables in data
                                     uiOutput("variableOutput2"),

                                     uiOutput("yearOutput2"),

                                     uiOutput("titleOutput"),
                                     
                                     fluidRow(
                                       column(6, 
                                              uiOutput("paletteOutput")
                                       ),
                                       column(6,
                                              br(),
                                              checkboxInput("showPalettes",
                                                            label = "Show Palettes",
                                                            value = FALSE
                                              )
                                       )
                                     ),
                                     
                                     # uiOutput("paletteOutput"),

                                     selectInput("groupingTypeInput",
                                                 label = "Type of grouping",
                                                 choices  = c("fixed", "sd", "equal", "pretty", "quantile",
                                                              "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih"),
                                                 selected = "pretty"),


                                     fluidRow(
                                       column(6,
                                              checkboxInput("reverseColor",
                                                            label = "Reverse color",
                                                            value = FALSE
                                                            # status = "success"
                                                            )
                                              ),
                                       column(6,
                                              checkboxInput("staticMap",
                                                            label = "Static Map",
                                                            value = F
                                                            )
                                              )
                                       ),


                                     # additional controls for static map, sizes of title and legend
                                     conditionalPanel(condition = "input.staticMap == true",
                                                      fluidRow(
                                                        column(4,
                                                               numericInput("titleSize",
                                                                           label = "Title",
                                                                           value = 15,
                                                                           min = 8,
                                                                           max = 20,
                                                                           step = 1,
                                                                           width = "100%")
                                                        ),
                                                        column(4,
                                                               numericInput("legendTitleSize",
                                                                            label = "Legend",
                                                                            value = 13,
                                                                            min = 8,
                                                                            max = 20,
                                                                            step = 1,
                                                                            width = "100%")
                                                        ),
                                                        column(4,
                                                               numericInput("legendLabelSize",
                                                                            label = "Labels",
                                                                            value = 12,
                                                                            min = 8,
                                                                            max = 20,
                                                                            step = 1,
                                                                            width = "100%"
                                                                            )
                                                               )
                                                        )
                                                      ),


                                     # type of bucketing
                                     radioButtons("bucketingTypeInput", label = "Bucketing mode",
                                                  choices = list("Automatic" = 0, "Manual" = 1),
                                                  selected = 0,
                                                  inline= T),

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
                                             # column(12, align="center",
                                             conditionalPanel(
                                               condition = "input.staticMap == false",
                                               # textOutput("staticMapOff"),
                                               br(),
                                               withSpinner(highchartOutput("interactiveMapOutput", height = "600px", width = "100%"))
                                             ),
                                             conditionalPanel(
                                               condition = "input.staticMap == true",
                                               # textOutput("staticMapOn"),
                                               br(),
                                               withSpinner(plotOutput("staticMapOutput", height = "500px", width = "70%"))
                                             )
                                             # ) # column end
                                    ), # tabPanel end
                                    tabPanel("Palettes colors",
                                             conditionalPanel(condition = "input.showPalettes == true", 
                                                              plotOutput("palettes", height = "800px")
                                             )
                                    ) # tabPanel
                        ) # tabsetPanel
                      ) # mainPanel
                      ), # tab sie konczy
             
             # -------------------------------------------------- tab 3 -------------------------------------------------  
             
             tabPanel("Analyses",
                      
                      fluidPage(theme = shinytheme("flatly")),
                      
                      # pageWithSidebar(
                        
                        # headerPanel(h3('Apply filters')),
                        
                        sidebarPanel(
                          uiOutput("year"),

                          uiOutput("var"),
                          
                          uiOutput("var2"),
                          
                          sliderInput("bars", "No of bars in histogram:", min = 2, max = 100, value = 20, step = 1),
                          textInput("x_lower", label = "X-axis limit in histogram (lower):", value = ''),
                          textInput("x_upper", label = "X-asix limit in histogram (upper):", value = ''),
                          actionButton("getAnalysis", label = "Gen Analysis")
                        ),
                        
                        mainPanel(
                          withSpinner(plotOutput("plots"))
                        )
                      # )
             ), # tab sie konczy
             
             # , p("This is a place for some explonation", target="_blank"), ".",style = "font-size:25px"),
             
             
             # -------------------------------------------------- tab 4 -------------------------------------------------  
             
             tabPanel("Model",
                      
                      fluidPage(theme = shinytheme("flatly")),
                      
                      # pageWithSidebar(
                        
                        # headerPanel(h3('Apply filters')),
                        
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
                                     
                                     uiOutput("DependentVariable"),
                                     
                                     #radioButtons("Stepwise", label = 'Choosing variables',
                                     #choices = list("automatic" = 0, "manual" = 1), 
                                     #selected = 1),
                                     
                                     #conditionalPanel(condition="input.Stepwise == 0",
                                     #                   selectInput("IndependentVariables", 
                                     #                               label = "Independent variables",
                                     #                              choices = colnames(data),
                                     #                              selected = 'kob_w_bezrob',
                                     #                               multiple = TRUE)
                                     #                  ),
                                     #conditionalPanel(condition="input.Stepwise == 1", 
                                     
                                     uiOutput("IndependentVariables"),
                                     
                                     actionButton("fitModel", label = "Fit Model")
                                     
                                     # )
                        ),
                        
                        mainPanel(
                          withSpinner(verbatimTextOutput("recommendation")),
                          br(),
                          withSpinner(verbatimTextOutput("evaluation"))
                        )
                      # )
                      
             ) # tab sie konczy
  )))

