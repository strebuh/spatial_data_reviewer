library(plotly)
library(shinythemes)
library(shinyWidgets)
library(highcharter)


shinyUI(fluidPage(
  # upper darg green bar
  navbarPage("Poviat flat prices",
             # tabs
             tabPanel("Data",
                      fluidPage(theme = shinytheme("flatly")),
                      # tags$head(
                      #   tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                      
                      pageWithSidebar(
                        headerPanel(h3('Apply filters')),
                        sidebarPanel(width = 3,
                                     
                                     # choice of variable, based on variables in data
                                     uiOutput("variableOutput"),
                                     
                                     # choice of voivodship
                                     uiOutput("areaOutput"),

                                     # choice of whether single year or time span
                                     radioButtons("periodType", label = h2("Period Span"),
                                                  choices = list("Single Yaar" = 0, "Multiple Years" = 1), 
                                                  selected = 0),
                                     
                                     # based ib period type wither a single year or span
                                     conditionalPanel(condition="input.periodType == 0", 
                                                      uiOutput("yearOutput")),
                                     conditionalPanel(condition="input.periodType == 1", 
                                                      uiOutput("yearsOutput")),
                                     hr(),
                                     # apply filters to prepare data statistics
                                     actionButton("filterAction",label = "Update filters")
                        ),
                        
                        mainPanel(
                          column(9,
                                 hr(),
                                 DT::dataTableOutput("dataOutput"),
 
                                 # plotlyOutput("plot1", width = 800, height=700),
                                 # hr(),
                                 # p("Some text at the bottom - maybe will be deleted.",
                                 #   style = "font-size:15px")
                                 
                                 
                          )
                        )
                      )
                      ),
             
             tabPanel("Map",
                      fluidPage(theme = shinytheme("flatly")),
                      # tags$head(
                      #   tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                      
                      pageWithSidebar(
                        headerPanel(h3('Apply filters')),
                        sidebarPanel(width = 3,
                                     
                                     # choice of variable, based on variables in data
                                     uiOutput("variableOutput2"),
                                     
                                     uiOutput("yearOutput2"),
                                     
                                     uiOutput("titleOutput"),
                                     
                                     uiOutput("paletteOutput"),
                                     
                                     # uiOutput("groupingTypeOutput"),
                                     radioButtons("groupingTypeInput",
                                                  label = "Type of grouping",
                                                  choices  = c( "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih"),
                                                  inline= T,
                                                  selected = "pretty"),
                                     
                                     # type of bucketing
                                     radioButtons("bucketingTypeInput", label = "Bucketing mode",
                                                  choices = list("Automatic" = 0, "Manual" = 1), 
                                                  selected = 0,
                                                  inline= T),
                                     
                                     # if kmeans or bclust enter seed | if not for name of kmean of bclust seet input could be here
                                     conditionalPanel(condition ="input.groupingTypeInput == 'kmeans' | input.groupingTypeInput == 'bclust'",
                                                      uiOutput("seedOutput")
                                                      ),
                                     
                                     # # if fixed
                                     # conditionalPanel(condition ="input.groupingTypeInput == 'fixed'",
                                     #                  # input here
                                     # ),
                                     
                                     # # if fixed and automatic
                                     # conditionalPanel(condition ="input.bucketingTypeInput == 0 & input.groupingTypeInput == 'fixed'", 
                                     #                  uiOutput("fixedBucketMessageOutput")
                                     # ),
                                     
                                     # # if fixed and manual
                                     # conditionalPanel(condition ="input.bucketingTypeInput == 1 & input.groupingTypeInput == 'fixed'",
                                     #                  uiOutput("ngroupsOutput")
                                     #                  # uiOutput("fixedBucketOutput"),
                                     # ),

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
                                     # apply filters to prepare data statistics
                                     actionButton("filterAction2",label = "Show map")
                        ),
                        
                        mainPanel(
                          column(9,
                                 addSpinner(highchartOutput("mapOutput"), spin = "rotating-plane", color = "#984EA3"),
                                 br(),
                                 downloadButton("downloadMap", "Download map")
                                 
                                 
                          )
                        )
                      )
             ),
             
             tabPanel("Map",
                      p("This is a place for some explonation", target="_blank"), ".",style = "font-size:25px"),
             
             tabPanel("Other part ",
                      p(a("WNE", href="https://www.wne.uw.edu.pl/en/", target="_blank"),style = "font-size:25px")
                      )
             
  )  
    
    
)
)