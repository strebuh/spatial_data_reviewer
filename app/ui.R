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
                                     uiOutput("variableOutput"),
                                     
                                     # choice of voivodship
                                     uiOutput("areaOutput"),

                                     # choice of whether single year or time span
                                     radioButtons("periodType", label = "Period Span",
                                                  choices = list("Single Yaar" = 0, "Multiple Years" = 1), 
                                                  selected = 0),
                                     
                                     # based ib period type wither a single year or span
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
                                     ),
                        ),
                        
                        mainPanel(width = 9,
                          br(),
                          tabsetPanel(type = "tabs",
                                      tabPanel("Uploaded", DT::dataTableOutput("contents")),
                                      tabPanel("Variables", DT::dataTableOutput("dataOutput")),
                                      tabPanel("Missings", DT::dataTableOutput("missingsOutput"))
                          )
                        )
                      # )
                      ),
             
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
                                     
                                     uiOutput("paletteOutput"),
                                     
                                     checkboxInput("reverseColor", label = "Reverse colors", value = FALSE, width = NULL),
                                     
                                     selectInput("groupingTypeInput",
                                                  label = "Type of grouping",
                                                  choices  = c("fixed", "sd", "equal", "pretty", "quantile", 
                                                               "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih"),
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
                                     actionButton("filterAction2",label = "Prepare"),
                                     myDownloadButton("downloadMap", "Download")
                                     
                        ),
                        
                        mainPanel(
                          column(12,
                                 withSpinner(highchartOutput("mapOutput", height = "600px", width = "100%")),
                                 br(),
                          )
                        )
                      # )
                      ),
             
             # -------------------------------------------------- tab 3 -------------------------------------------------  
             
             tabPanel("Analyses"), 
             
             # -------------------------------------------------- tab 4 -------------------------------------------------  
             
             tabPanel("Model") 
             
             
             )
  )
  )
