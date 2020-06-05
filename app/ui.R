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
                      
                      pageWithSidebar(
                        headerPanel(h3('Apply filters')),
                        sidebarPanel(width = 3,
                                     
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
                                     # apply filters to prepare data statistics
                                     actionButton("filterData",label = "Show Data"),
                                     actionButton("filterMissings",label = "Show missings")
                        ),
                        
                        mainPanel(width = 9,
                          br(),
                          tabsetPanel(type = "tabs",
                                      tabPanel("Variables", DT::dataTableOutput("dataOutput")),
                                      tabPanel("Missings", DT::dataTableOutput("missingsOutput"))
                          )
                          
                          # column(9,
                          #        hr(),
                          #        DT::dataTableOutput("dataOutput"),
                          # 
                          #        # plotlyOutput("plot1", width = 800, height=700),
                          #        # hr(),
                          #        # p("Some text at the bottom - maybe will be deleted.",
                          #        #   style = "font-size:15px")
                          #        )
                        )
                      )
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
                                     
                                     selectInput("groupingTypeInput",
                                                 label = "Type of grouping",
                                                 choices  = c("fixed", "sd", "equal", "pretty", "quantile", 
                                                              "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih"),
                                                 selected = "pretty"),
                                     
                                     
                                     fluidRow(
                                       column(4,
                                              checkboxInput("reverseColor",
                                                            label = "Reverse color",
                                                            value = FALSE,
                                                            # status = "success"
                                                            )
                                              ),
                                       column(4,
                                              checkboxInput("staticMap",
                                                            label = "Static Map",
                                                            value = FALSE
                                                            ),
                                              # radioButtons("staticMap",
                                              #              label = NULL,
                                              #              choices = list("Plot" = 1, "GGplot" = 0),
                                              #              # status = "success",
                                              #              selected = 0,
                                              #              inline= T
                                              #              )
                                              ),
                                       column(4,
                                              checkboxInput("ggplot",
                                                            label = "Static Map",
                                                            value = T
                                                            )
                                              )
                                       # column(4,
                                       # checkboxInput("staticMap", 
                                       #               label = "Static Map", 
                                       #               value = T)
                                       # ),
                                       # column(4,
                                       #        checkboxInput("staticMap", 
                                       #                      label = "Static ggplot", 
                                       #                      value = T)
                                       # )
                                     ),
                                     
                                     # checkboxInput("reverseColor", label = "Reverse colors", value = FALSE, width = NULL),
                                     # 
                                     # checkboxInput("staticMap", label = "Static Map", value = T, width = NULL),
                                     
    
                                     # additional controls for static map
                                     conditionalPanel(condition = "input.staticMap == true",
                                                      fluidRow(
                                                        column(6,
                                                               selectInput("staticLegendPlace",
                                                                           label = "Legend Place",
                                                                           choices  = c("bottomleft", "bottomright", 
                                                                                        "left", "topleft", 
                                                                                        "top","topright", 
                                                                                        "right", "center"),
                                                                           selected = "bottomleft"),
                                                        ),
                                                        column(6,
                                                               numericInput("staticLegendColumns",
                                                                            label = "Legend columns",
                                                                            value = 2,
                                                                            min = 1,
                                                                            max = 6,
                                                                            step = 1)
                                                        )
                                                      )#,
                                                      # selectInput("staticLegendPlace",
                                                      #             label = "Legend Place",
                                                      #             choices  = c("bottomleft", "bottomright", "left", "topleft", "top",
                                                      #                          "topright", "right", "center"),
                                                      #             selected = "bottomleft"),
                                                      # numericInput("staticLegendColumns",
                                                      #              label = "Legend columns",
                                                      #              value = 2,
                                                      #              min = 1,
                                                      #              max = 6,
                                                      #              step = 1)
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
                                       column(6,
                                              conditionalPanel(condition = "input.staticMap == false",
                                                               actionButton("filterAction2",label = "Interactive Map")),
                                              conditionalPanel(condition = "input.staticMap == true",
                                                               actionButton("filterAction3",label = "Static Map")),),
                                       column(6,
                                              myDownloadButton("downloadMap", "Download map"))
                                     ),

                                     
                                     
                        ),
                        
                        mainPanel(
                          column(12, align="center",
                                 conditionalPanel(
                                   condition = "input.staticMap == false",
                                   textOutput("staticMapOff"),
                                   br(),
                                   withSpinner(highchartOutput("interactiveMapOutput", height = "600px", width = "100%"))
                                   ),
                                 conditionalPanel(
                                   condition = "input.staticMap == true",
                                   textOutput("staticMapOn"),
                                   br(),
                                   withSpinner(plotOutput("staticMapOutput", height = "600px", width = "85%"))
                                   )
                                 , downloadButton("downloadMap", "Download map")
                                 )
                          )
                        # )
                      ),

             # -------------------------------------------------- tab 3, 4 -------------------------------------------------  
             
             tabPanel("Tab 3",
                      p("This is a place for some explonation", target="_blank"), ".",style = "font-size:25px"),
             
             tabPanel("Tab 4",
                      p(a("WNE", href="https://www.wne.uw.edu.pl/en/", target="_blank"),style = "font-size:25px")
                      )
             
  )  
    
    
)
)