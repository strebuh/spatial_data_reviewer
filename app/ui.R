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
                                     
                                     # # choice of whether single year or time span
                                     # radioButtons("periodType", label = h2("Period Span"),
                                     #              choices = list("Single Yaar" = 0, "Multiple Years" = 1), 
                                     #              selected = 0),
                                     
                                     # based ib period type wither a single year or span
                                     uiOutput("yearOutput2"),
                                     # title of the plot
                                     uiOutput("titleOutput"),
                                     uiOutput("paletteOutput"),
                                     uiOutput("groupingTypeOutput"),
                                     uiOutput("ngroupsOutput"),
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