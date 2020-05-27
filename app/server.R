# load packages here
library(shiny)
library(plotly)

getwd()
data <- readRDS("../data/data06_18_contig_na_fill.RDS") # fread fater but could not deal with long file names


dim(data)
names(data)
head(data)

shinyServer(function(input, output){
  
  
  # choose variable
  output$variableOutput <- renderUI({
    selectInput("variableInput", 
                label="Variable",
                choices = names(data)[5:length(names(data))-1])
  }) 
  
  # choose single year 
  output$yearOutput <- renderUI({
    selectInput("inputYear", 
                label="Year",
                choices = unique(data$rok))
  }) 
  
  # choose range of years
  output$yearsOutput <- renderUI({
    sliderInput("inputYears", 
                label = "Years",
                step = 1,
                round=TRUE,
                sep="",
                min = min(data$rok), max = max(data$rok),
                value = c(2017, 2018))
  }) 
  
  
})