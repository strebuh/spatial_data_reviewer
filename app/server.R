# load packages here
library(shiny)
library(plotly)

getwd()
data <- readRDS("../data/data06_18_contig_na_fill.RDS") # fread fater but could not deal with long file names


shinyServer(function(input, output){
  
  # observe the filters
  observe({
    # choose variable
    output$variableOutput <- renderUI({
      selectInput("variableInput", 
                  label="Variable",
                  choices = names(data)[5:length(names(data))-1])
    })
    
    # choose area
    output$areaOutput <- renderUI({
      selectInput("areaInput",
                  label="Area",
                  choices = c("Poland", unique(data$Nazwa)))
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
  
  # get data for table and plot
    filtered <- reactive({
      # do not show anyting at the beggining, otherwise error
      if(is.null(input$areaInput)) {
        return(NULL)
      } else if(input$periodType == 0){
        # if on single year
        data[data$rok == input$inputYear &
               data$Nazwa %in% if(input$areaInput=="Poland") unique(data$Nazwa) else input$areaInput, 
             c(names(data)[1:3],input$variableInput)]
      } else {
        # if range of years
        data[data$rok %in% input$inputYears[1]:input$inputYears[2] &
               data$Nazwa %in% if(input$areaInput=="Poland") unique(data$Nazwa) else input$areaInput, 
             c(names(data)[1:3],input$variableInput)]
        }

    })
    
    output$dataOutput <- DT::renderDataTable({
      input$filterAction
      isolate({filtered()
      })
    })
})



