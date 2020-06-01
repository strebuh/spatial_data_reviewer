# load packages here
library(shiny)
library(plotly)
library(htmlwidgets)
library(highcharter)
library(bsplus)

source("../scripts/get_interactive_map.R")

getwd()
data <- readRDS("../data/data06_18_contig_na_fill.RDS") # fread fater but could not deal with long file names
# data <- readRDS("data/data06_18_contig_na_fill.RDS") # fread fater but could not deal with long file names
# names(data)[1:5]
# class(data$cz_bud_m_jednorodz_mc)


# zmien nazwe zmiennej teryt
names(data)[which(names(data) == "teryt")] = "jpt_kod_je" # ty wlaczyc wybieranie ktora zmienna jest wspolna

# data <- readRDS("./data/data06_18_contig_na_fill.RDS") # fread fater but could not deal with long file names
# which(names(data) == "teryt")
# # zmien nazwe zmiennej teryt
# names(data)[which(names(data) == "teryt")] = "jpt_kod_je" # ty wlaczyc wybieranie ktora zmienna jest wspolna
# # do not show anyting at the beggining, otherwise error
# dane <- data[data$rok == input$inputYear2, 
#              c(names(data)[3], "jpt_kod_je", input$variableInput2)]

pov_json_list <- readRDS("../data/poviaty_json_list.RDS")
# pov_json <- geojsonio::as.json("../data/powiaty/pow.json")

shinyServer(function(input, output){
  
  # -------------------------------------------------- sidebar panel inputs -------------------------------------------------
  
  # -------------------------------------------------- tab 1 -------------------------------------------------  
  
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
  
  # -------------------------------------------------- tab 2 -------------------------------------------------  
  
  # MAP tab outputs
  observe({
    # choose variable
    output$variableOutput2 <- renderUI({
      selectInput("variableInput2", 
                  label="Variable",
                  choices = names(data)[5:length(names(data))-1])
    })
    
    # choose single year 
    output$yearOutput2 <- renderUI({
      selectInput("inputYear2", 
                  label="Year",
                  selected = 2018,
                  choices = unique(data$rok))
      }) 
    
    # choose title
    output$titleOutput <- renderUI({
      textInput("inputTitle", 
                label="Title",
                value = paste("Plot of", input$variableInput2 ,"in", input$inputYear2)
                )
      }) 
    
    # choose palette
    output$paletteOutput <- renderUI({
      selectInput("inputPalette",
                  label="Palette",
                  selected = "BuPu",
                  choices = row.names(brewer.pal.info))
      }) 
      
    # # pick rule of bucketing
    # output$groupingTypeOutput <- renderUI({
    #   radioButtons("groupingTypeInput",
    #                label = "Type of grouping",
    #                choices  = c( "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih"),
    #                inline= T,
    #                selected = "sd")
    #   })

    # select seed to bclust or kmeans
    output$seedOutput <- renderUI({
      textInput("seedInput",
                label = paste0(input$groupingTypeInput," seed"),
                value = 1)
      })
     
    # created text inputs for breaks 
    output$fixedBreaksTexts <- renderUI({
    #   n_breaks <- as.integer(input$ngroupsInput) # nie wiem czy tu nie blad, pokazuje bez min i max teraz, tylsko srodkowe wartosci
    # lapply(2:n_breaks, function(i) {
    #   # if(i==1){ # dodac min i max aktualnej zmiennej
    #     textInput(paste0("break_",i),
    #               label = paste0("Break ",i),
    #               width = "25%"
    #               )
    #   })
      
      variable <- data[data$rok == input$inputYear2, input$variableInput2]
      min_ <- min(variable)
      max_ <- max(variable)
      intermediate <- paste(rep("_", as.integer(input$ngroupsInput)-1), collapse = "  ")

      textInput("breaksInput",
                label = paste0("Breaks"),
                value = paste(min_, intermediate, max_, sep = "  ")) %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Replace underscores with break values separated by space")
        )
      })
    })
    
    # # extract breaks
    fixedBreaks <- reactive({
          # separate text inputs
      print(input$breaksInput)
          breaks_vec <- as.numeric(unlist(stringr::str_split(input$breaksInput, pattern = "\\s+")))
          # n_breaks <- as.integer(input$ngroupsInput)
          # breaks <- unlist(sapply(2:n_breaks-1, function(i) {
          #   as.numeric(input[[paste0("break_", i)]])
          })


  # -------------------------------------------------- outoputs -------------------------------------------------
  
  # -------------------------------------------------- tab 1 -------------------------------------------------  
  
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

    # -------------------------------------------------- tab 2 -------------------------------------------------
    
    # get data for table and plot
    mapka <- reactive({
      if(is.null(input$variableInput2)) {
        return(NULL)
      } else if(0){}
      else {
        # do not show anyting at the beggining, otherwise error
        dane <- data[data$rok == input$inputYear2, 
                       c(names(data)[2:3], "jpt_kod_je", input$variableInput2)] # input$variableInput2
          # }
        
        # set appropiate number of groups of NULL if Automatic bucketing selected
        if(input$bucketingTypeInput == 1){
          ngroupsInput <- input$ngroupsInput
        } else {
          ngroupsInput <- NULL
        }
        
        # # # tu nie dziala
        # fixedBreaks <- stringr::str_split(input$breaksInput, "\\s+")
        
        get_interactive_map(
          dane,                                   # ramka z danymi wg gmin (hospitalizacje itp)
          pov_json_list,                                   # obiekt mapy - json
          # mapline_json_list = NULL,
          zmienna_mapowana = 4,                        # index zmiennej do mapowania 
          joining_var = "jpt_kod_je",
          ilosc_grup = ngroupsInput,                               # na ile pobucketowac
          # lista_winietek = lista_winietek[c(1)],              # zmienne na hover klucz lista[nazwa_zmiennej] <- "teskt na hover"
          tytul = input$inputTitle,                                # tytul do wykresu 
          etykiety_obszarow = FALSE,                    # czy pokazac nazwy obszarow
          # kolor_granic_map = "",                        # kolor granic obszarow podstawowych (map)
          # kolor_granic_mapline = "black",               # kolor granic obszarow dodatkowych (mapline)
          bucketing_seed = input$seedInput,
          tryb_podzialu = input$groupingTypeInput,                     # hclust, kmeans, sd
          breaks = fixedBreaks(),
          paleta_kolorow = input$inputPalette,                        # nazwa palety do mapowania
          # zmienna_punkty = NULL,                     # bare name od zmiennej z liczba oddzialów
          nazwa_oddzialow = "zmienna"    # etykieta
        )
      }
    })
    
    output$mapOutput <- renderHighchart({
      input$filterAction2
      isolate({mapka()})
    })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadMap <- downloadHandler(
      filename = function() {
        paste0(input$variableInput2, "_", input$inputYear2 ,".html")
      },
      content = function(file) {
        # write.csv(datasetInput(), file, row.names = FALSE)
        saveWidget(mapka(), file)
      }
    )
  })

    
    # downloading
    # https://shiny.rstudio.com/articles/download.html
    # sipenner
    # https://rdrr.io/cran/shinyWidgets/man/addSpinner.html
    # https://datascienceplus.com/making-a-shiny-dashboard-using-highcharter-analyzing-inflation-rates/
    # https://cran.r-project.org/web/packages/rmapshaper/vignettes/rmapshaper.html





