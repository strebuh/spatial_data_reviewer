# load packages here
library(shiny)
library(plotly)
library(htmlwidgets)
library(highcharter)
library(bsplus)
library(backports)
library(shinycssloaders)
library(rgdal)

source("../scripts/PACKAGES.R")
source("../scripts/get_static_map.R")
source("../scripts/get_interactive_map.R")
source("../scripts/get_ggplot_map.R")


# input$dataFile
data <- NULL
pov_json_list <- NULL

# pov_json_list <- readRDS("../data/poviaty_json_list.RDS")

options(shiny.maxRequestSize=30*1024^2)


pov_sp <- NULL

shinyServer(function(input, output){

  data <- reactive({
    
    if(is.null(data)){
      data <- readRDS("../data/data06_18_contig_na_fill.RDS") 
    } else {
      data <- upload()
    }
    
    # zmien nazwe zmiennej teryt, # tu wlaczyc wybieranie ktora zmienna jest wspolna
    names(data)[which(names(data) == "teryt")] = "jpt_kod_je"
    
    if(is.data.table(data) | tibble::is_tibble(data)){
      tryCatch({
        data <- as.data.frame(data)
      }, error = function(e) {
        showNotification("A file must readable as data.frame or data.table!",
                         type="error",
                         duration = 7)
        return(NULL)
      })
    }

    return(data)
    })
  
  # -------------------------------------------------- sidebar panel inputs -------------------------------------------------
  
  # -------------------------------------------------- tab 1 -------------------------------------------------  
  
  # choose variable
  output$variableOutput <- renderUI({
    
    choices <- names(data())[5:length(names(data()))-1]

    selectInput("variableInput", 
                label="Variables",
                choices = choices,
                multiple = TRUE,
                selected = sample(choices, 3)
    )
  })
  
  # choose area
  output$areaOutput <- renderUI({
    selectInput("areaInput",
                label="Area",
                choices = c("Poland", unique(data()$Nazwa)))

  })
  
  # choose single year 
  output$yearOutput <- renderUI({
    selectInput("inputYear", 
                label="Year",
                choices = unique(data()$rok),

                selected = 2018)
  }) 
  
  # choose range of years
  output$yearsOutput <- renderUI({
    sliderInput("inputYears", 
                label = "Years",
                step = 1,
                round=TRUE,
                sep="",
                min = min(data()$rok), max = max(data()$rok),
                value = c(2016, 2018))
  }) 
  
  
  
  # -------------------------------------------------- tab 2 -------------------------------------------------  
  
  # MAP tab outputs
  
  ## in observe we put code that is dependend on reactive variables needs to be reevaluated when ractive variable (input) changes
  ## not sure if thing that data needs to be inside <- try it
  ## reactive is similiar but let get a returned value, so uses reactive variables and it's reealuation is triggered be their changes
  ## and also returns a values
  
  
  # choose variable <- doesnt depend on input variable, so not in observe
  output$variableOutput2 <- renderUI({
    selectInput("variableInput2", 
                label="Variable",
                choices = names(data())[5:length(names(data()))-1])

  })
  
  # choose single year <- doesnt depend on input variable, so not in observe
  output$yearOutput2 <- renderUI({
    selectInput("inputYear2", 
                label="Year",
                selected = 2018,
                choices = unique(data()$rok))

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
  
  # choose palette
  output$paletteOutput <- renderUI({
    selectInput("inputPalette",
                label="Palette",
                selected = "BuPu",
                choices = row.names(brewer.pal.info))
  }) 
  
  
  # select seed to bclust or kmeans
  output$seedOutput <- renderUI({
    textInput("seedInput",
              label = paste0(input$groupingTypeInput," seed"),
              value = 1)
  })
  
  
  # created text inputs for breaks, based on ranges of selected variable 
  output$fixedBreaksTexts <- renderUI({
    
    variable <- data()[data()$rok == input$inputYear2, input$variableInput2]

    min_ <- min(variable)
    max_ <- max(variable)
    intermediate <- paste(rep("_", as.integer(input$ngroupsInput)-1), collapse = "  ")
    
    textInput("breaksInput",
              label = paste0("Breaks"),
              value = paste(min_, intermediate, max_, sep = "  ")) %>%
      shinyInput_label_embed(
        # add information icon with instruction
        icon("info") %>%
          bs_embed_tooltip(title = "Replace underscores with break values separated by space")
      )
  })
  
  # -------------------------------------------------- tab 3 -------------------------------------------------  
  
  output$year <- renderUI({
    selectInput("year", 
                label = "Choose a year",
                choices = unique(data()$rok), # c(min(data()$rok):max(data()$rok))
                selected = 2018)
    })
  
  output$var <- renderUI({selectInput("var", 
                                      label = "Choose a variable",
                                      choices = colnames(data()),
                                      selected = 'kob_w_bezrob')})
  
  output$var2 <- renderUI({selectInput("var2",
                                       label = "Choose another variable for scatterplot",
                                       choices = colnames(data()),
                                       selected = 'bezrob_proc')})

  
  
  
  # -------------------------------------------------- tab 4 -------------------------------------------------  
  
  output$ChosenYear <- renderUI({selectInput("ChosenYear", 
                                             label = "Year",
                                             choices = unique(data()$rok),
                                             selected = 2018)})
  output$DependentVariable <- renderUI({selectInput("DependentVariable", 
                                                    label = "Dependent variable",
                                                    choices = colnames(data()),
                                                    selected = 'kob_w_bezrob')})
  output$IndependentVariables <- renderUI({selectInput("IndependentVariables", 
                                                       label = "Independent variables",
                                                       choices = colnames(data()),
                                                       selected = "bezrob_proc",
                                                       multiple = TRUE)})

  
  
  # -------------------------------------------------- functions and outoputs -------------------------------------------------
  
  # -------------------------------------------------- tab 1 -------------------------------------------------  
  
  # file upload
  upload <- reactive({
    
    req(input$dataFile)
    
    if((input$fileType != 0 & !grepl("(.rds)$|(.RDS)$", input$dataFile$datapath))|
       (input$fileType != 1 & grepl("(.csv)$|(.CSV)$", input$dataFile$datapath))
    ){
      
      showNotification("File format doesn't match the choice!",
                       type="error",
                       duration = 7)
      return(NULL)
    }
    
    if(input$fileType == 0){
      
      df <- readRDS(input$dataFile$datapath)
      
    } else if(input$fileType == 1){
      
      df <- read.csv(input$dataFile$datapath,
                     # header = input$header,
                     # sep = input$sep,
                     # quote = input$quote,
                     encoding = "UTF-8",
                     stringsAsFactors = F)
    }
    return(df)
  })
  
  # get data for table and plot
  filtered <- reactive({
    
    # # do not show anyting at the beggining, otherwise error
    req(input$variableInput)
    
    if(input$periodType == 0){
      # if on single year
      fitered_data1 <- data()[data()$rok == input$inputYear &
                                data()$Nazwa %in% if(input$areaInput=="Poland") unique(data()$Nazwa) else input$areaInput, 
                              c(names(data())[1:3],input$variableInput)]
      return(fitered_data1)
    } else {
      # if range of years
      fitered_data1 <- data()[data()$rok %in% input$inputYears[1]:input$inputYears[2] &
                                data()$Nazwa %in% if(input$areaInput=="Poland") unique(data()$Nazwa) else input$areaInput, 
                              c(names(data())[1:3],input$variableInput)]
      return(fitered_data1)
    }
  })
  
  # function for displaying missings
  missings_df <- reactive({
    
    # filter data, filtered is reactive, thus will not be null, always gets generated
    req(input$variableInput)
    
    if(input$periodType == 0){
      # if on single year
      fitered_data <- data()[data()$rok == input$inputYear &
                               data()$Nazwa %in% if(input$areaInput=="Poland") unique(data()$Nazwa) else input$areaInput, 
                             c(names(data())[1:3],input$variableInput)]
    } else {
      # if range of years
      fitered_data <- data()[data()$rok %in% input$inputYears[1]:input$inputYears[2] &
                               data()$Nazwa %in% if(input$areaInput=="Poland") unique(data()$Nazwa) else input$areaInput, 
                             c(names(data())[1:3],input$variableInput)]
    } 
    
    # data frame for missing values, columns-years, rows-variables
    missings = as.data.frame(matrix(NA, ncol=max(fitered_data$rok)-min(fitered_data$rok)+1, nrow=ncol(fitered_data)))
    
    j=1
    # add number of missing values in each year for each variable 
    for(i in seq(min(fitered_data$rok), max(fitered_data$rok),1)){
      
      # number of missing values in given year and variable
      col  = sapply(fitered_data[fitered_data$rok==i,], function(x) sum(is.na(x)))
      # input to right cell
      missings[,j] = col
      j=j+1
    }
    
    # name of columns as year
    colnames(missings) = seq(min(fitered_data$rok), max(fitered_data$rok), 1)
    
    # give NA column names
    rownames(missings) <- colnames(fitered_data)
    
    return(missings)
    
  }) 
  
  # ---------------------------- tab 1 outoputs ------------------
  
  
  output$contents  <- DT::renderDataTable({
    input$allData
    isolate({
      upload()
    })
  })
  # data output
  output$dataOutput <- DT::renderDataTable({
    input$filterData
    isolate({
      filtered()
    })
  })
  
  # missing data table
  output$missingsOutput <- DT::renderDataTable({
    input$filterMissings
    isolate({
      missings_df()
    })
  })
  

  # -------------------------------------------------- tab 2 -------------------------------------------------
  
  # function to extract numbers of breaks from breaksInput | this needs to be in reactive, because uses input that needs to be updates and
  # needs to return a value
  fixedBreaks <- reactive({
    # separate text inputs
    breaks <- as.numeric(unlist(stringr::str_split(input$breaksInput, pattern = "\\s+")))
  })

  sp_map <- reactive({
    if(is.null(pov_sp)){
      message("Shp map is being loaded.")
      pov_sp <- readOGR("../data/powiaty4", "powiaty", encoding = "UTF-8", stringsAsFactors = F)
    }
  })


  json_list_map <- reactive({
    if(is.null(pov_sp)){
      message("Json map is being loaded.")
      pov_json_list <- readRDS("../data/poviaty_json_list.RDS")
    }
  })

  ineractive_map <- reactive({

    # initially no variabls, so map cannot be generated, so initially empty screen
    req(input$variableInput2)

    if(input$groupingTypeInput == "fixed"){

      # if fixed but automatic retun notification of error and do not still NULL
      if(input$bucketingTypeInput == 0){
        showNotification("Fixed type cannot be automatic!",
                         type="error",
                         duration = 7)
        return(NULL)

      } else {

        # if fixed but manual, generate break from provided numbers
        # else if(input$bucketingTypeInput == 0 & input$groupingTypeInput == "fixed"){
        breaks <- fixedBreaks()

        # if breaks are not correct (not numbers, that were coerced to NA) notify and NULL
        if(anyNA(breaks)){
          showNotification("All breaks must be numeric!",
                           type="error",
                           duration = 7)
          return(NULL)

          # if fewer than expected breaks nofity error and return NULL
        } else if(length(breaks) != input$ngroupsInput+1){
          showNotification(paste0("Number of groups doesn't match number of breaks!\n Required ",
                                  input$ngroupsInput + 1," digits"),
                           type="error",
                           duration = 7)
          return(NULL)
        }
      }
    }

    # think of replacing notify by  validate() to get information <- write funciton for these

    # do not show anyting at the beggining, otherwise error
    dane <- data()[data()$rok == input$inputYear2,
                 c(names(data())[2:3], "jpt_kod_je", input$variableInput2)]

    # if one recalculates the map in the same session, change number of groups
    if(input$bucketingTypeInput == 1){
      ngroupsInput <- input$ngroupsInput
    } else {
      ngroupsInput <- NULL
    }

    # read map first time function is used
    pov_json_list <- json_list_map()

    get_interactive_map(
      plot_data = dane,                         # frame with data (variables)
      map_json = pov_json_list,                 # spatial object  - json_list (special for highcharter)
      mapped_variable = 4,                       # index of variable for mapping (always 4) in this setting
      joining_var = "jpt_kod_je",
      groups_quantity = ngroupsInput,           # nmber of groups to be created in map
      title = input$inputTitle,                 # map title
      bucketing_seed = input$seedInput,
      bucketing_type = input$groupingTypeInput, # bucketing algorithm
      breaks = breaks,
      colors_palette = input$inputPalette,      # coloring palette name
      reverse_palette = input$reverseColor                   # reverse palette
    )
  })


  static_map <- reactive({

    # initially no variabls, so map cannot be generated, so initially empty screen
    req(input$variableInput2)

    if(input$groupingTypeInput == "fixed"){

      # if fixed but automatic retun notification of error and do not still NULL
      if(input$bucketingTypeInput == 0){
        showNotification("Fixed type cannot be automatic!",
                         type="error",
                         duration = 7)
        return(NULL)

      } else {

        # if fixed but manual, generate break from provided numbers
        # else if(input$bucketingTypeInput == 0 & input$groupingTypeInput == "fixed"){
        breaks <- fixedBreaks()

        # if breaks are not correct (not numbers, that were coerced to NA) notify and NULL
        if(anyNA(breaks)){
          showNotification("All breaks must be numeric!",
                           type="error",
                           duration = 7)
          return(NULL)

          # if fewer than expected breaks nofity error and return NULL
        } else if(length(breaks) != input$ngroupsInput+1){
          showNotification(paste0("Number of groups doesn't match number of breaks!\n Required ",
                                  input$ngroupsInput + 1," digits"),
                           type="error",
                           duration = 7)
          return(NULL)
        }
      }
    }

    # think of replacing notify by  validate() to get information <- write funciton for these
    
    # do not show anyting at the beggining, otherwise error
    dane <- data()[data()$rok == input$inputYear2, 
                 c(names(data())[2:3], "jpt_kod_je", input$variableInput2)] 
    

    # if one recalculates the map in the same session, change number of groups
    if(input$bucketingTypeInput == 1){
      ngroupsInput <- input$ngroupsInput
    } else {
      ngroupsInput <- NULL
    }

    # read map first time function is used
    pov_sp <- sp_map()

    get_ggplot_map(
        plot_data = dane,                                  # frame with data (variables)
        map_sp = pov_sp,                                   # spatial object  - json_list (special for highcharter)
        mapped_variable = 4,                            # index of variable for mapping (always 4) in this setting
        joining_var = "jpt_kod_je",
        groups_quantity = ngroupsInput,                     # nmber of groups to be created in map
        bucketing_seed = input$seedInput,                             # seed if bclust or kmeans
        bucketing_type = input$groupingTypeInput,                  # bucketing algorithm
        breaks = breaks,
        colors_palette = input$inputPalette,                    # coloring palette name
        reverse_palette = input$reverseColor,                       # reverse palette
        title = input$inputTitle,
        title_size = input$titleSize,
        legend_title_size = input$legendTitleSize,
        legend_label_size = input$legendLabelSize
    )

  })
  

  # ---------------------------- tab 2 outoputs ------------------
  
  # --- output interactve ---

  # text1 <- eventReactive(input$filterAction2, {
  #   paste("Static map status:", input$staticMap)
  # })
  # output$staticMapOff <- renderText({
  #   text1()
  # })

  plot1 <- eventReactive(input$filterAction2, ineractive_map())
  output$interactiveMapOutput <- renderHighchart({
    plot1()
  })

  # --- output static ---

  # text2 <- eventReactive(input$filterAction3, {
  #   paste("Static map status:", input$staticMap)
  # })
  # output$staticMapOn <- renderText({
  #   text2()
  # })

  plot2 <- eventReactive(input$filterAction3, static_map())
  output$staticMapOutput <- renderPlot({
    plot2()
  })
  
  output$palettes <- renderPlot({
    display.brewer.all()
  })

  # --- output download ---

  # Downloadable csv of selected dataset ----
  output$downloadInteractMap <- downloadHandler(
    filename = function() {
      paste0(input$variableInput2, "_", input$inputYear2 ,".html")
    },
    content = function(file) {
      saveWidget(ineractive_map(), file)
    }
  )
  
  output$downloadggplotMap <- downloadHandler(
    filename = function() {
      paste0(input$variableInput2, "_", input$inputYear2 ,".png")
    },
    content = function(file) {
      ggsave(file, static_map(), width = 16, height = 12, units = "cm")
    }
  )

  
  # -------------------------------------------------- tab 3 -------------------------------------------------
  
  
  get_analysis_plot <- reactive({
    
    req(input$year, input$var, input$var2, input$bars)
    
    #all the input
    year = input$year
    var1 = input$var
    var2 = input$var2
    bars = input$bars
    
    #using one year only
    data_subset <- data()[data()$rok==input$year,]
    
    # get spatial weights matrix
    cont.listw <- get_weigth_matrix()
    
    #adjusting the histogram
    if(input$x_lower == '' | input$x_upper==''){
      x_lower = summary(data_subset[,match(var1, colnames(data_subset))])[1]
      x_upper = summary(data_subset[,match(var1, colnames(data_subset))])[6]
      
      x_lower = as.numeric(x_lower)
      x_upper = as.numeric(x_upper)
    }
    else{
      x_lower = as.numeric(input$x_lower)
      x_upper = as.numeric(input$x_upper)
    }
    
    #adjusted histogram
    source("../scripts/HISTOGRAM.R")
    his=nice_histogram(as.data.frame(data_subset), 
                       match(var1,colnames(data_subset)),
                       bars,
                       x_lower,
                       x_upper)
    
    #waffle plot with missing values
    source("../scripts/MISSINGS.R")
    mis=missings(as.data.frame(data_subset),
                 match(var1, colnames(data_subset))
                 )
    
    #scatterplot
    source("../scripts/SCATTERPLOT.R")
    sca=scatterplot(as.data.frame(data_subset), var1, var2)
    
    #calculates Morans's I and prepares a pie chart
    result01 <- moran.test(data_subset[,match(var1,colnames(data_subset))],
                         cont.listw)
    
    if(result01$estimate[1]>0){
      label = round(result01$estimate[1],4)
      moran_stat_df <- data.frame(groups=c('a','b'),
                            values=c(result01$estimate[1],1-abs(result01$estimate[1])))
      dir=1
      col="#00b159"
    }
    if(result01$estimate[1]<0){
      label = round(result01$estimate[1],4)
      moran_stat_df <- data.frame(groups=c('a','b'),
                            values=c(-result01$estimate[1],1+abs(result01$estimate[1])))
      dir=-1
      col="#d11141"
    }
    
    mor <- ggplot(moran_stat_df, aes(x="", y=values, fill=groups)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0, direction=dir) +
      theme_void() +
      scale_fill_manual(values = c(col,"#00aedb")) +
      ggtitle(paste("Moran's I for ",var1,sep='')) +
      theme(legend.position="none") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(aes(y = values+0.1, label = c('',label)), color = 'white', size=7)
    
    #finds top 10 correlated variables (considering its absolute values)
    source("../scripts/TOP_CORRELATIONS.R")
    vars = find_best_predictors(data_subset, var1)
    
    #combined plot
    combined_plot <- grid.arrange(grobs=list(his, mor, sca, mis, tableGrob(vars, rows=NULL, theme=ttheme_minimal(base_size = 10))),
                 layout_matrix = rbind(c(1,1,2,5),
                                       c(3,3,4,5)))
    
    print(class(combined_plot))
    
    return(combined_plot)
  })
  
  
  output$plots <- renderPlot({
    input$getAnalysis 
    isolate({
      get_analysis_plot()
    })
  })



  # -------------------------------------------------- tab 4 -------------------------------------------------

  get_weigth_matrix <- reactive({
    cont.nb<-poly2nb(as(sp_map(), "SpatialPolygons"))
    cont.listw<-nb2listw(cont.nb, style="W")
  })
  
    # prepares models formula
    fitter <- reactive({
      
      # get spatial weights matrix
      cont.listw <- get_weigth_matrix()
      
      req(input$ChosenYear, input$DependentVariable, input$IndependentVariables, input$bars)

      #prepares data subset
      data_subset = data()[data()$rok==input$ChosenYear,
                         match(c(input$DependentVariable, input$IndependentVariables), colnames(data()))]

      #classic
      if(input$Distance=='default: y~x'){
        model_formula = as.formula(
          paste(input$DependentVariable," ~ ",paste(input$IndependentVariables,
                                                    collapse="+")))
      }

      #multinomial
      if(input$Distance=='multinomial: y~x+x^2+x^3+x^4'){
        model_formula = as.formula(
          paste(input$DependentVariable," ~ ",paste(paste('poly(',input$IndependentVariables,',4)',sep=''),
                                                    collapse="+")))
      }

      #power
      if(input$Distance=='power: log(y)~log(x)'){
        model_formula = as.formula(
          paste(paste('log(',input$DependentVariable,'+1)',sep='')," ~ ",paste(paste('log(',input$IndependentVariables,'+1)',sep=''),
                                                                               collapse="+")))
      }

      #exponential
      if(input$Distance=='exponential: log(y)~x'){
        model_formula = as.formula(
          paste(input$DependentVariable," ~ ",paste(paste('log(',input$IndependentVariables,'+1)',sep=''),
                                                    collapse="+")))
      }

      #runs OLS
      if(input$ChosenModel=='ols'){
        fit <- lm(model_formula, data=data_subset)
      }

      #runs Manski model
      if(input$ChosenModel=='manski'){
        fit <- sacsarlm(model_formula,
                        data=data_subset,
                        listw=cont.listw,
                        type="sacmixed")
      }

      #runs SAC model
      if(input$ChosenModel=='sac'){
        fit <- sacsarlm(model_formula,
                        data=data_subset,
                        listw=cont.listw)
      }

      #runs SDEM model
      if(input$ChosenModel=='sdem'){
        fit <- errorsarlm(model_formula,
                          data=data_subset,
                          listw=cont.listw,
                          etype="emixed")
      }

      #runs SEM model
      if(input$ChosenModel=='sem'){
        fit <- errorsarlm(model_formula,
                          data=data_subset,
                          listw=cont.listw)
      }

      #runs SDM model
      if(input$ChosenModel=='sdm'){
        fit <- lagsarlm(model_formula,
                        data=data_subset,
                        listw=cont.listw,
                        type="mixed")
      }

      #runs SAR model
      if(input$ChosenModel=='sar'){
        fit <- lagsarlm(model_formula,
                        data=data_subset,
                        listw=cont.listw)
      }

      #runs SLX model
      if(input$ChosenModel=='slx'){
        fit <- lmSLX(model_formula,
                     data=data_subset,
                     listw=cont.listw)
      }

      print(class(fit))
      
      return(fit)

    })

    recom <- reactive({
      
      req(input$ChosenYear, input$DependentVariable)
      
      data_subset = data()[data()$rok==input$ChosenYear,]

      source("../scripts/STEPWISE_VARS.R")
      rec=recommendation(data_subset,input$DependentVariable)

      return(rec)
    })

    #returns chosen model's summary
    output$evaluation <- renderPrint({
      input$fitModel
      isolate(summary(fitter()))
    })

    output$recommendation <- renderPrint({
      input$fitModel
      isolate({
        paste('Recommended variables are:',paste(recom(),collapse=", "))
      })
      
    })
    
  
  })

# https://datascienceplus.com/making-a-shiny-dashboard-using-highcharter-analyzing-inflation-rates/
# https://cran.r-project.org/web/packages/rmapshaper/vignettes/rmapshaper.html





