library(data.table)
library(stringr)
library(highcharter)
library(RColorBrewer)
library(classInt)


get_interactive_map <- function(
  plot_data,                                  # frame with data (variables)
  map_json,                                   # spatial object  - json_list (special for highcharter)
  mapped_variable,                            # index of variable for mapping (always 4) in this setting
  joining_var = "jpt_kod_je",
  groups_quantity = NULL,                     # nmber of groups to be created in map
  title,                                      # map title
  bucketing_seed,                             # seed if bclust or kmeans
  bucketing_type = "hclust",                  # bucketing algorithm
  breaks = c(),
  colors_palette = "BuPu",                    # coloring palette name
  reverse_palette=FALSE                       # reverse palette
){
  
  # --------------------------------------------------------------------
  
  # which variables is being mapped
  message(paste0("Mapped varisle is: ", names(plot_data)[mapped_variable]))
  
  # prepare hover
    org_name <- names(plot_data)[mapped_variable]  
    names(plot_data)[mapped_variable] <- "value" 
    text_na_hover <- paste0("{point.Nazwa}<br>", org_name, ": {point.value}")

  
  # --------------------------------------------------------------------
  
  # variable
  variable <- plot_data$value

  # --------------------------------------------------------------------

  # split on classes 
  if(is.null(groups_quantity)){
    
    # in automatic mode you can still give seed for these two modes
    if(bucketing_type == "kmeans" | bucketing_type == "bclust") set.seed(bucketing_seed)
    
    # classes computed withoug given number
    klasy <- classIntervals(variable, style = bucketing_type)
    groups_quantity <- length(klasy$brks) - 1
    
    } else if(bucketing_type == "fixed"){
      
      # for fix compute classes using custom breaks
      klasy <- classIntervals(variable, style=bucketing_type, fixedBreaks = breaks)
      groups_quantity <- length(breaks) - 1
    
    } else {
      
      # if kmeans or bclust set seed
      if(bucketing_type == "kmeans" | bucketing_type == "bclust") set.seed(bucketing_seed)
      
      # compute classes
      klasy <- classIntervals(variable, groups_quantity, style = bucketing_type)
      
      # if sd, pretty but algorithm chose another number of classes adjust number of classes
      if(bucketing_type == "sd" | bucketing_type == "pretty" & length(klasy$brks) != groups_quantity ){
        
        groups_quantity <- length(klasy$brks) - 1
        warning(paste(bucketing_type, "has changed number of groups to", groups_quantity))
        }
      }

     # based on palette number pick appropriate number of colors, if necesarry, extend palette
      tryCatch(colors <- brewer.pal(groups_quantity, colors_palette),
               finally = {
                 colors <- colorRampPalette(brewer.pal(groups_quantity, colors_palette))(groups_quantity) 
               }
               )
      
    # if colors palette to be reverted
    if(reverse_palette == TRUE){
    colors <- rev(colors)
  }
    
  # table of colors
  colors.table <- findColours(klasy, colors)
  
  # --------------------------------------------------------------------
  
  # ranges of colors from colors.table
  ranges <- names(attr(colors.table, "table"))
  
  # extract number form ranges strings
  from <- str_replace_all(ranges, "\\[|\\(|,(-)?\\d+(\\.\\d+)?(\\)|\\])", "")
  to <- str_replace_all(ranges, "\\[|\\(|(-)?\\d+(\\.\\d+)?,|(\\)|\\])", "")
  
  # new table of colors
  colors.table <- data.frame(from = as.numeric(from),
                               to = as.numeric(to),
                               color = colors)
  colors.table_list <- list_parse(colors.table)
  
  # --------------------------------------------------------------------
  map <- highchart(type= "map") %>%
    hc_add_series(mapData = map_json,
                  type = "map",
                  name = "",
                  data = plot_data,
                  joinBy = joining_var,
                  value="value",
                  showInLegend = TRUE,
                  tooltip = list(pointFormat = text_na_hover)) %>%
    hc_colorAxis(dataClasses = colors.table_list) %>%
    hc_title(text=title,
             fontWeight = "bold") %>%
    hc_mapNavigation(enabled = TRUE) 
  
  return(map)
}

