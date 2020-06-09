library(data.table)
library(stringr)
library(RColorBrewer)
library(classInt)
library(broom)

get_ggplot_map <- function(
  plot_data,                                  # frame with data (variables)
  map_sp,                                   # spatial object  - json_list (special for highcharter)
  mapped_variable,                            # index of variable for mapping (always 4) in this setting
  joining_var = "jpt_kod_je",
  groups_quantity = NULL,                     # nmber of groups to be created in map
  bucketing_seed = NULL,                             # seed if bclust or kmeans
  bucketing_type = "hclust",                  # bucketing algorithm
  breaks = c(),
  colors_palette = "BuPu",                    # coloring palette name
  reverse_palette = FALSE,                       # reverse palette
  theme = theme_void(),
  title,
  title_size = 14,
  legend_title_size = 13,
  legend_label_size = 12,
  ...
){
  
  map_df <- tidy(map_sp, region = joining_var)
  
  var_name <- names(plot_data)[mapped_variable]
  
  # merge data to ggplot df
  map_df <- merge(map_df, plot_data, by.x = "id", by.y = joining_var, all.x=T)
  head(map_df)
  
  # --------------------------------------------------------------------
  
  # which variables is being mapped
  message(paste0("Mapped varisle is: ", var_name))
  
  # --------------------------------------------------------------------
  
  # variable
  variable <- unlist(plot_data[,mapped_variable])
  
  # --------------------------------------------------------------------
  
  # split on classes 
  if(is.null(groups_quantity)){
    
    # in automatic mode you can still give seed for these two modes
    if(bucketing_type == "kmeans" | bucketing_type == "bclust") set.seed(bucketing_seed)
    
    # classes computed withoug given number
    classes <- classIntervals(variable, style = bucketing_type)
    groups_quantity <- length(classes$brks) - 1
    
  } else if(bucketing_type == "fixed"){
    
    # for fix compute classes using custom breaks
    classes <- classIntervals(variable, style=bucketing_type, fixedBreaks = breaks)
    groups_quantity <- length(breaks) - 1
    
  } else {
    
    # if kmeans or bclust set seed
    if(bucketing_type == "kmeans" | bucketing_type == "bclust") set.seed(bucketing_seed)
    
    # compute classes
    classes <- classIntervals(variable, groups_quantity, style = bucketing_type)
    
    # if sd, pretty but algorithm chose another number of classes adjust number of classes
    if(bucketing_type == "sd" | bucketing_type == "pretty" & length(classes$brks) != groups_quantity ){
      
      groups_quantity <- length(classes$brks) - 1
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
  color.table <- findColours(classes, colors)
  
  # ranges of colors from colors.table
  ranges <- names(attr(color.table, "table"))
  
  map_df$classes <- cut(map_df[,var_name], breaks=classes$brks, labels=ranges, include.lowest=T)
  
  # change variable names to reflect the legend
  names(map_df)[c(which(names(map_df) == var_name), which(names(map_df) == "classes"))] <- c("classes", var_name)
  
  # --------------------------------------------------------------------
  
  # --------------------------------------------------------------------
  
  map <- ggplot(data=map_df, 
                aes(y=lat, x=long, group=group)) +
    geom_polygon(aes_string(fill = var_name), color = "grey") +
    scale_fill_manual(values = colors) +
    ggtitle(title) +
    theme_void() + 
    theme(plot.title = element_text(hjust = 0.5, size = title_size), #, face = "bold"),
          legend.text=element_text(size = legend_title_size),
          legend.title=element_text(size = legend_label_size))
  
  return(map)
}


