library(data.table)
library(stringr)
library(RColorBrewer)
library(classInt)


get_static_map <- function(
  plot_data,                                  # frame with data (variables)
  map_sp,                                   # spatial object  - json_list (special for highcharter)
  mapped_variable,                            # index of variable for mapping (always 4) in this setting
  joining_var = "jpt_kod_je",
  groups_quantity = NULL,                     # nmber of groups to be created in map
  title,                                      # map title
  bucketing_seed,                             # seed if bclust or kmeans
  bucketing_type = "hclust",                  # bucketing algorithm
  breaks = c(),
  colors_palette = "BuPu",                    # coloring palette name
  reverse_palette = FALSE,                       # reverse palette
  legend_place = "bottomleft",                # "bottomleft", "left", "topleft", "top", "topright", "right" and "center". 
  ncol_legend = 1,
  save_path = NULL
){

    # --------------------------------------------------------------------
    
    # which variables is being mapped
    message(paste0("Mapped varisle is: ", names(plot_data)[mapped_variable]))
    
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

    # --------------------------------------------------------------------
    
    # --------------------------------------------------------------------

    # 1. Open jpeg file
    if(!is.null(save_path)) png("rplot2.png", width = 600, height = 600)
    # 2. Create the plot
      plot(map_sp, col = color.table)
      legend(legend_place, 
           legend = names(attr(color.table, "table")),
           fill = attr(color.table, "palette"), 
           cex=0.8, 
           bty="n", 
          ncol = ncol_legend
          )
      title(main = title)
     # 3. Close the file
    if(!is.null(save_path)) dev.off() 
    
    # plot(pov_sp2, col=color.table)
    #   legend("bottomleft", legend=names(attr(color.table, "table")),
    #          fill=attr(color.table, "palette"), cex=0.8, bty="n")
    #   title(main=title)
}


