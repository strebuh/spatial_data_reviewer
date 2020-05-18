missings <- function(data, i){
  
  title = colnames(data)[i]
  
  if(nchar(title)>24){
    title = substr(title, 1, 24)
    title = as.character(paste(title,"..."))
  }
  
  #prepares table with amount of missings
  categ_table <- c(round((nrow(data)-sum(is.na(data[,i])))/nrow(data)*100,0),
                   round(sum(is.na(data[,i]))/nrow(data)*100,0))
  
  #sets colnames in that table
  names(categ_table)<-c("values","NAs")
  
  #prepares waffle plot with missings' share
  waffle_plot<-waffle(categ_table, 
                      rows=10,  #10 rows to make one square an equivalent of 1% share
                      glyph_size=6,
                      col=c("darkgreen","red")) +
    theme(legend.position="none") + 
    ggtitle(title) + 
    theme(plot.title = element_text(hjust = 0.5, size=8))
  
  return(waffle_plot)
}

