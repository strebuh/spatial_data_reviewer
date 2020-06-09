missings <- function(data, i, max_name_length=24, ...){
  
  title = colnames(data)[i]
  
  if(nchar(title)>max_name_length){
    title = substr(title, 1, max_name_length)
    title = as.character(paste(title,"..."))
  }
  
  #prepares table with amount of missings
  categ_table <- c(round((nrow(data)-sum(is.na(data[,i])))/nrow(data)*100,0),
                   round(sum(is.na(data[,i]))/nrow(data)*100,0))
  
  #sets colnames in that table
  names(categ_table)<-c("values","NAs")
  
  #prepares waffle plot with missings' share
  waffle_plot<-waffle(categ_table, 
                      #size = 24,
                      rows=10,  #10 rows to make one square an equivalent of 1% share
                      glyph_size=6,
                      #xlab = "1 square = 1% of variable's values",
                      col=c("darkgreen","red")) +
    theme(legend.position="none") + 
    ggtitle(paste('Missings in ',title,': ',categ_table[2],'%',sep='')) + 
    theme(plot.title = element_text(hjust = 0.5, size=14))
  
  return(waffle_plot)
}
