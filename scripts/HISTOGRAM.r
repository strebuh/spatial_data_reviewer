nice_histogram = function(data,i,bars=20,
                          x_lower=min(na.omit(data[,i])),
                          x_upper=max(na.omit(data[,i]))){
  
  title = colnames(data)[i]
  
  if(nchar(title)>24){
    title = substr(title, 1, 24)
    title = as.character(paste(title,"..."))
  }
  
  histogram<-ggplot(data, aes(data[,i])) + 
    geom_histogram(breaks=seq(x_lower, x_upper, 
                              by=(x_upper-x_lower)/bars),
                   col="red", 
                   aes(fill=..count..)) +
    theme(legend.position="none") + 
    ggtitle(title) + 
    labs(x="",y="") +
    theme(plot.title = element_text(hjust = 0.5, size=8)) +
    
    #bars are filled with different colours considering the numerical amount in each interval
    scale_fill_gradient("Numerical amount", low="darkblue", high="darkorange")
  
  return(histogram)
}

