
scatterplot = function(data,x,y){
  
  plot=ggplot(data, aes_string(x = x, y = y)) + # x = data[,match(x,colnames(data))],  y = data[,match(y,colnames(data))]
    geom_point() +
    theme(legend.position="none") + 
    ggtitle(paste(x,' vs ',y,sep='')) + 
    labs(x=x,y=y) +
    theme(plot.title = element_text(hjust = 0.5, size=14))
  
  return(plot)
}
