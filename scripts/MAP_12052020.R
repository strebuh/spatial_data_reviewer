
map = function(data,year,var){
  
  variable <- data[data$rok==year,match(var,colnames(data))]
  
  
  intervals<-9
  colors<-brewer.pal(intervals, "BuPu")
  classes<-classIntervals(variable, intervals, style="fixed", 
                          fixedBreaks=c(min(variable),
                                        round(as.numeric(quantileCuts(variable,10)),0),
                                        max(variable)))
  color.table<-findColours(classes, colors) 
  
  plot(pov, col=color.table)
  plot(voi, lwd=2, add=TRUE)
  legend("bottomleft", legend=names(attr(color.table, "table")),
         fill=attr(color.table, "palette"), cex=0.8, bty="n")
}