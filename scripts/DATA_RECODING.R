
data_recoding<-function(dataset){
  
  #dataset=data
  
  options(warn=-1)
  
  char_cols<-c()
  to_drop<-c()
  
  dataset <- as.data.frame(dataset)
  
  for(i in c(1:ncol(dataset))){
    
    #if there are at most 10 unique values, it's probably a factor
    if(length(levels(as.factor(dataset[,i])))<=10){
      dataset[,i]<-as.factor(dataset[,i])
    }
    
    #if there are more than 10 unique values, it's probabaly either a numeric or string
    if(length(levels(as.factor(dataset[,i])))>10){
      
      #vhanges into character
      dataset[,i]<-as.character(dataset[,i])
      
      #a copy
      temp<-dataset[,i]
       
      #tires recoding a copy into numeric
      temp<-try(as.numeric(temp), silent=T)
      
      #if there are no only NAs, change the original one into numeric
      if(sum(is.na(temp))<0.9*nrow(dataset)){
        dataset[,i]<-as.numeric(dataset[,i])
      }
      
      #remembers characters
      if(sum(is.na(temp))>=0.9*nrow(dataset)){
        char_cols<-append(char_cols,i)
      }
    }
    
  }
  
  options(warn=0)
  
  return(invisible(list(dataset=dataset,characters=char_cols)))
}
