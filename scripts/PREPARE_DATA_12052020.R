
prepare_data = function(data,years,variables){
  
  #data = data
  #years = 2018
  #variables = vars
  
  which_rows = which(data$rok %in% years==TRUE)
  which_columns = match(variables, colnames(data))
  
  data = data[which_rows,which_columns]
  
  #recoding variables if neccessary
  for(i in c(1:ncol(data))){
    if(length(levels(as.factor(data[,i])))>10){
      temp = as.numeric(gsub(",",".",as.character(na.omit(data[,i]))))
      if(all(is.na(temp))==FALSE){
        data[,i] = as.numeric(gsub(",",".",as.character(data[,i])))
      }
    }
  }
  
  #removing NAs, first we look for columns with number of NA > unique(data$Kod)-380
  to_remove = c()
  for(i in c(1:ncol(data))){
    if(sum(is.na(data[,i]))>length(unique(data$Kod))-380){
      to_remove = append(to_remove,i)
    }
  }
  
  if(!is.null(to_remove)){
    data = na.omit(data[,-to_remove])
  }
  
  source('MAPPING_TERYTS_12052020.R')
  data = correct_teryts(data)[,-1]
  
  return(data)
}