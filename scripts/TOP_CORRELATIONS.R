find_best_predictors = function(data_lr,y_name){
  
  data_lr=data_lr[,-match(c('Kod','Nazwa','rok','jpt_kod_je'),colnames(data_lr))]
  
  to_remove = c()
  for(i in c(1:ncol(data_lr))){
    if(sum(is.na(data_lr[,i]))>=0.5*ncol(data_lr)){
      to_remove = append(to_remove,i)
    }
  }
  data_lr = data_lr[,-to_remove]
  data_lr = na.omit(data_lr)
  
  useful_vars = cor(data_lr)
  useful_vars = as.data.frame(x=useful_vars[,match(y_name,colnames(useful_vars))])
  colnames(useful_vars) = 'x'
  useful_vars$variable = rownames(useful_vars)
  useful_vars$correlation = useful_vars$x
  useful_vars$abs = abs(useful_vars$correlation)
  
  useful_vars = useful_vars %>%
    arrange(desc(abs)) %>%
    top_n(21) %>%
    select(-x) %>%
    select(-abs)
  
  useful_vars = useful_vars[-1,]
    
  return(useful_vars)
  
}