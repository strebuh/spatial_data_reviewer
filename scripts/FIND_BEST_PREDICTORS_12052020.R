find_best_predictors = function(data_lr,y_name){

  #data_lr = data_2018[,c(5:149,154)]
  #y_name = "ZASOBY_MIESZKANIOWE_przecietna_powierzchnia_uZytkowa_1_mieszkania.m2"
  
  formula = as.formula(paste(y_name, paste(colnames(data_lr), collapse=" + "), sep=" ~ "))
  
  model <- lm(formula, data = data_lr)
  #summary(model)
  step_model <- stepAIC(model, direction = "both", trace = FALSE)
  #summary(step_model)
  
  #taking only the variables reported as significant in step-wise ols
  useful_vars = y_name
  useful_vars = append(useful_vars,names(step_model$coefficients)[-1])
  useful_vars_indexes = match(useful_vars,colnames(data_lr))
  data_lr = data_lr[,useful_vars_indexes]
  
  #removing high correlations
  data_lr_nocorr = data_lr[,-findCorrelation(cor(data_lr),cutoff = 0.5)]
  data_lr = cbind(data_lr_nocorr,data_lr[,which(colnames(data_lr)==y_name)])
  colnames(data_lr)[ncol(data_lr)] = y_name
  
  formula = as.formula(paste(y_name, paste(colnames(data_lr), collapse=" + "), sep=" ~ "))
  
  model <- lm(formula, data = data_lr)
  #summary(model)
  step_model <- stepAIC(model, direction = "both", trace = FALSE)
  #summary(step_model)
  
  return(names(step_model$coefficients)[-1])
  
}