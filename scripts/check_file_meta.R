library(data.table)

# function to check which separate files in a folder have less unique values than selected treshhold (if kod powiatu < 380) 
check_file_meta <- function(folder_path,
                                    file_extension = "csv",
                                    treshold = 380,
                                    zmienna_sprawdzana = "Kod",
                                    sort = TRUE
                                    ){
  
  #---------------------------------------------------------
  # raw data folder file names 
  
  data_files <- list.files(folder_path) # "data/clear/"
  print(paste("Folder contains", sum(grepl(file_extension, data_files)), "of", file_extension, "files."))
  
  # pick file of selecter extension
  data_files <- data_files[grepl(file_extension, data_files)]
  
  #---------------------------------------------------------
  
  # vector for filenames that has already been long, thus omminted in firther transformation 
  number_of_entities <- c()
  
  # loop though files and save number of poviats in each file
  for(i in 1:length(data_files)){ 
    
    # read current file
    data <- fread(paste0(folder_path, data_files[i]), encoding = "UTF-8") # fread fater but could not deal with long file names
    # data <- read.csv(paste0("data/clear/",data_files[i]), sep=";", encoding = "UTF-8")
    
    number_of_entities[data_files[i]] <- uniqueN(data[,..zmienna_sprawdzana])
  }
  if(sort == TRUE){
    return(sort(unlist(number_of_entities)[unlist(number_of_entities) < treshold], decreasing = T))
  } else {
    return(unlist(number_of_entities)[unlist(number_of_entities) < treshold])
  }
}





