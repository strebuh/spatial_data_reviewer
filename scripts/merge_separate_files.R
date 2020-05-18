library(data.table)


merge_GUS_files <- function(source_path = "data/clear/",
                                    file_extension = "csv",
                                    merging_columns = c("Kod", "Nazwa","rok"),
                                    destination_file = "data/clear/GUS_aLL_merged.csv",
                                    encoding = "UTF-8"){
  #---------------------------------------------------------
  # raw data folder file names 
  
  data_files <- list.files(source_path) # "data/clear/"
  message(paste("Folder contains", sum(grepl(file_extension, data_files)), "of", file_extension, "files."))
  
  # pick file of selecter extension
  data_files <- data_files[grepl(file_extension, data_files)]

  #--------------------------------------------------------------------------------------------------------
  # loop though files and save number of poviats in each file
  data <- fread(paste0(source_path, data_files[1]), encoding = encoding) # fread fater but could not deal with long file names
  data <- data[,-c(1)]
  
  # merge files in a loop
  for(i in 2:length(data_files)){ 
    # read current file
    data_2 <- fread(paste0(source_path, data_files[i]), encoding = encoding) # fread fater but could not deal with long file names
    data_2 <- data_2[,-c(1)]
    data <- merge(data, data_2, by = merging_columns, all = T) 
  }

  # check how shape of the file
  dim(data)
  
  # save the merged file
  write.csv(data, paste0(destination_file), fileEncoding = encoding)
  
  message(paste(length(data_files), "files merged into one in:", destination_file))
}

# run funcitno
merge_GUS_files(source_path = "data/clear/",
                            file_extension = "csv",
                            merging_columns = c("Kod", "Nazwa","rok"),
                            destination_file = "data/clear/GUS_aLL_merged.csv",
                            encoding = "UTF-8")


  