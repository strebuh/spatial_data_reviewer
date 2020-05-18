library(data.table)

Sys.setenv(LANG = "pl")

# check local langauge settings
Sys.getlocale()

# change system local language setting to Polish
Sys.setlocale(category = "LC_ALL", locale = "Polish") 

#--------------------------------------------------------------------------------------------------------
# dict for replacing letters in file names (occured to be not helpful)
dict <- list(
  ą="a",
  Ą="A",
  ć="c",
  Ć="Ć",
  ę="e",
  Ę="e",
  ł="l",
  Ł="L",
  ó="o",
  Ó="O",
  ś="s",
  Ś="S",
  ż="Z",
  Ż="Z",
  ź="z",
  Ź="Z",
  ń="n",
  Ń="N")

# encoding <- "UTF-8"
# data_files <- list.files("data/raw/")
# data <- fread(paste0("data/raw/", data_files[37]), encoding = encoding, dec=",") # fread fater but could not deal with long file names
# str(data)

reshape_files <- function(source_path = "data/raw/",
                            file_extension = "csv",
                            destination_file = "data/clear/",
                            encoding = "UTF-8"){
  
  #---------------------------------------------------------
  # raw data folder file names 
  
  data_files <- list.files(source_path)
  message(paste("Folder contains", sum(grepl(file_extension, data_files)), "of", file_extension, "files."))
  
  # pick file of selecter extension
  data_files <- data_files[grepl(file_extension, data_files)]

  # vector for filenames that has already been long, thus omminted in firther transformation 
  already_long <- c()
  
  # loop though files and process each
  for(i in 1:length(data_files)){ 
    
    # read current file
    data <- fread(paste0(source_path, data_files[i]), encoding = encoding, dec=",") # fread fater but could not deal with long file names
    # data <- read.csv(paste0("data/raw/",data_files[i]), sep=";", encoding = encoding)
    # # transform to data table
    # setDT(data)
    
    # if columns of current file don't have a 4 digits (year) in theirs names, file is long already, skip the transformation
    if(!rlang::is_empty(names(data)[grepl("[1-9]", names(data)) & nchar(names(data)) > 4])){
  
      # reshape to long format
      data_long <- melt(data,
                        # columns with no number in name
                        id.vars = names(data)[!grepl("[1-9]", names(data))], 
                        # colname longer than 4 char and containing number 
                        measure.vars = names(data)[grepl("[1-9]", names(data)) & nchar(names(data)) > 4], 
                        variable.name = "var1",
                        value.name = "value",
                        variable.factor = F,
                        na.rm = F
      )
  
      # remove square bracket and dot
      data_long <- data_long[,var1 := gsub("\\[|\\]|\\|\\.|/","", var1)]
      
  
      # picking just poviat related rows
      data_long <- data_long[grepl("Powiat", Nazwa),]
  
      # number of columns to be after split
      rok_compounds_num <- length(tstrsplit(data_long$var1[1], ";", fixed=T))
  
      # new names for column to be splited into multiple
      rok_splitted_names <- c(paste0("var", seq(1,rok_compounds_num)))
  
      # separate into multiople columns to get year column
      data_long <- tidyr::separate(data_long, "var1", rok_splitted_names, sep = ";", remove = TRUE,
               convert = TRUE, extra = "warn", fill = "warn")
  
      # convert to dataframe (to be able to use character vector of names)
      data_long <- as.data.frame(data_long)
  
      # get names of character column among those which were split
      to_merge <- rok_splitted_names[sapply(data_long[,rok_splitted_names], function(x) !is.numeric(x))]
  
      # get the year column name
      year_column =  rok_splitted_names[!rok_splitted_names %in% to_merge]
      # rename it to rok
      names(data_long)[names(data_long) == year_column] <- "rok"
      
      # subset the data to be merged into one column
      data_to_merge <- data_long[,to_merge]
  
      # merge character columns to be used in spliting the data into more tables
      data_to_merge <- tidyr::unite(data_to_merge, "var1", sep = "-", remove = TRUE, na.rm = FALSE)
  
      # combine united column with the rest of the data
      data_long <- cbind(data_long[, !(names(data_long) %in% to_merge)], data_to_merge)
      # setDT(data_long)
  
      # split into multiple tables
      split_list <- split(data_long, data_long$var1)
      # length(split_list)
  
      # to save left
      for(j in 1:length(split_list)){
        
        # adjust filenames replacing given characters: ), (, ., csv, at least 4 digits, -, ",", ",liczba"
        current_file_name <- paste(gsub("\\.csv", "", data_files[i]), unique(split_list[[j]]$var1), sep = "_")
        current_file_name <- gsub("\\s|-|=|\\(|\\)|\\.+","_", current_file_name) # space to _
        current_file_name <- gsub("(\\,\\d)|,|[0-9]{4,}", "", current_file_name) # -, 40,1 to 40
        current_file_name <- gsub("_+","_", current_file_name) # space to _
        current_file_name <- gsub("%","proc", current_file_name) # % to proc
        current_file_name <- gsub("(_)$","", current_file_name) # % to proc

        # change polish letters in filenames
        for(l in names(dict)){
          current_file_name <- gsub(l, dict[[l]], current_file_name)
        }
        # print(current_file_name)
        
      # rename the value column as filename
      names(split_list[[j]])[4] <- current_file_name


      # remove last column (all rows contain filename/value name)
      split_list[[j]] <- split_list[[j]][,-c(ncol(split_list[[j]]))]


      message(paste("writing:", current_file_name))
      # save CSV
      write.csv(split_list[[j]], paste0(destination_file, current_file_name, ".csv"), fileEncoding = encoding)
      
      }
    } else {
      already_long <- c(already_long, data_files[i])
    }
  }
  message(length(already_long)," files were already in long form.")
}


# 
# a <- "tą liczbę ów podał"
# print(a)
# 
# source_path = "data/raw/"
# file_extension = "csv"
# destination_file = "data2/clear/"
# encoding = "UTF-8"
# i <- 1
# 
# #---------------------------------------------------------
# # raw data folder file names 
# 
# data_files <- list.files(source_path)[1:5]
# message(paste("Folder contains", sum(grepl(file_extension, data_files)), "of", file_extension, "files."))
# 
# # pick file of selecter extension
# data_files <- data_files[grepl(file_extension, data_files)]
# 
# # vector for filenames that has already been long, thus omminted in firther transformation 
# already_long <- c()
# 
# # loop though files and process each
# # for(i in 1){ #:length(data_files)){ 
#   
#   # read current file
#   data <- fread(paste0(source_path, data_files[i]), encoding = encoding) # fread fater but could not deal with long file names
#   # data <- read.csv(paste0("data/raw/",data_files[i]), sep=";", encoding = encoding)
#   # # transform to data table
#   # setDT(data)
#   
#   # if columns of current file don't have a 4 digits (year) in theirs names, file is long already, skip the transformation
#   # if(!rlang::is_empty(names(data)[grepl("[1-9]", names(data)) & nchar(names(data)) > 4])){
#     
#     # reshape to long format
#     data_long <- melt(data,
#                       # columns with no number in name
#                       id.vars = names(data)[!grepl("[1-9]", names(data))], 
#                       # colname longer than 4 char and containing number 
#                       measure.vars = names(data)[grepl("[1-9]", names(data)) & nchar(names(data)) > 4], 
#                       variable.name = "var1",
#                       value.name = "value",
#                       variable.factor = F,
#                       na.rm = F
#     )
# 
#     # remove square bracket and dot from variable that contains wide file variable names
#     data_long <- data_long[,var1 := gsub("\\[|\\]|\\|\\.|/","", var1)]
#     head(data_long$var1)
#     dim(data_long) # [1] 27370     4
#     
#     # picking just poviat related rows
#     data_long <- data_long[grepl("Powiat", Nazwa),]
#     dim(data_long) # [1] 2660    4
#     
#     # split Var1 column into multiple by semi colon, because it contains multiple information including a year. we wanna get year into semarate column, the
#     # rest leave as one column
#     # number of columns to be after split
#     rok_compounds_num <- length(tstrsplit(data_long$var1[1], ";", fixed=T))
#     
#     # new names for column to be splited into multiple
#     rok_splitted_names <- c(paste0("var", seq(1,rok_compounds_num)))
#     
#     # separate into multiple columns to get year column
#     data_long <- tidyr::separate(data_long, "var1", rok_splitted_names, sep = ";", remove = TRUE,
#                                  convert = TRUE, extra = "warn", fill = "warn")
#     dim(data_long)
#     # convert to dataframe (to be able to use character vector of names)
#     data_long <- as.data.frame(data_long)
#     
#     # get names of a character columns among those which were created by split (numeric has a year value and we dont wanna transform it)
#     to_merge <- rok_splitted_names[sapply(data_long[,rok_splitted_names], function(x) !is.numeric(x))]
#     
#     # get the year column name
#     year_column =  rok_splitted_names[!rok_splitted_names %in% to_merge]
#     # rename it to rok
#     names(data_long)[names(data_long) == year_column] <- "rok"
#     
#     # subset the data to be merged into one column
#     data_to_merge <- data_long[,to_merge]
#     
#     # merge character columns to be used in spliting the data into more tables, as var1 column
#     data_to_merge <- tidyr::unite(data_to_merge, "var1", sep = "-", remove = TRUE, na.rm = FALSE)
#     
#     # combine united column with the rest of the data
#     data_long <- cbind(data_long[, !(names(data_long) %in% to_merge)], data_to_merge)
#     # setDT(data_long)
#     
#     # print(head(data_long))
#     
#     # split into multiple tables
#     split_list <- split(data_long, data_long$var1)
#     length(split_list)
#     class(split_list)
#     dim(split_list[[1]])
#     
#     j <- 1
#     # to save left
#     # for(j in 1:length(split_list)){
#       
#       # adjust filenames replacing given characters: ), (, ., csv, at least 4 digits
#       current_file_name <- paste(gsub("[0-9]{4,}|\\(\\)|\\.|csv", "", data_files[i]), unique(split_list[[j]]$var1), sep = "_")
#       current_file_name <- gsub("\\s","_", current_file_name) # space to _
#       current_file_name <- gsub("%","proc", current_file_name) # % to proc
#       
#       # change polish letters in filenames
#       for(l in names(dict)){
#         current_file_name <- gsub(l, dict[[l]], current_file_name)
#       }
#       
#       # rename the value column as filename
#       names(split_list[[j]])[4] <- current_file_name
#       
#       
#       
#       # remove last column (all rows contain filename/value name)
#       split_list[[j]] <- split_list[[j]][,-c(ncol(split_list[[j]]))]
#       
#       print(current_file_name)
#       
#       # message(paste("writing:", current_file_name))
#       # save CSV 
#       write.csv(split_list[[j]], paste0(destination_file, current_file_name, ".csv"), fileEncoding = encoding)
#       
#     # }
# # }


a <- gsub("(\\,\\d)|-", "", "RYNKOWA_SPRZEDAZ_LOKALI_MIESZKALNYCH__CTAB__rynek_wtorny-od_40,1_do_60_m2-szt." )
gsub("[0-9]{4,}|\\(\\)|\\.|csv", "", a)
