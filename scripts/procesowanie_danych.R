

# Sys.setenv(LANG = "en")

length(list.files("data/clear/"))

getwd()
list.files()

source("scripts/reshape_files.R", encoding = "UTF-8")
source("scripts/check_file_meta.R", encoding = "UTF-8")
source("scripts/merge_GUS_files.R", encoding = "UTF-8")
source("scripts/HISTOGRAM.R", encoding = "UTF-8")

# reshape raw files
reshape_files(source_path = "data/raw/",
              file_extension = "csv",
              destination_file = "data/clean/",
              encoding = "UTF-8")

# check files, how how many poviat there is data missing
NAs_per_file <- check_file_meta("data/clean/",
                        "csv",
                        treshold = 370,
                        zmienna_sprawdzana = "Kod",
                        sort = TRUE)

# merge into 1 file
merge_GUS_files(source_path = "data/clean/",
                file_extension = "csv",
                merging_columns = c("Kod", "Nazwa","rok"),
                destination_file = "data/GUS_all_merged3.csv",
                encoding = "UTF-8",
                dec=",")


data <- read.csv("data/clear/GUS_aLL_merged2.csv", encoding = "UTF-8", dec = ",")

dim(data)
nazwy <- data.frame(dlugie_nazwy = names(data))
View(nazwy)
write.csv(nazwy, "./lista_danych2.csv")

for(i in 5:length(names(data))){
  nice_histogram(data,5)
}

stringr::str_locate_all(pattern ='_', names(data)[7])[[1]][,1]
# max(unlist(lapply(names(data), nchar)))

title <- names(data)[nchar(names(data))==173]    

# sep_locations <- stringr::str_locate_all(pattern ='_', title)[[1]][,1]
# 
# diff(sep_locations)

# substring(title, sep_locations[c(TRUE, FALSE)], sep_locations[c(TRUE, FALSE)])

# stringr::str_split_n(title, "-", 4)

title = gsub("_"," ", title)
paste(strwrap(title, width = 30), collapse = "\n")

which(names(data) == title)

nice_histogram(data, 201)


View(head(data, 40))


source_path = "data/clean/"
list.files(source_path)
data_2 <- fread(paste0(source_path, "SKOLARYZACJA__CTAB__wspolczynnik_skolaryzacji_brutto-szkoly_podstawowe-proc.csv"), encoding = "UTF-8", dec = ",") # fread fater but could not deal with long file names
names(data_2)
data_2 <- data_2[,-c(1)]
data_2
str(data_2)

