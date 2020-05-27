library(data.table)
library(stringr)
library(rgdal)
library(spdep)

data <- fread("data/GUS_all_merged3.csv", encoding = "UTF-8", stringsAsFactors = F) # fread fater but could not deal with long file names
data <- data[,-1]
# str(data)
dim(data) # [1] 101

# write.csv(data.frame(long_names=names(data)),"names_dict4.csv")

# read data from class
data_class = read.csv('../data/data_nts4_2019.csv',sep=';',dec=',')
# data_class = data_class[,match(c('code_GUS','dist','year'), colnames(data_class))]

# correct the teryt in datas
data$teryt <- str_sub(ifelse(nchar(data$Kod)==6,paste0('0', data$Kod), data$Kod),1,4)
data_class$teryt <- str_sub(ifelse(nchar(data_class$code_GUS)==6,paste0('0', data_class$code_GUS), data_class$code_GUS),1,4)

# merge class data with other data
data <- merge(data, data_class[, c(9,11:34)], by.x=c("teryt", "rok"), by.y = c("teryt","year"), all.x=T)
data <- data[,-1]
names(data)[1:4]

# read dict of names
var_dict <- read.csv("names_dict4.csv", sep = ";", stringsAsFactors = F)

# match columns order with names in dict
data <- as.data.frame(data)
data <- data[, match(var_dict$long_names, names(data))]
# names(data)[1:4]
# head(data[,1:4])

# names(data)[1:5]
names(data) <- var_dict$short_names

names(data)
# [1] "Kod"                                           "Nazwa"                                         "rok"                                           "kob_w_bezrob"                                 
# [5] "bezrob_proc"                                   "bezrob_do_akt_zaw"                             "mieszk_na_10k_ludn"                            "przec_pow_uzytk_mieszk_m2"                    
# [9] "cz_bud_m_jednorodz_mc"                         "cz_bud_m_wielorodz_mc"                         "miesz_kanaliz_ogol"                            "miesz_wodoc_ogol"                             
# [13] "bud_mieszk_ogol"                               "dochod_pow_1_m"                                "dodat_mieszk_zl"                               "drogi_gm_pow_gr_10k_lud"                      
# [17] "drogi_gm_pow_tw_10k_lud"                       "taxi_szt"                                      "zan_pow_zakl_og"                               "gest_zal_km2"                                 
# [21] "kina_seans_og"                                 "klub_sport_cwicz_os"                           "lasy_ogol_ha"                                  "lekarz_pracuj_10k_lud"                        
# [25] "dod_mieszk_zl_2"                               "miesz_sprz_do_40_m2"                           "miesz_sprz_40_60_m2"                           "miesz_sprz_60_80_m2"                          
# [29] "miesz_sprz_od_80_m2"                           "miesz_sprz_ogol"                               "miesz_sprz_pierw_do_40_m2"                     "miesz_sprz_pierw_40_60_m2"                    
# [33] "miesz_sprz_pierw_60_80_m2"                     "miesz_sprz_pierw_od_80_m2"                     "miesz_sprz_pierw_ogol"                         "miesz_sprz_wtr_do_40_m2"                      
# [37] "miesz_sprz_wtr_40_60_m2"                       "miesz_sprz_wtr_60_80_m2"                       "miesz_sprz_wtr_80_m2"                          "miesz_sprz_wtr_ogol"                          
# [41] "kina_ludz_na_1_miejsc"                         "szpital_ogol_lozka"                            "ludn_poprod_proc"                              "ludn_prod_proc"                               
# [45] "ludn_przedprod_proc"                           "lundn_ogol"                                    "miesz_med_cen_1m2_do40"                        "miesz_med_cen_1m2_40_60"                      
# [49] "miesz_med_cen_1m2_60_80"                       "miesz_med_cen_1m2_od_80"                       "miesz_med_cen_1m2_og"                          "miesz_med_cen_1m2_pier_do_40_m2"              
# [53] "miesz_med_cen_1m2_pier_40_60_m2"               "miesz_med_cen_1m2_pier_60_80_m2"               "miesz_med_cen_1m2_pier_od_80_m2"               "miesz_med_cen_1m2_pier_ogol"                  
# [57] "miesz_med_cen_1m2_wt_do_40_m2"                 "miesz_med_cen_1m2_wt_40_60_m2"                 "miesz_med_cen_1m2_wt_60_80_m2"                 "miesz_med_cen_1m2_wt_od_80_m2"                
# [61] "miesz_med_cen_1m2_wt_og_m2"                    "mieszk_oddane_szt"                             "mieszk_oddane_pow_m2"                          "muzea_oddz_10k_lud"                           
# [65] "proby_samobojcze"                              "samob_zgon_100k_lud"                           "ciag_roln"                                     "ciezarowki_10t+"                              
# [69] "samochod_osob"                                 "porad_lekarskie_liczba"                        "przestep_stwierdz_1k_lud"                      "przestep_wykrywal_proc"                       
# [73] "PS_rodz_bezdom"                                "PS_rodz_bezrob"                                "PS_rodz_niepelnospraw"                         "PS_rodz_przemoc_rodz"                         
# [77] "PS_rodz_sierota"                               "PS_rodz_ubostw"                                "sciezk_rower_1km2"                             "sciezk_rower_10k_lud"                         
# [81] "skolar_br_szk_gim"                             "srodki_UE_1_mieszk"                            "turys_kraj_nocl_10k_lud"                       "noclegi_tury_zagran_10k_lud"                  
# [85] "uczelnie_abs_ogol"                             "studenci_og"                                   "uczelnie_og_szt"                               "przyr_natur_1k_lud"                           
# [89] "wydatk_pow_1_miesz"                            "wydatk_pow_ogol_zl2"                           "wydatk_pow_biezace"                            "wydatk_pow_majatk_inwest"                     
# [93] "wydatk_pow_majatk_ogol"                        "wynagrodz_pl100"                               "smierc_wypad_100k_lud"                         "smierc_wypad_100k_wypad"                      
# [97] "ranni_wypad_100k_wyapd"                        "mieszk_przec_izb"                              "mieszk_os_na_izb"                              "mieszk_os_na_mieszk"                          
# [101] "dist_core"                                     "tot_own_rev"                                   "pit_rev"                                       "cit_rev"                                      
# [105] "tot_Rev"                                       "invest_expend"                                 "inhabitants  "                                 "pre_product_inhab"                            
# [109] "prod_inhab"                                    "post_prod_inhab"                               "pop_1km2"                                      "pop_urab_area_1km2"                           
# [113] "reg_unemp_labor"                               "firms_10k_pop_productive"                      "salaries_pl100"                                "Employed in total"                            
# [117] "empl_agri_forest_fish"                         "empl_construction"                             "empl_trade_transp_stora_accomod_gastro_commun" "empl_finance_insuran_estate"                  
# [121] "empl_other"                                    "unempl_rate"                                   "unempl_rate_pl100"                                                                                

# check if any names are duplicated
names(data)[duplicated(names(data))]


#------------------------ simplifying 4 categories to 2 categories in few sets of columns -------------------------------

# data <- data[rok %in% 2002:2017]
setDT(data)

data <- data[,c("med_cen_sprz_do_40m2",
                "med_cen_sprz_40_80m2",
                "med_cen_sprz_od_80m2",
                "med_cen_sprz_ogol",
                "med_cen_sprz_do_40m2_pie",
                "med_cen_sprz_40_80m2_pie",
                "med_cen_sprz_od_80m2_pie",
                "med_cen_sprz_ogol_pie",
                "med_cen_sprz_do_40m2_wt",
                "med_cen_sprz_40_80m2_wt",
                "med_cen_sprz_od_80m2_wt",
                "med_cen_sprz_ogol_wt"    ):=.(miesz_med_cen_1m2_do40,               #
                                               sum(miesz_med_cen_1m2_40_60,
                                                   miesz_med_cen_1m2_60_80),
                                               miesz_med_cen_1m2_od_80,
                                               miesz_med_cen_1m2_og, 
                                               miesz_med_cen_1m2_pier_do_40_m2,      #
                                               sum(miesz_med_cen_1m2_pier_40_60_m2,
                                                   miesz_med_cen_1m2_pier_60_80_m2),
                                               miesz_med_cen_1m2_pier_od_80_m2,
                                               miesz_med_cen_1m2_pier_ogol,
                                               miesz_med_cen_1m2_wt_do_40_m2,        #
                                               sum(miesz_med_cen_1m2_wt_40_60_m2, 
                                                   miesz_med_cen_1m2_wt_60_80_m2),
                                               miesz_med_cen_1m2_wt_od_80_m2,
                                               miesz_med_cen_1m2_wt_og_m2
                )]

# [47] "miesz_med_cen_1m2_do40"          "miesz_med_cen_1m2_40_60"         "miesz_med_cen_1m2_60_80"         "miesz_med_cen_1m2_od_80"         "miesz_med_cen_1m2_og"  
# [52] "miesz_med_cen_1m2_pier_do_40_m2" "miesz_med_cen_1m2_pier_40_60_m2" "miesz_med_cen_1m2_pier_60_80_m2" "miesz_med_cen_1m2_pier_od_80_m2" "miesz_med_cen_1m2_pier_ogol"   
# [57]   "miesz_med_cen_1m2_wt_do_40_m2"   "miesz_med_cen_1m2_wt_40_60_m2"   "miesz_med_cen_1m2_wt_60_80_m2"   "miesz_med_cen_1m2_wt_od_80_m2"   "miesz_med_cen_1m2_wt_og_m2"

# [26] "miesz_sprz_do_40_m2"             "miesz_sprz_40_60_m2"             "miesz_sprz_60_80_m2"             "miesz_sprz_od_80_m2"             "miesz_sprz_ogol"                
# [31] "miesz_sprz_pierw_do_40_m2"       "miesz_sprz_pierw_40_60_m2"       "miesz_sprz_pierw_60_80_m2"       "miesz_sprz_pierw_60_80_m2"       "miesz_sprz_pierw_ogol"          
# [36] "miesz_sprz_wtr_do_40_m2"         "miesz_sprz_wtr_40_60_m2"         "miesz_sprz_wtr_60_80_m2"         "miesz_sprz_wtr_80_m2"            "miesz_sprz_wtr_ogol"  

data <- data[,c("n_miesz_sprz_do_40m2",
                "n_miesz_sprz_40_80m2",
                "n_miesz_sprz_od_80m2",
                "n_miesz_sprz_ogol",
                "n_miesz_sprz_do_40m2_pie",
                "n_miesz_sprz_40_80m2_pie",
                "n_miesz_sprz_od_80m2_pie",
                "n_miesz_sprz_ogol_pie",
                "n_miesz_sprz_do_40m2_wt",
                "n_miesz_sprz_40_80m2_wt",
                "n_miesz_sprz_od_80m2_wt",
                "n_miesz_sprz_ogol_wt"    ):=.(miesz_sprz_do_40_m2,            #
                                               sum(miesz_sprz_40_60_m2,
                                                   miesz_sprz_60_80_m2),
                                               miesz_sprz_od_80_m2,
                                               miesz_sprz_ogol, 
                                               miesz_sprz_pierw_do_40_m2,      #
                                               sum(miesz_sprz_pierw_40_60_m2,
                                                   miesz_sprz_pierw_60_80_m2),
                                               miesz_sprz_pierw_60_80_m2,
                                               miesz_sprz_pierw_ogol,
                                               miesz_sprz_wtr_do_40_m2,        #
                                               sum(miesz_sprz_wtr_40_60_m2, 
                                                   miesz_sprz_wtr_60_80_m2),
                                               miesz_sprz_wtr_80_m2,
                                               miesz_sprz_wtr_ogol)]

# drop old columns (with more levels)
data <- data[,-c(26:40,47:61)]
dim(data) # [1] 9550   117

# data frame for missing values, columns-years, rows-variables
missings = as.data.frame(matrix(NA, ncol=max(data$rok)-min(data$rok)+1, nrow=ncol(data)))

j=1
# add number of missing values in each year for each variable 
for(i in seq(min(data$rok), max(data$rok),1)){
  # number of missing values in given year and variable
  col  = sapply(data[rok==i,], function(x) sum(is.na(x)))
  # input to right cell
  missings[,j] = col
  j=j+1
}

# name of columns as year
colnames(missings) = seq(min(data$rok), max(data$rok), 1)

# give NA column names
rownames(missings) <- colnames(data)
# View(missings)

#------------------------ subset data -------------------------------

data06_18 <- data[rok > 2005 & rok < 2019,]
dim(data06_18)

# "0263" <- walbrzych nie istnieje juz ten powiat 
# "1431" <- stary teryt powiatu warszawskiego


# data06_18 <- readRDS("data/data06_18_NA.RDS")
data06_18$teryt <- str_sub(ifelse(nchar(data06_18$Kod)==6, paste0('0', data06_18$Kod), data06_18$Kod),1,4)

# drop two teryts
data06_18 <- data06_18[!(teryt %in% c("0263", "1431")),]
dim(data06_18)

# saveRDS(data06_18, "data/data06_18_NA.RDS")


#------------------------ look again at missings -------------------------------

# data frame for missing values, columns-years, rows-variables
missings2 = as.data.frame(matrix(NA, ncol=max(data06_18$rok)-min(data06_18$rok)+1, nrow=ncol(data06_18)))
j=1
# add number of missing values in each year for each variable 
for(i in seq(min(data06_18$rok), max(data06_18$rok),1)){
  # number of missing values in given year and variable
  col  = sapply(data06_18[rok==i,], function(x) sum(is.na(x)))
  # input to right cell
  missings2[,j] = col
  j=j+1
}
# name of columns as year
colnames(missings2) = seq(min(data06_18$rok), max(data06_18$rok), 1)
# give NA column names
rownames(missings2) <- colnames(data06_18)



#------------------------ fill na with neighours value -------------------------------
getwd()
# reading maps with rgdal::
pov <- readOGR("../data", "powiaty") # 380 jedn. 

# changing projections
pov <- spTransform(pov, CRS("+proj=longlat +datum=NAD83"))

# Spatial weights matrix – contiguity matrix
cont.nb <- poly2nb(as(pov, "SpatialPolygons"))
cont.listw <- nb2listw(cont.nb, style="W")
# head(cont.listw$neighbours)

# get list of neighors for each pov and rename elements to it's teryts
pov_neigh <- cont.listw$neighbours
names(pov_neigh) <- pov$jpt_kod_je

#-------------------------------------------------------------------------------------

# rematch order of poviats
pov_order <- rep(as.character(pov$jpt_kod_je), 13)
years <- unlist(lapply(2006:2018, rep, 380))
pov_year_order <- unlist(paste(pov_order, years, sep = ""))

# match order of poviats in datafile with order in map file
data06_18 <- data06_18[match(pov_year_order, paste0(data06_18$teryt, data06_18$rok)),]

View(head(data06_18[,1:10],380))

#------------------------------------------------------------------------------------

# name of columns as year
colnames(missings2) = seq(min(data06_18$rok), max(data06_18$rok), 1)

# # give NA column names
rownames(missings2) <- colnames(data06_18)
# View(missings2)


missings_t <- setDT(as.data.frame(t(missings2)))
missings_t$rok_2 <- seq(min(data06_18$rok), max(data06_18$rok), 1)


data06_18 <- as.data.frame(data06_18)
# setDT(data06_18_)

# function to clean data, which means replacing NA with mean of neighbours, given neighrours 
# function returns list of two elemetns
# 1. dataframe with NA replaced by neighborus means (only non NA neighbours are taken into accounts)
# 
fill_by_neigh_mean <- function(data,              # data frame with data
                               years = 2006:2018, # scope of years
                               rep_treshold = 66, # up to how many missing poviats to fill
                               pov_neigh,         # cont.listw
                               # spatial_sp,        # spatial object
                               missing_df=NULL
                               ){
  
  # if no missing data dastafame was given, create it
  if(is.null(missing_df)){
    
    # data frame for missing values, columns-years, rows-variables
    missing_df = as.data.frame(matrix(NA, ncol=max(data$rok)-min(data$rok)+1, nrow=ncol(data)))
    j=1
    
    # add number of missing values in each year for each variable 
    for(i in seq(min(data$rok), max(data$rok),1)){
      
      # number of missing values in given year and variable
      col  = sapply(data[rok==i,], function(x) sum(is.na(x)))
      
      # input to right cell
      missing_df[,j] = col
      j=j+1
    }
    
    # name of columns as year
    colnames(missing_df) = seq(min(data$rok), max(data$rok), 1)
   
     # give NA column names
    rownames(missing_df) <- colnames(data)
    
    missings_t <- setDT(as.data.frame(t(missings2)))
    missings_t$rok_2 <- seq(min(data$rok), max(data$rok), 1)
  }
  
  # create base for data which was replaced
  na_replaced <- c() 
  
  # loop though colum names, and years in scope
  for(i in names(data)){
    for(y in years){
      
      # check if for given year and variable there were more than 0 and less than 100 pov data missing
      if(missings_t[rok_2==y, ..i] <= rep_treshold & missings_t[rok_2==y, ..i] > 0){ # missings_t[rok_2==y, ..i] == 20){ #
        
        # subest from data, chuk of this year with this variable and teryts
        temp <- data[data$rok==y, c(i, "teryt")]
        
        # pick teryts of rows where there's NA for this column and year
        missing_teryts <- temp[!complete.cases(temp), "teryt"]
        
        # for those teryts (rows) where there's NA 
        # lapply(missing_teryts, function(x){}
        for(x in missing_teryts){
          
          # compute mean value of selected teryt neighours in given column, dropping NA
          neigh_mean <- mean(temp[pov_neigh[[x]], i], na.rm = T)
          
          # replace this particular cell NA with mean of neighoburs
          # data[rok==y & teryt==x, ..i] <- neigh_mean
          data[data$rok==y & data$teryt==x, i] <- round(neigh_mean,2)
          
          # add log
          na_replaced <- rbind(na_replaced, c(y, i, x, neigh_mean))}#)
      }
    }
  }
  
  # create replacement dasta
  na_replaced <- as.data.frame(na_replaced)
  names(na_replaced) <- c("rok", "kolumna", "teryt", "wartosc_zam")
  
  # prepare result object, clean data and df with info of replacement 
  results <- list(data=data, na_replaced=na_replaced)
  return(results)
  }


data06_18_contig_na_fill <- fill_by_neigh_mean(data06_18,              # data frame with data
                              years = 2006:2018,       # scope of years
                              rep_treshold = 66,       # up to how many missing poviats to fill
                              pov_neigh,              # cont.listw
                              # pov,                     # spatial object
                              missing_df=NULL
                              )

dim(data06_18_contig_na_fill$data)
# save a file
saveRDS(data06_18_contig_na_fill$data, "./data/data06_18_contig_na_fill.RDS")

clean_data <- results$data
dim(clean_data)

na_replaced <- results$na_replaced
dim(na_replaced)


# check again NAs
missings3 = as.data.frame(matrix(NA, ncol=max(clean_data$rok)-min(clean_data$rok)+1, nrow=ncol(clean_data)))

j=1
# add number of missing values in each year for each variable 
for(i in seq(min(clean_data$rok), max(clean_data$rok),1)){
  # number of missing values in given year and variable
  col  = sapply(clean_data[clean_data$rok==i,], function(x) sum(is.na(x)))
  # input to right cell
  missings3[,j] = col
  j=j+1
}

# name of columns as year
colnames(missings3) = seq(min(clean_data$rok), max(clean_data$rok), 1)

# give NA column names
rownames(missings3) <- colnames(clean_data)

# compare if it worked
View(missings3)
View(missings2)


#------------------------ here repacemnt without fucntion ------------------------

# zastapione <- c() 
# 
# for(i in names(data06_18_)){
#   for(y in 2006:2018){
#     # check if for given year and variable there were more than 0 and less than 100 pov data missing
#     if(missings_t[rok_2==y, ..i] < 67 & missings_t[rok_2==y, ..i] > 0){ # missings_t[rok_2==y, ..i] == 20){ #
#       # subest from data, chuk of this year with this variable and teryts
#       temp <- data06_18_[data06_18_$rok==y, c(i, "teryt")]
#       
#       # pick teryts of rows where there's NA for this column and year
#       missing_teryts <- temp[!complete.cases(temp), "teryt"]
#       
#       # for those teryts 
#       # lapply(missing_teryts, function(x){
#       for(x in missing_teryts){
#         # compute mean value of this column of it's neighours, dropping NA
#         neigh_mean <- mean(temp[pov_neigh[[x]], i], na.rm = T)
#         # replace this particular cell NA with mean of neighoburs
#         # data06_18_[rok==y & teryt==x, ..i] <- neigh_mean
#         data06_18_[data06_18_$rok==y & data06_18_$teryt==x, i] <- neigh_mean
#         zastapione <- rbind(zastapione, c(y, i, x, neigh_mean))
#         
#       }
#       #)
#     }
#   }
# }
