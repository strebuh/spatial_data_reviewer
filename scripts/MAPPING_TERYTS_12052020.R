correct_teryts= function(data18){
  #data18 = data_2018_clean
  data17 = read.csv('data_nts4_2019.csv',sep=';',dec=',')
  
  # correct the teryt in datas
  data18$teryt <- str_sub(ifelse(nchar(data18$Kod)==6,paste0('0', data18$Kod), data18$Kod),1,4)
  data17$teryt <- str_sub(ifelse(nchar(data17$code_GUS)==6,paste0('0', data17$code_GUS), data17$code_GUS),1,4)
  
  #setDT(data18)
  #setDT(data17)
  
  names(pov@data)
  # [1] "iip_przest" "iip_identy" "iip_wersja" "jpt_sjr_ko" "jpt_kod_je" "jpt_nazwa_" "jpt_nazw01" "jpt_organ_" "jpt_orga01" "jpt_jor_id"
  # [11] "wazny_od"   "wazny_do"   "jpt_wazna_" "wersja_od"  "wersja_do"  "jpt_powier" "jpt_kj_iip" "jpt_kj_i01" "jpt_kj_i02" "jpt_kod_01"
  # [21] "id_bufora_" "id_bufor01" "id_technic" "jpt_opis"   "jpt_sps_ko" "gra_ids"    "status_obi" "opis_bledu" "typ_bledu" 
  
  # wont work due to different lenths
  # compare_order <- data.frame(pov_spatial = pov@data$jpt_kod_je,
  #                             pov_class = data17$teryt,
  #                             pov_our = data18_$teryt)
  
  length(intersect(pov@data$jpt_kod_je,data18$teryt))
  length(intersect(pov@data$jpt_kod_je,data17$teryt))
  
  compare_class17 <- as.character(data17$poviat_name1) #data17[,c("poviat_name1","teryt")]
  class(compare_class17)
  #length(compare_class17) <- 382 
  
  compare_pov <- as.character(pov@data$jpt_nazwa_)
  #length(compare_pov) <- 382 
  
  # comparison
  compare <- cbind(as.character(data18$Nazwa), compare_class17,compare_pov)
  head(compare)
  
  # poviats in data18 that are not in pov spatial object
  setdiff(data18$teryt, pov@data$jpt_kod_je)
  # [1] "0263" "1431"
  
  # remove those two retyt rows
  #data18 <- data18[!(teryt %in% setdiff(data18$teryt, pov@data$jpt_kod_je))]
  
  # data.table with to get teryt order
  pov_order_dt <- as.data.frame(pov@data[,match("jpt_kod_je",colnames(pov@data))])
  colnames(pov_order_dt) = c('jpt_kod_je')
  
  # reorder data18 according to pov data
  data18 <- merge(pov_order_dt, data18, by.x = "jpt_kod_je", by.y = "teryt", all.x=T, sort = FALSE)
  
  return(data18)  
}

