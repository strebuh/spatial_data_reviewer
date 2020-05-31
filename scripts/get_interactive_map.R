library(data.table)
library(stringr)
library(highcharter)
library(RColorBrewer)
library(classInt)
# remove.packages("highcharter")


get_interactive_map <- function(
  plot_data,                                   # data frame with data, each row one polygon equivalent
  map_json,                                   # json file holding a map, to be ploted (.json)
  # mapline_json_list = NULL,                          # json obszarow zabezpieczenia, jezeli nie podane, nie ryzuje wartosci konturow dodatkowych
  zmienna_mapowana,                             # index zmiennej do mapowania 
  joining_var = "jpt_kod_je",
  ilosc_grup,                                   # na ile pobucketowac
  # lista_winietek,                                # zmienne na hover klucz lista[nazwa_zmiennej] <- "teskt na hover"
  tytul = " ",                                  # tytul do wykresu 
  etykiety_obszarow = FALSE,                    # czy pokazac nazwy obszarow
  # kolor_granic_map = "",                        # kolor granic obszarow podstawowych (map)
  # kolor_granic_mapline = "black",               # kolor granic obszarow dodatkowych (mapline)
  tryb_podzialu = "hclust",                     # hclust, kmeans, sd
  paleta_kolorow = "BuPu",                        # nazwa palety do mapowania, albo wektor kolorow o ilosci kolorow rownej zmiennej ilosc_group
  reverse_palette=FALSE,                        # odwroc kolejnosc nasycenia kolorow palety
  # zmienna_punkty = NULL,                      # zmienna wyznaczajaca punkty
  nazwa_oddzialow = "Oddziały neurologiczne"    # etykieta
){
  
  # --------------------------------------------------------------------
  
  # informacja o zmiennej ktora jest mapowana
  message(paste0("Zmienna mapowana to: ", names(plot_data)[zmienna_mapowana]))
  
  
  # przygotuj tekst winietek (hover)
  # if(is.character(lista_winietek)){
  #   # jezeli teksty sas wjuz w odpowiedzniej formie, zmien tylko nazwe zmiennej mapowanej na value
  #   text_na_hover <- lista_winietek
  #   names(plot_data)[zmienna_mapowana] <- "value" 
  # } else {
    # # zmien nazwe zmiennej do mapowania na value w winietkach
    # names(lista_winietek)[names(lista_winietek)==names(plot_data)[zmienna_mapowana]] <- 'value'
    # # zmien w danych nazwe zmiennej mapowanej
    org_name <- names(plot_data)[zmienna_mapowana]  
    names(plot_data)[zmienna_mapowana] <- "value" 
    # # wybierz zmienne do hovera (pozostale)
    # zmienne_na_hover <- intersect(names(lista_winietek), names(plot_data))
    # przygotuj jeden tekst na hover
    # text_na_hover <- paste0(
    #   sapply(zmienne_na_hover, function(x){
    #     paste0(lista_winietek[x],": {point.", x,"}")}),
    #   collapse = " <br> ")
    text_na_hover <- paste0("{point.Nazwa}<br>", org_name, ": {point.value}")
    # }
  
  # --------------------------------------------------------------------
  
  # przgotuj zmienna do mapowania
  zmienna <- plot_data$value

  # --------------------------------------------------------------------
  

  
  # przygotuj kolory dla wartosci zmiennej
  # jezeli 1 element, to nazwa palety, wybierze z niej ilosc kolorow
  if(length(paleta_kolorow)==1){
    tryCatch(kolory <- brewer.pal(ilosc_grup, paleta_kolorow),
             finally = {
               kolory <- colorRampPalette(brewer.pal(ilosc_grup, paleta_kolorow))(ilosc_grup) 
             }
             )
    }
      
  # } else if(length(paleta_kolorow)!=1 | length(paleta_kolorow)!=ilosc_grup){
  #   # jezeli dlugosc wektora to nie 1 i nie ilosc grup, to zle podana paleta
  #   warning("Podana ilość kolorów nie odpowiada ilości grup, albo paleta nie ma tylu kolorów.")
  # } # czy odwrocic nasycenie kolorow
  if(reverse_palette==TRUE){
    kolory <- rev(kolory)
  }
  # podzial na przedzialy wg wybranego trybu podzialu
  klasy <- classIntervals(zmienna, ilosc_grup, style=tryb_podzialu)
  # tabela z przedzialami i kolorami
  tabela.kolorow <- findColours(klasy, kolory)
  
  # --------------------------------------------------------------------
  
  # zasiegi przedzialow wyznaczone przez findColours
  zakresy <- names(attr(tabela.kolorow, "table"))
  
  # wytnij dolne i gorne granice przedzialow z zasiegow przedzialow
  od <- str_replace_all(zakresy, "\\[|,(-)?\\d+(\\.\\d+)?(\\)|\\])", "")
  do <- str_replace_all(zakresy, "\\[|(-)?\\d+(\\.\\d+)?,|(\\)|\\])", "")
  
  # tabela od,do,kolor
  tabela.kolorow <- data.frame(from = as.numeric(od),
                               to = as.numeric(do),
                               color = kolory)
  tabela.kolorow_lista <- list_parse(tabela.kolorow)
  
  # # --------------------------------------------------------------------
  # # punkty szpitali
  # if(!is.null(zmienna_punkty)){
  #   baza <- plot_data[zmienna_punkty != 0,]
  #   orig_coords <- data.frame(lat=as.double(baza$latitude), lon=as.double(baza$longitude))
  #   
  #   coordinates(orig_coords) <- c('lon', 'lat')
  #   proj4string(orig_coords) <- CRS("+init=epsg:4326")
  #   
  #   # Zamiana lon/lat na EPSG:3857 - taka sama, jak projekcja w mapce
  #   Metric_coords <- spTransform(orig_coords, CRS("+init=epsg:3857"))
  #   
  #   # Stworzenie nowej bazy ze współrzędnymi i nazwą szpitala
  #   geo_oddz <- data.frame(lat=Metric_coords$lat, lon=Metric_coords$lon, name = baza$name, oddzial = baza$n_odzialow)
  #   
  #   # Stworzenie geojsona na podstawie nowej bazy
  #   map_geojson <-  geojsonio::geojson_json(geo_oddz, lat = "lat", lon = "lon")
  # }
  
  # --------------------------------------------------------------------
  mapka <- highchart(type= "map") %>%
    hc_add_series(mapData = map_json,
                  type = "map",
                  name = "",
                  data = plot_data,
                  joinBy = joining_var,
                  value="value",
                  # geojson = TRUE
                  showInLegend = TRUE,
                  # color = kolor_granic_map, # nie dziala tutaj, nie znalazlem jak wstawic
                  tooltip = list(pointFormat = text_na_hover)) %>%
    hc_colorAxis(dataClasses = tabela.kolorow_lista) %>%
    hc_title(text=tytul,
             fontWeight = "bold") %>%
    hc_mapNavigation(enabled = TRUE) 
    
  #   if(!is.null(zmienna_punkty)){
  #     mapka <-  hc_add_series(mapka, mapData = map_json,
  #                   type='mappoint',
  #                   dataLabels = list(enabled = etykiety_obszarow), # bez podpisow
  #                   showInLegend = FALSE,
  #                   data = map_geojson,
  #                   name = nazwa_oddzialow,
  #                   tooltip = list(pointFormat = "{point.name}"))
  #     
  #   }
  # 
  # if(!is.null(mapline_json_list)){
  #   mapka <-  hc_add_series(mapka, data = mapline_json_list, 
  #                           type = "mapline", 
  #                           color = kolor_granic_mapline,
  #                           name = "", 
  #                           geojson = TRUE)
  # }

  
  return(mapka)
}