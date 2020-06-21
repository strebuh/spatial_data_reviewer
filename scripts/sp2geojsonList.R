# library(sp)
# library(sf)
# library(rgdal)
library(geojsonio)

sp2geojsonList <- function(spObject){
  
  # convert to sf
  sfObject <- st_as_sf(spObject)
  
  # transform
  sfObject <- st_transform(sfObject, 3857)
  
  # get higcharter accepted geojson_list
  JSONlistObject <- geojson_list(sfObject)
  
  return(JSONlistObject)
}
