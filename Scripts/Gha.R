
library(sf)
library(tidyverse)
library(raster)



perim <- st_read("~/pCloud Sync/EnCours/Pichery/Cirey/Foncier/Contour.shp") %>% 
  dplyr::select(geometry)
mnh <- raster("~/pCloud Sync/EnCours/Pichery/Cirey/mnh.tif")
crs(mnh) <- CRS('+init=EPSG:2154')

EstimGha <- function(shp, mnh, seuilH = 15, resout = 50, Choup=500) {
  zone <- st_buffer(shp, dist=resout)
  resol <- res(mnh)
  if (resol[1] < 5) {
    mnh <- aggregate(mnh, fact=c(5/resol[1], 5/resol[2]), fun=mean)
  }
  mnh <- raster::mask(mnh, as(zone, "Spatial"))
  print("Fin aggrégation et découpe du MNH ........")
  
  pts <- rasterToPoints(mnh, fun=function(x){x>seuilH}, spatial=T) %>% 
    st_as_sf() %>% 
    st_transform(2154)
  print("Fin extraction des pixels ........")
  
  grd <- st_make_grid(zone, cellsize=c(resout,resout)) %>% 
    st_sf()
  print("Fin création du grid ........")
  
  Nb = unlist(c(lapply(st_intersects(grd, pts), length)))
  ech <- resout^2/5^2
  grd$Gha <- Nb/ech*10000/Choup
  
  # grd <- st_cast(grd, "POLYGON")
  grd <- st_intersection(grd, shp)
  return(grd)
}

Gha <- EstimGha(perim, mnh)
plot(Gha)
st_write(Gha, "Gha.shp")

