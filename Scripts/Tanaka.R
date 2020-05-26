library(tidyverse)
library(tanaka)
library(elevatr)
library(sf)
library(Forestree)
library(raster)

# -------- import
com <- st_read("/Users/maxbruciamacchie/CloudStation/SIG/perimetre/CT.perimetreext.shp", quiet = TRUE) %>%
  st_transform(2154)
r1 = SiteMnt(com)
r2 = raster("/Users/maxbruciamacchie/CloudStation/SIG/Origine/MNT/MNT.tif")
crs(r1) = CRS('+init=EPSG:2154')
crs(r2) = CRS('+init=EPSG:2154')

# creer une palette de couleur
pal <- colorRampPalette(colors = c("#F9D3A1", "#1E315B"))

# --------- Afficher
par(mar = c(0,0,0,0))
par(mfrow = c(1,2))
bas = floor(r2@data@min/10)*10
haut = floor(r2@data@max/10)*10
tanaka(r1, mask = com, breaks = seq(bas,haut,10), col = pal((haut-bas)/20), legend.pos="n")
tanaka(r2, mask = com, breaks = seq(bas,haut,10), col = pal((haut-bas)/20), legend.pos="n")

