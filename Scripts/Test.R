library(raster)
library(sf)
library(Forestree)
library(tidyverse)
library(readxl)
library(rasterVis)

# ----------- Import ---------
Limites <- st_read("/Users/maxbruciamacchie/Downloads/Archive/CT.perimetreext.shp")
zone <- st_buffer(Limites, dist=1000)
ign <- raster("/Users/maxbruciamacchie/Downloads/Archive/MNT.tif")
crs(ign) <- CRS('+init=EPSG:2154')
ign <- crop(ign, as(zone,"Spatial"))
pts <- read_excel("/Users/maxbruciamacchie/Downloads/Archive/PTS.xlsx")
pts <- st_as_sf(pts, coords = c("X", "Y"), crs = 2154) %>%
  st_cast("POINT")

aws <- SiteMnt(zone, buffer=0)

# ------------- Harmonisation ----------
ign1 <- resample(ign, aws)
aws1 <- resample(aws, ign)
s <- stack(ign, aws1)
names(s) <- c("Ign", "Aws")
par(mar=c(0.5,0.5,1,0.5))

gplot(s) + geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  coord_equal() +
  scale_fill_gradientn(colours = terrain.colors(10)) +
  theme_void() + labs(x="", y="")

# ------------- Référence ----------
tab <- raster::extract(s, as(pts, "Spatial"), method='bilinear')
t1 <- pts
st_geometry(t1) <- NULL
tab <- cbind(t1, tab) %>%
  gather(Source, value, -Type, -Num, -Ref)
ggplot(tab, aes(x=Ref, y=value, color=Source, shape=factor(Type))) +
  geom_abline(intercept = 0, slope = 1, color="grey") +
  geom_point() +
  theme_bw() + labs(x="Référence", y="Altitude")
# ------------- Différence ----------
r1 <- ign1 - aws
r2 <- ign - aws1
t1 <- data.frame(y=values(r1), x="ign")
t2 <- data.frame(y=values(r1), x="aws")
tab <- rbind(t1,t2)
ggplot(tab, aes(x, y)) + geom_violin() + theme_bw()

# ------------- Sauvegarde ----------
st_write(pts, "PtsIgn.shp", driver="ESRI Shapefile",update=T, delete_layer=T, quiet =T)
writeRaster(r2, "DiffIgnAws.tif", overwrite=TRUE)
writeRaster(aws, "Aws.tif", overwrite=TRUE)

# ------------- dessin ----------
plot(ign)
plot(st_geometry(Limites), add=T)
plot(aws)
plot(st_geometry(Limites), add=T)
plot(r)
plot(st_geometry(Limites), add=T)
