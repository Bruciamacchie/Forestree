library(sf) # lecture vecteurs
library(sp)
library(raster)
library(velox) #accelere extract
library(tidyverse)

################### Import #################
setwd("P:/Projet route")
desserte <- st_read("Desserte_Veyton.shp", quiet=T)
mnt <- raster("MNT.tif")
crs(mnt)=CRS("+init=epsg:2154")

################### Carte ###################
plot(mnt)
#image(mnt)
plot(st_geometry(desserte), add=T)


################### Calculs #################
pas=50
route <- desserte %>% filter(nom=="Le Veyton")
nb <- as.integer(round(st_length(route)/pas,0)) # nombre de points
zone <- st_buffer(route, dist=100)
mnt1 <- crop(mnt, as(zone, "Spatial"))
pts <- st_line_sample(route, nb) %>%
  st_cast("POINT") %>%
  st_sf()

plot(mnt1)
plot(st_geometry(route), add=T)
plot(st_geometry(pts), add=T, col='red', pch=3)

# ----------- Pentes
profil <- data.frame(Point = 1:dim(pts)[1],
                    Dist = (1:dim(pts)[1])*pas,
                    Alti = raster::extract(mnt1, as(pts, "Spatial")))

profil <- profil %>%
  mutate(Apres = lead(Alti),
         Avant = lag(Alti),
         PenteAp = (Apres - Alti)/nb,
         PenteAv = lead(PenteAp),
         Pente = (PenteAp + PenteAv)/2)

PenteForte <- profil %>%
  filter(Pente > 0.07)

ggplot(profil, aes(x=Dist, y=Alti)) +
  geom_line() +
  geom_point(data=PenteForte, aes(x=Dist, y=Alti), color='red')


# ----------- Zones exploitables
slope<- terrain(mnt1,opt = "slope", unit='tangent')
plat<- slope
slope[slope>0.3] <- NA
plot(slope)
plot(st_geometry(route), add=T)
plot(st_geometry(pts), add=T, col='red', pch=3)


##-----------Buffer asym?trique

# 1?re ?tape : s?parer les x des y
coord_rte <- as.data.frame(st_coordinates(pts$geometry))

##initialisation des Dx et Dy :
coord_rte <- coord_rte %>%
  mutate(alti=profil$Alti,
         Dx = lead(X) - lag(X),
         Dy = lead(Y) - lag(Y),
         Xg = X - 3/5 * Dy,
         Yg = Y + 3/5 * Dx,
         Xd = X + 3/5 * Dy,
         Yd = Y - 3/5 * Dx)

ggplot(coord_rte,aes(x=Xg,y=Yg))+
  geom_point(data=coord_rte, aes(x=Xg, y=Yg), color='red')+
  geom_point(data=coord_rte, aes(x=X, y=Y), color='black')+
  geom_point(data=coord_rte, aes(x=Xd, y=Yd), color='blue')


##Transformation en coord et r?cup?ration des altitudes
coord_rte[is.na(coord_rte)] <- 0

alti_G <- SpatialPointsDataFrame(coords = coord_rte[,c("Xg","Yg")],data=coord_rte, proj4string = CRS("+init=epsg:2154"))
coord_rte$alti_G <- raster::extract(x = mnt1, y = alti_G[,c("Xg","Yg")])

alti_D <- SpatialPointsDataFrame(coords = coord_rte[,c("Xd","Yd")],data=coord_rte, proj4string = CRS("+init=epsg:2154"))
coord_rte$alti_D <- raster::extract(x = mnt1, y = alti_D[,c("Xd","Yd")])

##Calcul des pentes
coord_rte <- coord_rte %>%
  mutate(Pente_G = (alti_G - alti)*10,
         Pente_D = (alti_D - alti)*10)

coord_rte[is.na(coord_rte)] <- 0

#####---------CREATION DU BUFFER ASYMETRIQUE-------------

#1 - Colonnes des distances de part et d'autre : 150m si ?a descend, 50 si ?a monte.
coord_rte <- mutate(coord_rte, buff_G = as.numeric(coord_rte$Pente_G > 0)*50 + as.numeric(coord_rte$Pente_G < 0)*150)
coord_rte <- mutate(coord_rte, buff_D = as.numeric(coord_rte$Pente_D > 0)*50 + as.numeric(coord_rte$Pente_D < 0)*150)


#2 - Calcul des coordonn?es des points plac?s ? 150 ou 50m de part et d'autre de la piste :
coord_rte$Dx=coord_rte$Dx/pas
coord_rte$Dy=coord_rte$Dy/pas

coord_rte$X_buff_G = coord_rte$X - coord_rte$buff_G * coord_rte$Dy
coord_rte$Y_buff_G = coord_rte$Y + coord_rte$buff_G * coord_rte$Dx
coord_rte$X_buff_D = coord_rte$X + coord_rte$buff_D * coord_rte$Dy
coord_rte$Y_buff_D = coord_rte$Y - coord_rte$buff_D * coord_rte$Dx


#3 - Tra?? d'un polygone reliant tous les points ainsi plac?s.
SP_Buff_G <- SpatialPointsDataFrame(coords = coord_rte[,c("X_buff_G","Y_buff_G")],data=coord_rte, proj4string = CRS("+init=epsg:2154"))

SP_Buff_D <- SpatialPointsDataFrame(coords = coord_rte[,c("X_buff_D","Y_buff_D")],data=coord_rte, proj4string = CRS("+init=epsg:2154"))

#Buffer_gauche <- polygon(x=coord_rte$X_buff_G , y=coord_rte$Y_buff_G)
#Buffer_droite <- polygon(x=coord_rte$X_buff_D , y=coord_rte$Y_buff_D)

plot(mnt1)
plot(st_geometry(route), add=T)
plot(SP_Buff_D, col="red", add=T)
plot(SP_Buff_G, col="blue", add=T)

#--------------------FIN BUFFER---------------------------------

st_buffer(route, dist=150)
plot(mnt1)


zone <- st_buffer(desserte, dist=100)
crs(mnt) <- CRS('+init=EPSG:2154')
mnt1 <- crop(mnt, as(zone, "Spatial"))
pente <- terrain(mnt1, opt='slope', unit='tangent')
pente[pente > 0.3] <- NA
emprise <- st_buffer(desserte, dist=2.5) %>%
  st_union() %>%
  st_sf()
plat <- rasterToPolygons(pente, n=8, dissolve=F) %>%
  st_as_sf()  %>%
  st_union() %>%
  st_cast("POLYGON")

plat$emprise <- st_intersects(plat, emprise, sparse = F)

plat1 <- plat[plat$emprise] %>%
  st_buffer(0.5) %>%
  st_union() %>% # unite to a geometry object
  st_sf()


plot(mnt1)
plot(st_geometry(desserte), add=T, color='blue')
plot(st_geometry(plat1), add=T, col='red', border="red")

##########################################limites

limits<-st_cast(plat1,to='MULTILINESTRING') %>%
  st_cast(to='LINESTRING') %>%
  st_sf()

plot(limits)

pas=50
nblim <- as.integer(round(st_length(limits)/pas,0)) # nombre de points
zone <- st_buffer(limits, dist=100)
mnt1 <- crop(mnt, as(zone, "Spatial"))
ptslim <- st_line_sample(limits, nblim) %>%
  st_cast("POINT") %>%
  st_sf()

plot(mnt1)
plot(st_geometry(limits), add=T)
plot(st_geometry(ptslim), add=T, col='red', pch=3)


##########################################

