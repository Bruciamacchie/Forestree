library(ForestTools)
library(sf)
library(readxl)
library(tidyverse)

####################### DONNEES ###################
ser <- st_read("/Users/maxbruciamacchie/Dropbox/Packages/CoursR/Data/ser100union.shp", quiet=T) %>%
  ms_simplify(keep = 0.05) %>%
  dplyr::select(geometry)


usethis::use_data(ser, overwrite = T)
usethis::use_data(cad, overwrite = T)

# ------ Correspondance cadastre -----------------
rep <- "/Users/bruciamacchiemax/Desktop/Packages/ForestTools/Exemples"
parCadast <- st_read(dsn=rep, layer="ParcellesCadastrales", quiet = T)
parFor    <- st_read(dsn=rep, layer="ParcellesForestieres", quiet = T)
st_write(parFor, dsn=rep,layer ="ParcellesForestieres", update=TRUE, delete_layer = TRUE, driver = "ESRI Shapefile")

devtools::use_data(parCadast, parFor, overwrite = TRUE)

# ------ Liens internet -----------------
liens <- read_excel("~/Desktop/Packages/ForestTools/Exemples/Liens.xlsx")
perim <- st_read(dsn="~/Desktop/Packages/GF/Forets/PNRVN/Data/SIG",
                 layer="Perimetre", quiet=T)
devtools::use_data(liens, perim, overwrite = TRUE)

# ------ Creation parcellaire cadastral
rep <- "~/Documents/Cours/AgroParisTech/APT3/ProjetsR/2018/Sujet8/Rendu/Sujet 8 PSG dynamique/2 DONNEES/HTML"
MatriceCad <- read_csv(paste(rep,"MatriceCad.csv",sep="/"))
Parcellaire <- st_read(dsn = "~/Desktop/Packages/ForestTools/Exemples", layer = "PARCELLAIRE")
# st_write(shp, dsn="~/Desktop/Packages/ForestTools/Exemples",layer ="PARCELLAIRE",
         # update=TRUE, delete_layer = TRUE, driver = "ESRI Shapefile")
devtools::use_data(Parcellaire, MatriceCad, overwrite = TRUE)


# ------ Erreur tarifs -----------------
library(readxl)
Echan <- read_excel("/Users/bruciamacchiemax/Desktop/Packages/ForestTools/data/ErreurSch.xlsx", sheet="Echan")
Lot   <- read_excel("/Users/bruciamacchiemax/Desktop/Packages/ForestTools/data/ErreurSch.xlsx", sheet="Lot")
devtools::use_data(Echan, Lot, overwrite = TRUE)

# ------ Météo -----------------
data("DataMeteo")
##### Visualisation
head(DataMeteo)
##### Utilisation fonction
ClimatOmbroMF(DataMeteo)

# ------ IFN -----------------
IFNdata(2011,2016)

data(perim)
load("IFNdata.Rdata")
res <- IFNacctD(file=perim, TailleBuffer=500)
res$tab
res$Effectif
res$Graph

data(perim)
load("IFNdata.Rdata")
res <- IFNdendro(perim)
res$tab        # Répartition de la surface terrière
res$Nb         # nombre de placettes IFN
res$perimetre  # périmètre retenu
res$placettes  # placettes IFN

# ------ MNT -----------------
r <- raster("/Volumes/Samsung_T5/GeoData/MNT/Mnt75.tif")
data(perim)
perim <- as(perim, 'Spatial')
r1 <- crop(r, perim)
mnt <- mask(r1, perim)
devtools::use_data(mnt, overwrite = TRUE)


# ------ Découpage -----------------
bioreg <- st_read(dsn="~/Desktop/Packages/ForestTools/Exemples", layer="zpifn250", quiet=T)
rfifn  <- st_read(dsn="~/Desktop/Packages/ForestTools/Exemples", layer="rfifn250_l93", quiet=T)
rfifn <- rfifn %>%
  filter(!is.na(DEP))
devtools::use_data(bioreg, rfifn, overwrite = TRUE)

rn <- DecoupRN(formatSf = T, dept = F)
class(rn)
rn <- DecoupRN(formatSf = F, dept = F)
class(rn)

####################### MNH ##################
# library(ForestTools)
library(raster)
library(sf)
library(tidyverse)
library(rgeos)

# ------ Import
rep <- "~/Desktop/Packages/AnalysesAFI/Lidar/MNS"
r1 <- raster(paste(rep,"MNS_FD51_843_6871.TIF", sep="/"))
r2 <- raster(paste(rep,"MNS_FD51_843_6872.TIF", sep="/"))
mns <- merge(r1, r2)
mnt <- raster(paste(rep,"RGEALTI_FXX_0840_6875_MNT_LAMB93_IGN69.asc", sep="/"))
crs(mns) <- CRS('+init=EPSG:2154')
crs(mnt) <- CRS('+init=EPSG:2154')

repAFI <- "/Users/bruciamacchiemax/Desktop/Packages/AFI"
load(paste(repAFI,"Tables/AFIFichesTerrain.RData",sep="/"))
load(paste(repAFI,"Tables/AfiTablesElaborees.RData",sep="/"))
Dispo <- st_read(dsn=paste(repAFI,"SIG/Vecteurs",sep="/"), layer="Dispositif", quiet=T)
Placettes <- st_read(dsn=paste(repAFI,"SIG/Vecteurs",sep="/"), layer="Placettes", quiet=T)
Choix = 17 # Dispositif Belval
Parcelle <- Dispo %>% filter(Num==Choix)
Placettes <- Placettes %>%
  mutate(NumDisp = as.numeric(as.character(Num_dispo))) %>%
  rename(Placette = Num_plac) %>%
  filter(NumDisp==Choix) %>%
  dplyr::select(Placette)
# ------ Coordonnées placettes
Centres <-  as.data.frame(st_coordinates(Placettes)) %>%
  mutate(Placette = Placettes$Placette) %>%
  rename(CentreX = X,
         CentreY = Y)
# ------ Coordonnées arbres
arbres <- t %>%
  filter(NumDisp==Choix & Cycle ==4) %>%
  dplyr::select(Placette:Dh2) %>%
  mutate(X = Distance * sin(Azimut/200*pi),
         Y = Distance * cos(Azimut/200*pi)) %>%
  left_join(Centres, by = "Placette") %>%
  mutate(X = X + CentreX,
         Y = Y + CentreY)
locArbres <- st_as_sf(arbres, coords = c("X", "Y"))

tabGB <- afiPlaCat %>%
  filter(NumDisp == Choix & Cycle == 4) %>%
  filter(Cat =="GB") %>%
  dplyr::select(Placette, Gha) %>%
  rename(GhaGB = Gha,
         NumPlac = Placette)
Correspond <- data.frame(ID = 1:dim(Placettes)[1],
                         NumPlac = Placettes$Placette)
# --------- Extraction valeurs
Distance = 30
tab <- data.frame()
placB <- st_buffer(Placettes, dist=Distance)

for (i in 1:dim(placB)[1]) {
  shp <- as(placB[i,], 'Spatial')
  r1 <- crop(mnh, shp, snap="out")
  r1[is.na(r1)] <- 0
  for (j in c(0, -15, -20)) {
    shp1 <- gBuffer(shp, width=j)
    shp1 <- spTransform(shp1, crs(r1))
    r <- mask(r1, shp1)
    v <- raster::extract(r1, shp1, df=T, cellnumbers=TRUE)
    v <- v %>%
      mutate(Cercle = as.character(Distance + j),
             ID = i)
    tab <- rbind(tab, v)
  }
  print(paste("Placette n°", i))
}
tab <- tab %>% left_join(Correspond, by="ID")
# --------- Graphiques ----------
t2 <- tab %>%
  left_join(tabGB, by = "NumPlac") %>%
  mutate(ClasseMNH = cut(layer, breaks=c(0,20,22.5,25,27.5,40),
                         labels = c("inf20","inf22.5","inf25","inf27.5","inf40"),
                         include.lowest =T),
         GhaGB = ifelse(is.na(GhaGB), 0, GhaGB)) %>%
  group_by(Cercle,ClasseMNH, NumPlac) %>%
  summarise(Surf = n())

t0 <- t2 %>%
  group_by(NumPlac) %>%
  summarise(Tot = sum(Surf))
t2 <- t2 %>%
  left_join(t0, by = "NumPlac") %>%
  mutate(Surf = Surf / Tot)
t4 <- t2 %>%
  spread(key = ClasseMNH, Surf, fill=0) %>%
  left_join(tabGB, by = "NumPlac") %>%
  mutate(GhaGB = ifelse(is.na(GhaGB), 0, GhaGB))
t10 <- tab %>%
  filter(Cercle=="10") %>%
  group_by(NumPlac) %>%
  summarise(Moy10 = mean(layer))
t30 <- tab %>%
  filter(Cercle=="30") %>%
  filter(layer > 20) %>%
  group_by(NumPlac) %>%
  summarise(Surf30 = n())
t5 <- t30 %>%
  left_join(t0, by = "NumPlac") %>%
  mutate(Surf30 = Surf30 / Tot) %>%
  left_join(t10, by = "NumPlac") %>%
  left_join(tabGB, by = "NumPlac") %>%
  mutate(Pop = ifelse(Moy10 < 24, "1", "2"))


zone <- st_buffer(Parcelle, dist=50)
mns <- crop(mns, as(zone, "Spatial"))
mnt <- crop(mnt, as(zone, "Spatial"))
devtools::use_data(mns, mnt, Parcelle, Placettes, overwrite = TRUE)
devtools::use_data(mnh, overwrite = TRUE)


par(mar = rep(0.5, 4))
plot(mnh, axes=F, box=F)
plot(st_geometry(Parcelle), add=T)


razel <- st_read(dsn="~/Desktop/Packages/GF/Forets/Razel/Data/SIG",
                 layer="Perimetre", quiet=T)
devtools::use_data(razel, overwrite = TRUE)

# ------------- Exploitation en montagne -------------
rep = "/Users/maxbruciamacchie/Dropbox/Cours/APT3/MicroProjetsR/2018/Micro1"
piste <- st_read(paste(rep,"Projet.shp",sep="/"), quiet=T)
mntPiste <- raster(paste(rep,"MNT.tif",sep="/"))
devtools::use_data(piste, overwrite = TRUE)
devtools::use_data(mntPiste, overwrite = TRUE)

data(mntPiste)
plot(mntPiste )
plot(piste, add=T)
BufferDis(piste, mntPiste)

line = piste
r = mntPiste
line1 = st_cast(line, "MULTILINESTRING")
st_length(line1)

ggplot(pts, aes(x=distcum, y=Pente)) + geom_line()



