############################ Exemples ###########################

############################ Tarifs de cubage #####################
# -------- Fonction TarifSch
Types = c("SchR","SchR","SchL")
Types2 = c("SchR","Sch","SchL")
Nums = c(9,8,9)
Diams = c(45,45,50)
TarifSch(Types,Nums,Diams)
TarifSch(Types2,Nums,Diams)

# -------- Fonction TarifFindSch
## Exemple1
library(tcltk)
library(readxl)
file <- tk_choose.files(caption = "Choix du fichier contenant volumes",
                        filters=matrix(c(".xlsx",".xlsx"),1,2, byrow = T))
if (length(file) > 0) {
  tab <- read_excel(file, sheet = 1)
  if (sum(c("Essence","Vol","Diam") %in% names(tab)) == 3) {
    res <- TarifFindSch(tab)
  } else {print("Le fichier doit contenir les colonnes Essence, Vol et Diam")}
} else {print("Recherche tarifs annulée")}


## Exemple2
data("Vol") # donnée présente dans le package Forestree
if (sum(c("Essence","Vol","Diam") %in% names(Vol)) == 3) {
  res <- TarifFindSch(Vol)
  } else {print("Le fichier doit au moins contenir les 3 colonnes Essence, Vol et Diam")}

####### Analyse des résultats pour les 2 cas
res$tab
res$graph

# -------- Fonction TarifErreurSch
library(dplyr)
data(Echan)
data(Lot)
TarifErreurSch(Echan,Lot)

# -------- Fonction TarifChaude
Hauts <- c(13,14,14)
Diams = c(45,45,50)
TarifChaude(13, Diams, Hauts) # cas d'un Chaudé n°13

# -------- Fonction TarifSch2
Hauts <- c(13,14,14)
Diams = c(45,45,50)
TarifSch2(2, Diams, Hauts) # cas d'un Schaeffer2 n°2


############################ Données IFN #####################



############################ Rayonnement #####################
library(sf)
library(tidyverse)
library(raster)
library(insol)

data(razel)
r <- SiteMnt(razel)
rad <- Rayonnement(razel, r)

plot(rad, axes=F, box=F)
plot(st_geometry(razel), add=T)
# writeRaster(rad, "Rad.tif", format="GTiff", overwrite=TRUE)

############################ CourseSoleil #####################
library(tidyverse)
library(suncalc)

res <- CourseSoleil(Lat = 44, Lon = 1.8)
res$graph


############################ ValeurVenale #####################
library(tidyverse)
library(Forestree)
library(sf)
library(rmapshaper)

Evol <- data.frame()
Detail <- data.frame()
for (i in c(2018:2016)) {
  res  <- ValeurVenale(i)
  Evol <- rbind(Evol, as.data.frame(res$tab))
  Detail <- rbind(Detail, as.data.frame(res$Detail))
}

textes <- Evol %>% filter(Année == 2018)
ggplot(Evol, aes(x=Nature, y=Prix, color=factor(Année))) +
  geom_point() + geom_text(data=textes, aes(label=round(Prix,0)), size=4, hjust=1.2) +
  theme_bw() + labs(color="")

ggplot(Detail, aes(x=Nature, y=Prix, fill=factor(Année), color=factor(Année))) +
  geom_point(position = position_jitterdodge()) +
  geom_boxplot(alpha=0.4) +
  geom_text(data=textes, aes(label=round(Prix,0)), size=3, hjust=-1.8) +
  theme_bw() + labs(fill="", color="", y="Prix (euro/ha)")

communes <- st_read("/Users/maxbruciamacchie/Downloads/n-com-fla-000/n_com_fla_000.shp") %>%
  dplyr::select(insee_comm, nom_commun) %>%
  st_centroid() %>%
  mutate(CodePos = as.numeric(as.character(insee_comm))) %>%
  st_transform(2154)

france <- st_read("~/pCloud Drive/Bureau/GeoData/Limites/Administratif/France25.shp") %>%
  st_transform(2154) %>%
  ms_simplify(keep = 0.01) %>%
  dplyr::select(geometry)


test <- Detail %>%
  filter(Nature == "L") %>%
  left_join(communes, by = "CodePos")

ggplot() +
  geom_sf(data=ser, color='grey', fill="white") +
  geom_sf(data=test, aes(size=Prix/10000, color=factor(Année)), alpha=0.5) +
  theme_bw() + labs(color="") + guides(size=F)

data(ser)
plot(ser)
