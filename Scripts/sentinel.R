#------------------ Libraries ------------------
library(raster)
library(sf)
library(tidyverse)
library(velox)
library(readxl)
library(tools)
library(gam)
# library(stars)

#------------------ Functions ------------------
SentinelImport <- function(rep, shp, ext=200) {
  fichs <- list.files(rep, full.names =T, pattern="\\.tif$")
  noms <- substring(file_path_sans_ext(basename(fichs)), 54)
  if(all(noms %in% c("B11","B12","B2","B3","B4","B5","B6","B7","B8","B8A"))){
    e <- extent(as(shp, "Spatial")) + ext
    # la bande B2 sert de référence pour l'extension et la résolution
    r1 <- raster(fichs[3])
    r1 <- crop(r1, e)
    s <- stack(r1)
    # boucle sur les autres bandes
    for(i in fichs[-3]) {
      r <- raster(i)
      r <- crop(r, e)
      if (sum(res(r) == c(10,10)) < 2) {
        r <- resample(r, r1, method='bilinear')
      }
      s <- stack(s, r)
    }
    names(s) <- noms
    #------------------ Calcul des variables de synthèse ------------------
    SR           <- s$B8 / s$B4
    SAVI         <- 1.5*(s$B8 - s$B4)/(s$B8 + s$B4 + 0.5)
    S2REP        <- 705 + 35*(0.5*(s$B7 + s$B4) - s$B5)/(s$B6 - s$B5)
    RENDWI       <- (s$B3 - s$B5)/(s$B3 + s$B5)
    RedEdgeNDVI  <- (s$B8 - s$B6)/(s$B8 + s$B6)
    PSRI         <- (s$B4 - s$B2)/s$B5
    PSRINRI      <- (s$B4 - s$B2)/s$B8
    NDWI         <- (s$B3 - s$B8)/(s$B3 + s$B8)
    NDVI         <- (s$B8 -s$B4)/(s$B8 + s$B4)
    NDVIgreen    <- s$B3*NDVI
    NDVI705      <- (s$B6 - s$B5)/(s$B6 + s$B5)
    NDII         <- (s$B8 - s$B11)/(s$B8 + s$B11)
    NDI45        <- (s$B5 - s$B4)/(s$B5 + s$B4)
    NDBI         <- (s$B11 - s$B8)/(s$B11 + s$B8)
    NBRraw       <- (s$B8 - s$B12)/(s$B8 + s$B12)
    MTCI         <- (s$B6 - s$B5)/(s$B5 - s$B4)

    s1 <- stack(SR,SAVI,S2REP,RENDWI,RedEdgeNDVI,PSRI,PSRINRI,NDWI,NDVI,NDVIgreen,
                NDVI705,NDII,NDI45,NDBI,NBRraw,MTCI)
    names(s1) <- c("SR","SAVI","S2REP","RENDWI","RedEdgeNDVI","PSRI","PSRINRI","NDWI","NDVI","NDVIgreen",
                   "NDVI705","NDII","NDI45","NDBI","NBRraw","MTCI")
    # s <- stack(s, s1)
    s <- s1
    return(s)
  } else {
    print(paste("Le répertoire ne contient pas les rasters",noms))
  }
}

SentinelExtract <- function(s, shp) {
  t1 <- data.frame(Placette = shp$name)
  vx <- velox(s)
  mat <- vx$extract(as(shp, "Spatial"), fun=mean)
  tab <- as.data.frame((mat))
  names(tab) <- names(s)
  tab <- cbind(t1, tab)
  return(tab)
}

SentinelPredict <- function(df, var, model='lm', s, max=80) {
  df <- df %>%
    rename(y = !!names(.[match(var, names(df))])) %>%
    dplyr::select(y, names(s))
  if (model=='glm') {
    lm1 <- glm(y ~ ., data=df)
    lm1 <- glm(step(lm1)$call$formula, data=df)
    res <-summary(lm1)
  } else if (model=='gam') {
    lm1 <- gam(y ~ ., data=df)
    res <-summary(lm1)
  } else {
    lm1 <- lm(y ~ ., data=df)
    lm1 <- lm(step(lm1)$call$formula, data=df)
    res <-summary(lm1)
  }

  r <- predict(s, lm1, progress='text')
  r[r<0] <- 0
  r[r>max] <- max
  out <- list(r, res, lm1)
  names(out) <- c("raster", "summary", "model")
  return(out)
}

SentinelVerif <- function(r, shp) {
  t1 <- data.frame(Zone = shp$id)
  vx <- velox(r)
  mat <- vx$extract(as(shp, "Spatial"), fun=mean)
  tab <- as.data.frame((mat))
  names(tab) <- names(r)
  tab <- cbind(t1, tab)
  return(tab)
}

#------------------ Import ------------------
# Répertoire de travail
rep="~/Dropbox/Projets PSG/2018/SENTINEL2A_20170818-103421-569_L2A_T31TGL_D_V1-4/FRE"
# Périmètre de la forêt
perim <- st_read(dsn="~/Dropbox/Projets PSG/2018/Perimetre",
                 layer="CT.perimetreext", quiet=T) %>% st_transform(32631)
# Localisation des placettes
plac <- st_read(dsn="~/Dropbox/Projets PSG/2018/PointsTerrain", layer="PlacetteTerrain", quiet=T) %>%
  st_transform(32631) %>%
  st_buffer(dist=15)
# Données terrain
Placettes <- read_excel("~/Dropbox/Projets PSG/Placettes_fusionnees.xlsx", sheet="placettes")
Inv       <- read_excel("~/Dropbox/Projets PSG/Placettes_fusionnees.xlsx", sheet="inventaire")
Regroup   <- read_excel("~/Dropbox/Projets PSG/Placettes_fusionnees.xlsx", sheet="Regroup")

#------------------ Extractions données Sentinel ------------------
s   <- SentinelImport(rep, perim)
tab <- SentinelExtract(s, plac)

#------------------ Calculs placettes ------------------
Inv[is.na(Inv)] <- 0
Pla <- Inv %>%
  dplyr::select(-Total) %>%
  gather(Cat, Gha, -Placette, -Essences) %>%
  # filter(Gha !=0 ) %>%
  # filter(!is.na(Gha)) %>%
  group_by(Placette) %>%
  summarise(Gha = sum(Gha)) %>%
  left_join(tab, by = "Placette") %>%
  # filter(!is.na(B2)) %>%
  dplyr::select(Gha,SR:MTCI)

PlaEss <- Inv %>%
  dplyr::select(-Total) %>%
  left_join(Regroup, by = "Essences") %>%
  gather(Cat, Gha, -Placette, -Essences, -Regroup) %>%
  # filter(Gha !=0 ) %>%
  # filter(!is.na(Gha)) %>%
  group_by(Placette, Regroup) %>%
  summarise(Gha = sum(Gha)) %>%
  spread(Regroup, Gha, fill=0) %>%
  left_join(tab, by = "Placette") %>%
  # filter(!is.na(B2)) %>%
  dplyr::select(Epicéa:Sapin,SR:MTCI) %>%
  ungroup()

#------------------ Recherche modèle ------------------
mod <- SentinelPredict(Pla, "Gha", 'lm', s, max=100)
r <- mod$raster
mod <- SentinelPredict(Pla, "Gha", 'glm', s, max=100)
mod <- SentinelPredict(Pla, "Gha", 'gam', s, max=100)
writeRaster(r, "Gha.tif", format="GTiff", overwrite=T)

mod <- SentinelPredict(Pla, "Gha", 'gam', s, max=100)
writeRaster(r, "GhaGam.tif", format="GTiff", overwrite=T)

mod <- SentinelPredict(PlaEss, "Epicéa", 'glm', s, max=80)
writeRaster(r, "Epicéaglm.tif", format="GTiff", overwrite=T)

mod<- SentinelPredict(PlaEss, "Epicéa", 'gam', s, max=80)
writeRaster(r, "Epicéagam.tif", format="GTiff", overwrite=T)

mod <- SentinelPredict(PlaEss, "Sapin", 'glm', s, max=80)
writeRaster(r, "Sapin.tif", format="GTiff", overwrite=T)

mod <- SentinelPredict(PlaEss, "Feuillus", 'glm', s, max=60)
r <- mod$r
writeRaster(r, "Feuillus.tif", format="GTiff", overwrite=T)

plot(r)
plot(st_geometry(perim), add=T)

# ----------------- Qualité régression -----------------
mod$summary
t1 <- cbind( mod$model$fitted.values, mod$model$model)
names(t1)[1:2] <- c("Yestim", "Yréel")
ggplot(t1, aes(x=Yréel, y=Yestim)) + geom_point() + theme_bw() +
  geom_abline(intercept = 0, slope = 1)

# ----------------- Vérification zone pied à pied -----------------
Test <- st_read(dsn="~/Dropbox/Projets PSG",layer="TestSent", quiet=T) %>%
  st_transform(32631)

SentinelVerif(r, Test)


#------------------ Exemple de plan ------------------
plot(r)
plot(st_geometry(plac), add=T, col='blue', border='blue')
plot(st_geometry(perim), add=T)

