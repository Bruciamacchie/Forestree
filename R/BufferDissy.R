#' Buffer dissymetrique
#'
#' @description Cette fonction renvoie un polygone correspondant à un buffer dissymétrique amont aval.
#' Elle permet de cartographier les zones exploitables par skidder en montagne.
#'
#' @param line = objet de type LINE correspondant à une piste ou une route.
#' @param r = raster correspondant à un MNT.
#' @param amont = distance selon la pente exploitable en amont. Par défaut amont = 50m.
#' @param aval = distance selon la pente exploitable en aval. Par défaut aval = 150m.
#' @param pas = résolution.  Par défaut pas = 25m
#' @param conc = indice relatif de concavité. Plus cet indice est faible plus le contour sera découpé.
#' Cet indice ne doit pas être inférieur à 1. Par défaut cet indice est égal à 1.8.
#'
#' @import sf
#' @import raster
#' @import tidyverse
#' @import concaveman
#' @import nngeo
#'
#' @author Bruciamacchie Max
#'
#' @examples
#'
#' @export
#'

BufferDissy <- function(shp, r, amont=50, aval=150, pas=50, conc=1.8) {
  if(st_geometry_type(shp) !="LINESTRING" | class(r) != "RasterLayer") {
    stop(cat("la fonction nécessite en entrée un objet sf de type LINESTRING et un rasterLayer"))
  }
  # zone <- st_buffer(shp, dist=aval*2)
  # r <- crop(r, as(zone, "Spatial"))

  # -------- Echantillonnage de la route -----------
  ecart = as.numeric(pas/st_length(shp))
  echan <- seq(0,1,by=ecart)
  echan <- c(echan[-length(echan)],1)
  pts <- st_line_sample(shp, sample=echan) %>%
    st_sf() %>%
    st_cast("POINT")  %>%
    mutate(Iden = 1:dim(.)[1],
           AltiRef = raster::extract(mnt, as(., "Spatial")))

  tab <- pts %>%
    st_drop_geometry() %>%
    dplyr::select(Iden, AltiRef)

  # -------- Recherche points buffer -----------
  buf1 <- st_buffer(projet, dist=amont) %>%
    st_cast("POINT") %>%
    mutate(Id = 1:dim(.)[1],
           Alti = raster::extract(mnt, as(., "Spatial")))

  buf2 <- st_buffer(projet, dist=aval) %>%
    st_cast("POINT") %>%
    mutate(Id = 1:dim(.)[1],
           Alti = raster::extract(mnt, as(., "Spatial")))

  ls1 <- nngeo::st_nn(buf1, pts, returnDist = T, progress=F)
  ls2 <- nngeo::st_nn(buf2, pts, returnDist = T, progress=F)

  ptsamont <- buf1 %>%
    mutate(Iden = unlist(ls1$nn)) %>%
    left_join(tab, by = "Iden") %>%
    mutate(Dif = AltiRef - Alti) %>%
    filter(Dif < 0)

  ptsaval <- buf2 %>%
    mutate(Iden = unlist(ls2$nn)) %>%
    left_join(tab, by = "Iden") %>%
    mutate(Dif = AltiRef - Alti) %>%
    filter(Dif > 0)

  # -------- Recherche polygone -----------
  buf <- rbind(ptsaval, ptsamont) %>%
    concaveman(concavity=conc)

  return(buf)
}
















