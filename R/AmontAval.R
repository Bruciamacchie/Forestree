#' Buffer dissymetrique
#'
#' @description Cette fonction est nécessaire pour cartographier les zones exploitables par skidder en montagne.
#' Elle renvoie un polygone correspondant à un buffer dissymétrique amont aval autour d'un seul LINESTRING.
#'
#' @param lig = objet de type LINESTRING correspondant à une piste ou une route.
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


AmontAval <- function(lig, r, amont=50, aval=150, pas=25, conc=1.8) {
  # -------- Echantillonnage du troncon -----------
  ecart = as.numeric(pas/st_length(lig))
  echan <- seq(0,1,by=ecart)
  echan <- c(echan[-length(echan)],1)
  pts <- st_line_sample(lig, sample=echan) %>%
    st_sf() %>%
    st_cast("POINT")  %>%
    mutate(Iden = 1:dim(.)[1],
           AltiRef = raster::extract(mnt, as(., "Spatial")))

  tab <- pts %>%
    st_drop_geometry() %>%
    dplyr::select(Iden, AltiRef)

  # -------- Recherche points buffer -----------
  buf1 <- st_buffer(lig, dist=amont) %>%
    st_cast("POINT") %>%
    mutate(Id = 1:dim(.)[1],
           Alti = raster::extract(mnt, as(., "Spatial")))

  buf2 <- st_buffer(lig, dist=aval) %>%
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
  temp <- rbind(ptsaval, ptsamont) %>%
    concaveman(concavity=conc)

  return(temp)
}
