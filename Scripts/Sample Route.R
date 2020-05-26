#' Echantillonnage de la route
#'
#' @description Création de points systématiques le long de la route
#'
#' @return La fonction renvoit 2 objets
#'
#' @param r = un modèle numérique de terrain
#' @param shp = la route au format sf
#' @param pas=50 = le pas entre 2 points de la route. Par défaut il est égal à 50.
#'
#' @import sf
#' @import tidyverse
#' @import raster
#' @import sp
#'
#' @author camille, mathilde et nathan
#'
#'
#'
#' @export
#'

sampleroute<-function(r, shp, pas=50){
  nb <- as.integer(round(st_length(shp)/pas,0))
  zone <- st_buffer(shp, dist=100)
  r1 <- crop(r, as(zone, "Spatial"))
  pts <- st_line_sample(shp, nb) %>%
    st_cast("POINT") %>%
    st_sf()

  out=list(pts, r1)
  names(out) <- c("pts", "MNT")

  return(out)

}
