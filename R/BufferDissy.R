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
  if(!inherits(shp, "sf") & !(st_geometry_type(st_union(shp)) %in% c("LINESTRING", "MULTILINESTRING"))) {
    stop(cat("la fonction nécessite en entrée un objet sf de type LINESTRING."))
  }
  if(class(r) != "RasterLayer") {
    stop(cat("la fonction nécessite en entrée un objet de type rasterLayer"))
  }

  buf <- st_sf(st_sfc(), crs=st_crs(shp)) # création objet vide
  for (i in 1:dim(shp)[1]) {
    temp <- AmontAval(shp[i,], r, amont, aval, pas, conc)
    buf <- rbind(buf, temp)
  }
  buf <- st_union(buf)

  return(buf)
}















