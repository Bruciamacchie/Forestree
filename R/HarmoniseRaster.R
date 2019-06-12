#' Harminisation de 2 rasters
#'
#' @description La fonction harmonise le deuxième raster de sorte qu'il ait même résolution, origine
#' et extension que le premier.
#'
#' @return La fonction renvoie un stack contenant les deux rasters.
#'
#' @param r1 = RasterLayer qui sert de référence
#' @param r2 = RasterLayer à harmoniser
#' @param resout = résolution des deux rasters en sortie. Par défaut resout = 1m.
#'
#' @import tidyverse
#' @import sp
#' @import rgeos
#' @import sf
#' @import raster
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' data(mns)
#' data(mnt)
#' s <- HarmoniseRaster(mns, mnt)
#' @export

HarmoniseRaster <- function(r1, r2, resout = 1) {
  if (is(r1, "RasterLayer") & is(r2, "RasterLayer")) {
    ech <- resout/res(r1)
    print("Aggregation du premier raster")
    r1 <- aggregate(r1, fact=ech, 'max')
    crs(r1) <- CRS('+init=EPSG:2154')
    crs(r2) <- CRS('+init=EPSG:2154')
    print("Harmonisation")
    r2 <- projectRaster(r2, r1)
    s <- stack(r1,r2)
    return(s)
  } else {print("La fonction n'admet que des RasterLayers")}
}
