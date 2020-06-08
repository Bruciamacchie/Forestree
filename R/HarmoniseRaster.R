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
#' @import sf
#' @import raster
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' file <- system.file("MNS.tif", package = "Forestree")
#' mns <- raster(file)
#' file <- system.file("MNT.tif", package = "Forestree")
#' mnt <- raster(file)
#' result <- HarmoniseRaster(mns, mnt, resout = 1)
#' @export

HarmoniseRaster <- function(r1, r2, resout = 1) {
  if (is(r1, "RasterLayer") & is(r2, "RasterLayer")) {
    print("Mise à la résolution de sortie du premier raster")
    ech <- resout/res(r1)[1]
    if (ech > 1) {
      r1 <- aggregate(r1, fact=ech, 'max')
    }
    if(ech < 1){
      r1 <- disaggregate(r1, fact=1/ech)
    }
    print("Harmonisation du second raster")
    r2 <- projectRaster(r2, r1)
    out <- list(r1,r2)
    return(out)
  } else {print("La fonction n'admet que des RasterLayers")}
}

