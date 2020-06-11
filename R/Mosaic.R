#' Assembler rasters
#'
#' @description Cette fonction assemble tous les rasters au format TIF ou ASC contenus dans un 
#' dossier en un seul fichier au format TIF.
#'
#' @import tcltk
#' @import rgdal
#' @import gdalUtils
#' @import raster
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' Mosaic(Nom = "IRC.tif")
#'
#' @export


Mosaic <- function(Nom) {
  file <- tk_choose.files(caption = "Choix d'un fichier raster",
                          filters=matrix(c("GEOTIF", ".tif", "ASCII", ".asc"),2,2, byrow = T))
  if (length(file) == 0) {
    Stop("Import annulé")
  }
  rep <- dirname(file)
  fichiers <- c(list.files(rep, full.names =T, pattern="\\.tif$"),
                list.files(rep, full.names =T, pattern="\\.asc$"))
  
  e <- extent(raster(fichiers[1]))
  for (i in 2:length(fichiers)) {
    e2 <- extent(raster(fichiers[i]))
    e[1] = min(e[1], e2[1])
    e[2] = max(e[2], e2[2])
    e[3] = min(e[3], e2[3])
    e[4] = max(e[4], e2[4])
  }
  
  template <- raster(e)
  proj4string(template) <- CRS("+init=epsg:2154")
  writeRaster(template, file=Nom, format="GTiff")
  mosaic_rasters(gdalfile=fichiers, dst_dataset=Nom, of="GTiff")
}




