#' Conversion JP2 to TIF
#'
#' @description Cette fonction convertit tous les fichiers au format JP2 contenus dans un dossier en format .tif.
#'
#' @import tcltk
#' @import gdalUtils
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' JP2toTIF()
#'
#' @export

JP2toTIF <- function() {
  rep <- tk_choose.dir("Choisir un dossier contenant des fichiers .JP2")
  fichiers <- list.files(rep, pattern="\\.jp2$")
  if (length(fichiers) == 0) {
    stop("Le dossier ne contient aucun fichier au format .jp2")
  }
  cat(paste("Le dossier contient",length(fichiers),"fichiers au format .jp2"))
  for(i in 1:length(fichiers)) {
    input <- paste0(rep,"/", fichiers[i])
    out <- paste0(rep,"/", tools::file_path_sans_ext(fichiers[i]),".tif")
    gdal_translate(input,out)
    print(paste("Conversion",fichiers[i],"........ terminée"))
  }
}
