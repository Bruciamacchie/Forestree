#' Fabrication d'un stack
#'
#' @description Fabrication stack à partir de couches raster, harmonisation des rasters,
#' adaptation des rasters à une zone d'étude.
#'
#' @import sf
#' @import tidyverse
#' @import raster
#'
#' @param listeRaster Liste des rasters qui consitueront le stack.
#' @param noms Noms des rasters à utiliser.
#' @param shp1 Zone d'étude au format.shp, les rasters seront découpés selon cette zone détude
#'elle doit donc être plus petite que les rasters.
#' @param shp2 Ppartie à exclure de la zone d'étude, par défaut shp2 = NULL.
#'
#' @return La fonction renvoie une pile de raster sous forme d'objet stack.
#'
#' @author Max Bruciamacchie
#'
#' @export


CreateStack <- function(listeRaster, noms, shp1, shp2 = NULL) {
  rlist <- lapply(listeRaster, raster)
  names(rlist) <- noms
  # par convention le premier sert de modèle
  for (i in 2:length(rlist)) {
    rlist[[i]] <- projectRaster(rlist[[i]],rlist[[1]])
    print(paste("harmonisation du raster :", CriteresNom [i]))
  }

  s <- stack(rlist) %>% mask(shp1)  #raster multicouche
  print("Découpage selon shp1")
  if(!is.null(shp2)) {
    s <- s %>% mask(shp2, inverse=T)
    print("exclusion terminée")
  }

  return(s)
}
