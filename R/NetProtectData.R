#' Recherche statuts de protection
#'
#' @description La fonction recherche sur internet les statuts de protection applicables sur un territoire donné.
#' La fonction nécessite en entrée un fichier géoréférencé du périmètre au format sf ou shp.
#' Si les fichiers en entrée sont au format shp la fonction les transforme au format sf.
#'
#' @return La fonction crée un dossier appelé StatutProtect qui contiendra tous les statuts de protection
#' existant sur la propriété.
#'
#' @param shp = périmètre du territoire au format sf ou shp.
#' @param width = taille du buffer en mètre. Par défaut width= 50 m.
#' @param inter = Par défaut il n'y a pas intersection avec le perimètre (inter=FALSE)
#' @param onedir = Par défaut les différents statuts sont rangés dans des dossiers différents
#' (onedir=FALSE). Si onedir = TRUE, les différents statuts sont rangés dans un seul dossier
#' appelé StatutProtect.
#'
#' @import sf
#' @import tools
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' library(sf, quietly =T, warn.conflicts =F)
#' library(tools, quietly =T, warn.conflicts =F)
#' ##### Données
#' data(perim)
#' NetProtectData(perim, width=100)
#'
#' @export

NetProtectData <- function(shp, width=50, inter=TRUE, onedir = FALSE){
  if (sum(class(shp) %in% c("SpatialPolygonsDataFrame","sf")) > 0) {
    if (class(shp) == "SpatialPolygonsDataFrame") {
      shp <- st_as_sf(shp)
    }
    dir.create("StatutProtect", showWarnings = FALSE)
    shp <- st_buffer(shp, dist=width)
    data(liens)

    for (i in 1:dim(liens)[1]){
      theme <- as.character(liens[i,1])
      lien  <- as.character(liens[i,2])
      if(onedir) {
        dossier <- "StatutProtect"
      } else {
        dossier <- paste0("StatutProtect/",theme)
      }
      dossiertemp <- paste0("StatutProtect/",theme,".zip")
      print(paste("Téléchargement du dossier", theme))
      download.file(url = lien, destfile = dossiertemp, method = "auto")
      unzip(zipfile = dossiertemp, exdir = dossier)
      file.remove(dossiertemp)

      reptemp <- list.dirs(dossier)
      if(length(reptemp) == 1) {
        dossiertemp = dossier
      } else {
        dossiertemp = reptemp[[length(reptemp)]]
      }
      fich <- list.files(dossiertemp, pattern="\\.shp$", recursive=T)
      fich <- file_path_sans_ext(basename(fich))

      sf1 <- st_read(dsn=dossiertemp, layer=fich, quiet=T)
      sf1 <- st_transform(sf1, st_crs(shp))
      sf1 <- st_intersection(sf1, shp)

      if (dim(sf1)[1] > 0){
        st_write(sf1, dsn=dossier, layer=fich, driver="ESRI Shapefile",
                 update=T, delete_layer=T, quiet =T)
        print(paste0("Ecriture du fichier ", fich,".shp"))
      } else {
        print(paste0("La zone d'étude n'est pas concernée par le thème ", theme))
      }

    }
  } else { print("Le fichier en entrée doit être au format SpatialPolygonsDataFrame ou sf")}
}

