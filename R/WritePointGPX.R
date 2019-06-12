#' Ecriture au format GPX de données pontuelles
#'
#' @description La fonction sauvegarde au format GPX des données ponctuelles en permettant de choisir
#' dans la table attributaire, par le biais d'une boîte de dialogue, le champ qui sera utilisé
#' pour nommer les points.
#'
#' @return La fonction enregistre dans le même répertoire que le fichier utlisé en entrée.
#'
#' @param nom = nom du fichier en sortie. Par défaut nom="gpxfile.gpx".
#'
#' @import sf
#' @import tools
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' WritePointGPX_sf()
#' WritePointGPX_sf("VeytonPlac")
#' @export

WritePointGPX_sf <- function(nom="gpxfile.gpx") {
  rep=getwd()
  file <- tk_choose.files(caption = "Choix du fichier géoréférencé ponctuel",
                          filters=matrix(c(".shp",".shp"),1,2, byrow = T))
  if (length(file) > 0) {
    setwd(dirname(file))
    shp <- st_read(dsn=dirname(file), layer=basename(file_path_sans_ext(file)), quiet=T)
    shp <- st_transform(shp, 4326)

    Noms <- names(shp)
    Choix <- tk_select.list(Noms, title = "Choisir une variable en tant qu'identifiant")
    if (length(Choix) > 0) {
      shp <- shp %>%
        dplyr::select(which(Noms==Choix))
      # Le format n'admet comme nom de champs que "name" pour le nom, "ele" pour elevation
      # et "time" pour l'information sur le temps
      names(shp)[1] <- "name"

      st_write(shp, dsn=nom, layer="waypoints", driver="GPX", update=T, delete_dsn=T, quiet =T)
      print(paste("Fichier imprimé sous le nom",nom,"dans le répertoire",rep))
    } else {print("Import annulé")}
    setwd(rep)
  }
}
