#' Selection de parcelles cadastrales
#'
#' @description La fonction permet de produire un shapefile polygone contenant l'ensemble des parcelles cadastrales
#' d'un ou plusieurs propriétaires. La fonction sélectionne dans un fichier .shp les parcelles cadastrales
#' figurant dans des extraits de matrice cadastrale.
#'
#' @return La fonction retourne un shapefile polygone au format shp.
#'
#' @author Bruciamacchie Max & Chevereau Matthieu
#'
#' @param df = tableau contenant la liste des parcelles cadastrales. Il doit contenir une colonne nommée "idu" ou "IDU"
#' @param shp = un fichier géoréférencé contenant des parcelles cadastrales.
#' Sa table attributaire doit contenir une colonne nommée "idu" ou "IDU"
#'
#' @import readr
#' @import dplyr
#' @import sf
#'
#' @examples
#' \donttest{
#' ####### Utilisation
#' data("MatriceCad")
#' data("Parcellaire")
#' shp <- CadPar(MatriceCad, Parcellaire)
#' plot(st_geometry(Parcellaire), border='blue', lwd=0.5)
#' plot(st_geometry(shp), border='red', add=T)
#' }
#'
#' @export

CadPar <- function(df, shp) {
  if (sum(c("idu","IDU") %in% names(shp)) >0 & sum(c("idu","IDU") %in% names(df))) {# Si l'idu est détecté
    print("Identifiant unique détecté")
    names(df) <- sapply(names(df), tolower)
    names(shp) <- sapply(names(shp), tolower)
    if (length(unique(shp$idu)) < dim(shp)[1]) {
      print("Les identifiants ne sont pas uniques")
      shp <- shp %>%
        group_by(idu) %>%
        slice(1) %>%
        ungroup
    }
    shp <- shp %>%
      filter(idu %in% df$idu) %>%
      left_join(df, by = "idu")
    st_write(shp, dsn=getwd(),layer ="ParCad", update=TRUE, delete_layer = TRUE, driver = "ESRI Shapefile")
    return(shp)
  } else {
    print("Champs de référence absent : tentative abandonnée")
    print("Traitement annulé")
    stop() # Arrêt du traitement
  }
}
