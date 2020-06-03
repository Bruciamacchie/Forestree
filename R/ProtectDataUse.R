#' Recherche statuts de protection
#'
#' @description La fonction recherche tous les statuts de protection situés sur ou à proximité immédiate
#' d'une propriété. La fonction nécessite en entrée
#' - un fichier géoréférencé au format sf contenant tous les statuts de protection en France métropolitaine.
#' Ce fichier peut être construit avec la fonction ProtectDataCreate
#' - un fichier géoréférencé du périmètre au format sf.
#'
#' @return La fonction enregistre un fichier au format .shp contenant tous les statuts de protection
#' existant sur la propriété. Il renvoie également une carte réalisée avec ggplot.
#'
#' @param shp1 = périmètre du territoire au format sf ou shp.
#' @param shp2 = périmètre du territoire au format sf ou shp.
#' @param width = taille du buffer en mètre. Par défaut width= 50 m.
#' @param nom = nom du fichier en sortie. Par défaut il s'appellera Protect.shp et sera enregistré
#' sur le répertoire en cours.
#'
#' @import sf
#' @import tools
#' @import rmapshaper
#' @import readxl
#' @import tidyverse
#' @import lwgeom
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' library(DataForet)
#' library(PPtools)
#' load("~/pCloud Sync/Packages/Forestree/StatutProtec.Rdata")
#' data(FD, package = "PPtools")
#' perim <- FD %>% filter(IIDTN_FRT =="F09844P")
#' graph <- ProtectDataUse(Protect, perim, width=100)
#' graph
#'
#' @export


ProtectDataUse <- function(shp1, shp2, width=50, nom="Protect"){
  zone <- shp2 %>% st_buffer(dist = width)
  shp1 <- st_intersection(shp1, zone)

  if (dim(shp1)[1] > 0){
    st_write(shp1, dsn=getwd(), layer=nom, driver="ESRI Shapefile", delete_layer=T, quiet =T)
    print(paste0("Ecriture du fichier ", nom,".shp"))
  } else {
    print("La zone d'étude n'est pas concernée par des status de proterction. ")
  }

  shp1 <- shp1 %>% ms_simplify(keep = 0.05)

  MyTheme <- theme(axis.ticks = element_blank(),
                   axis.text = element_blank(),
                   panel.background = element_blank(),
                   strip.text.x = element_text(size = 8))

  g <- ggplot() +
    geom_sf(data=shp1, aes(fill=Theme), alpha=0.2, size=0.2) +
    geom_sf(data=zone, fill=NA) +
    MyTheme
  return(g)
}

