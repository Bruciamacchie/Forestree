#' Correspondance parcellaire forestier et cadastral
#'
#' @description La fonction recherche la correspondance entre parcellaire forestier et cadastral.
#' Elle fait en sorte que la somme des surfaces des parcelles forestières soit égale à la surface
#' officielle cadastrale. Cette recherche est faite en évitant de trop aggréger les parcelles.
#' Elle calcule un coefficient par parcelle permettant de passer de la surface SIG à la surface cadastrale.
#'
#' La fonction nécessite en entrée 2 fichiers géoréférencés au format sf : parcellaire cadastral et forestier.
#' Si les fichiers en entrée sont au format shp, l'exemple ci-après montre comment les transformer au format sf.
#'
#' La table attributaire du parcellaire cadastral doit contenir au moins les champs suivants :
#' Commune, Section, Num et Surface.
#'
#' @return La fonction
#'
#' - renvoie un tableau de correspondance entre numéros de parcelle cadastrale et numéro de parcelle forestiére.
#'
#' - renvoie un tableau des surfaces des parcelles forestières
#'
#' - enregistre le shape des parcelles forestières avec comme nouveau champ, un coefficient permettant de passer de la surface
#' SIG à la surface cadastrale;
#'
#' @param shpCad = fichier du parcellaire cadastral au format sf.
#' @param shpFor = fichier du parcellaire forestier au format sf.
#' @param Nom = Nom par défaut du fichier de sortie.
#'
#' @import sf
#' @import maptools
#' @import rgeos
#' @import tidyr
#' @import dplyr
#' @import tcltk
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' library(sf, quietly =T, warn.conflicts =F)
#' library(ForestTools)
#' ##### Données
#' data("parCadast")
#' data("parFor")
#' parCadast <- st_as_sf(parCadast)
#' parFor <- st_as_sf(parFor)
#' ##### Visualisation
#' par(mar=c(0,0,0,0))
#' plot(st_geometry(parFor), lwd=2.5, border='blue')
#' plot(st_geometry(parCadast), add=TRUE, border='red')
#' ##### Utilisation fonction
#' res <- CadCorFor(parCadast, parFor)
#' ##### Résultats
#' res$Correspondance
#' res$Surfaces
#'
#' @export

# ------ Fonctions -----------------
CadCorFor <- function(shpCad, shpFor, nom="ParcellaireForestier") {
  options(digits=6)
  if (is(shpCad, "sf") & is(shpFor, "sf")) {
    if (is.na(st_crs(shpCad))) {st_crs(shpCad) = 2154}
    if (is.na(st_crs(shpFor))) {st_crs(shpFor) = 2154}
    if (st_crs(shpCad) == st_crs(shpFor)) {
      if (sum(c("Commune","Section","Num","Surface") %in% names(shpCad))==4) {
        # --- Intersection
        shpCad <- shpCad %>%
          mutate(IdenCad = paste(Commune,Section,Num, sep="-"))
        Liste <- names(shpFor)
        seuil = 2 + ifelse("geometry" %in% Liste, 1, 0)
        if (length(Liste) > seuil) {
          ChoixVar <- tk_select.list(as.character(Liste), multiple = FALSE,
                                     title = "Choisir la variable correspondant au num du parcellaire forestier")
        } else {ChoixVar <- Liste[1]}

        tab <- shpFor
        st_geometry(tab) <- NULL
        pos <- which(names(tab) == ChoixVar)
        shpFor$NumFor <- tab[, pos]

        x <- st_intersection(shpCad, st_buffer(shpFor, dist=-1e-07)) %>%
          mutate(SurfSig = as.numeric(st_area(.)))

        tab <- subset(x, select=c(IdenCad,Commune,Section,Num,NumFor,SurfSig))
        st_geometry(tab) <- NULL
        Corresp <- subset(tab, select=c(Commune,Section,Num,NumFor))

        tab1 <- tab %>%
          group_by(IdenCad) %>%
          summarise(SurfT = sum(SurfSig)/10000) %>%
          left_join(shpCad[,c("IdenCad","Surface"),], by="IdenCad") %>%
          mutate(Coefft = Surface/SurfT) %>%
          dplyr::select(IdenCad, Coefft)

        tab <- tab %>%
          left_join(tab1, by="IdenCad") %>%
          mutate(Surf = SurfSig*Coefft/10000) %>%
          group_by(NumFor) %>%
          summarise(Surf = sum(Surf))

        # ------ Ecriture du shape ---------------------
        shpFor <- shpFor %>%
          left_join(tab, by = "NumFor")
        shpFor$NumFor <- NULL
        st_write(shpFor, dsn=".", layer=nom, driver="ESRI Shapefile",
                 update=T, delete_layer=T, quiet =T)
        print(paste("fichier enregistré sous le nom",nom, "dans le répertoire ",getwd()))

        Liste <- list(Corresp, tab)
        names(Liste) <- c("Correspondance","Surfaces")
        return(Liste)
        print("Correspondance terminée")
      } else{print("Le parcellaire cadastral doit contenir les champs Commune, Section, Num, Surface")}
    } else {print("Les deux SPDF doivent avoir le même système de projection")}
  } else{print("Les deux fichiers doivent être des objets sf.")}
}
