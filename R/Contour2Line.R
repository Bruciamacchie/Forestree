#' Transformation raster en courbes de niveau
#'
#' @description La fonction transforme un modèle numérique de terrain en courbes de niveau.
#' puis offre la possibilité de sélectionner une de ces courbes de niveau comme limite de
#' séparation d'un polygone.
#'
#' @return La fonction renvoie un objet LINESTRING.
#'
#' @param shp = périmètre de la zone d'étude au format sf.
#' @param r = raster à convertir en courbe.
#' @param nlevels = nombre de courbes. Par défaut nlevels = 40.
#' @param valline = valeur de la ligne à extraire
#'
#' @import sf
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' library(sf, quietly =T, warn.conflicts =F)
#' library(Forestree)
#' data(razel)
#' mnt = SiteMnt(razel)
#' courbe <- Contour2Line(razel, mnt, valline="430")
#'
#' plot(st_geometry(razel))
#' plot(st_geometry(courbes), add=T, col='orange', lwd=0.5)
#' plot(st_geometry(Ligne), add=T, col='red')
#'
#' @export

Contour2Line <- function(shp, r, nlevels = 40, valline=NULL){
  r <- crop(r, as(shp, "Spatial"))
  courbes <- rasterToContour(r, nlevels = nlevels)
  courbes <- st_as_sf(courbes) %>%
    st_set_crs(2154)

  zone <- shp %>% st_buffer(dist=50)

  if (!is.null(valline)) {
    ligne <- courbes %>%
      filter(level==valline) %>%
      st_intersection(zone) %>%
      st_cast("LINESTRING") %>%
      mutate(long = st_length(.)) %>%
      arrange(desc(long)) %>%
      slice(1) %>%
      st_cast("LINESTRING")
  }

  out = list(courbes, ligne)
  names(out) = c("courbes", "ligne")
  return(out)
}

