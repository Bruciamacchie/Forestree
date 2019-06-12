#' Modèle numérique de terrain
#'
#' @description Modèle numérique de terrain à partir du serveur AWS.
#'
#' @return La fonction renvoie un MNT dont la zone d'extension correspond à celle du polygone fourni en entrée.
#'
#' @param shp = périmètre de la zone au format sf.
#' @param buffer = taille du buffer. Par défaut buffer = 50 m.
#' @param zoom = un entier compris entre 1 et 14. Plus le zoom est élevé plus la résolution sera faible.
#'
#' @import elevatr
#' @import sf
#'
#' @examples
#' \donttest{
#' library(elevatr)
#' data(razel)
#' r <- SiteMnt(razel)
#' plot(r, axes=F, box=F)
#' plot(st_geometry(razel), add=T)
#' }
#'
#' @author Bruciamacchie Max
#'
#' @export
#'
SiteMnt <- function(shp, buffer=50, zoom=14, epsg=NULL) {
  if (is.numeric(buffer) & is.numeric(zoom)) {
    shp <- st_buffer(shp, dist=buffer)
    zoom = max(zoom, 9)
    x <- get_elev_raster(as(shp, "Spatial"), z = zoom, src = "aws")
    x <- crop(x, as(shp, "Spatial"))
    if (!is.null(epsg)) {
      x <- projectRaster(x, crs=CRS(paste0('+init=EPSG:',epsg)))
    }
    return(x)
  } else {
    print("buffer et zoom doivent être des entiers.")
  }
}
