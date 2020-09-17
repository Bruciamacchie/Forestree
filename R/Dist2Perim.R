#' Calcul de la distance à un périmètre
#'
#' @description La fonction nécessite en entrée un polygone. Ce dernier est d'abord transformé en grid, puis la distance au périmètre
#' de chaque cellule du grid est claculé
#'
#'
#' @return La fonction renvoie
#' - un objet sf contenant les coordonnées de chaque pixel avec sa distance au périmètre.
#' - un graphique
#' - un objet raster
#'
#' @param sf = périmètre de la zone d'étude au format sf.
#' @param size = résolution du grid. Par défaut, size est fixé à 2500.
#' @param raster  = par défaut cet argument est FALSE, ce qui signifie que l'objet renvoyé est au format stars.
#' Si l'argument est TRUE, l'objet est renvoyé au format raster.
#'
#' @import sf
#' @import tidyverse
#' @import RColorBrewer
#' @import rnaturalearth
#' @import rnaturalearthhires
#' @import raster
#' @import stars
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' library(Forestree)
#' perim <- ne_countries(scale = 10, country = "Ireland", returnclass = "sf") %>%
#' st_transform(3857)
#'
#' plot(st_geometry(perim))
#' res <- Dist2Perim(perim)
#' res$graph
#' head(res$tab)

#' @export

Dist2Perim <- function(sf, size=2500, raster=FALSE) {
  grid <- st_make_grid(sf, cellsize = size, what = "centers")
  sf <- st_cast(sf, "MULTILINESTRING")
  dist <- st_distance(sf, grid)
  df <- data.frame(st_coordinates(grid),
                   dist = as.vector(dist)/1000)

  g <- ggplot(df, aes(X, Y, fill = dist)) +
    geom_tile() + #geometry
    coord_fixed() +
    labs(fill = "Distance (km)") + #legend name
    theme_void() + #map theme
    theme(legend.position = "bottom") #legend position

  z <- st_as_sf(df, coords=c("X","Y"))
  z <- st_rasterize(z)

  if(raster) {
    r <- as(z, "Raster")
  } else {
    r <- z
  }

  out <- list(df, g, r)
  names(out) <- c("tab", "graph", "raster")

  return(out)
}

