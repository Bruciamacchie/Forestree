#' Recherche voisins
#'
#' @description Recherche de tous les polygones voisins d'un polygone.
#'
#' @import sf
#'
#' @param shp couche géoréférencée contenant des POLYGON ou MULTIPOLYGON.
#'
#' @return La fonction renvoie l'objet sf fourni en entrée avec deux géométries supplémentaires,
#' l'une correspondant pour chaque polygone à l'union des voisins (geom2),
#' l'autre à l'union du polygone avec ses voisins (geom1).
#'
#' @author Max Bruciamacchie
#'
#' @examples
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' shp <- voisins(nc)
#'
#' par(mfrow=c(1,2))
#' st_geometry(shp) <- "geom1"
#' plot(st_geometry(shp[100,]),border="blue", lwd=4)
#' plot(st_geometry(nc[100,]), col="grey90", add=T)
#' plot(st_geometry(nc), border="red", lwd=1.5, add=T)
#'
#' st_geometry(shp) <- "geom2"
#' plot(st_geometry(shp[100,]),border="blue", lwd=4)
#' plot(st_geometry(nc[100,]), col="grey90", add=T)
#' plot(st_geometry(nc), border="red", lwd=1.5, add=T)
#' par(mfrow=c(1,1))
#'
#' @export


Voisins <- function(shp){
  if (!inherits(shp, "sf")) {
    stop("La fonction nécessite en entrée un objet sf.")
  }

  geom1 = list()
  geom2 = list()
  for(i in 1:nrow(shp)) {
    geom1[[i]] = st_union(shp[shp[i, ], ])
    geom2[[i]] = st_difference(geom1[[i]], shp[i, ])
  }
  geom1 = do.call(c, geom1)
  geom2 = do.call(c, geom2)
  shp$geom1 = geom1
  shp$geom2 = geom2
  return(shp)
}





