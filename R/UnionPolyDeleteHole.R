#' Simplification du domaine d'étude - union et suppression des trous
#' @description La fonction UnionPolyDeleteHole utilise en entrée un shape de type surfacique.
#' Elle agrège tous les polygones en un seul, et supprime les éventuels trous.\cr
#' @return La fonction renvoie le shape PerimCor.shp comme polygone unifié.
#' @param nom = nom souhaité pour le fichier en sortie.
#' @import rgeos
#' @import rgdal
#' @import tools
#' @import tcltk
#' @import sp
#' @import maptools
#' @author Bruciamacchie Max
#' @export
UnionPolyDeleteHole <-function(nom="PerimCor") {
  shp <- tk_choose.files(caption = "Choix du périmètre",
                         filters=matrix(c(".shp",".shp"),1,2, byrow = T))
  if (length(file) > 0) {
    perim <- readOGR(dsn=dirname(shp), layer=basename(file_path_sans_ext(shp)), verbose = F)
    SP <- gUnaryUnion(perim)
    trous <- sapply(SP@polygons[[1]]@Polygons, function(x) x@hole)
    Poly <- list()
    for(i in 1:length(trous)){
      if(!trous[[i]]) Poly <- c(Poly, SP@polygons[[1]]@Polygons[[i]])
    }
    SP1 <- SpatialPolygons(Srl = list(Polygons(srl = Poly, ID="Global")))
    tab <- data.frame(Nom="Perim")
    row.names(tab) <- "Global"
    SPDF <- SpatialPolygonsDataFrame(Sr = SP1, data = tab)
    SPDF@proj4string <- perim@proj4string
    dir <- getwd()
    writePolyShape(SPDF, paste(dirname(shp), nom, sep="/"))
    setwd(dirname(shp))
    cat(showWKT(proj4string(perim)),file=paste0(nom,".prj"))
    setwd(dir)
    # paste("fichier enregistré sous le nom",nom, "dans le répertoire ",dirname(shp))
    out <- list(shp, paste0(nom, ".shp"))
    names(out) <- c("input", "output")
    return(out)
  } else {return(print("Import annulé"))}
}


