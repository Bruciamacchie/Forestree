#' Buffer dissymetrique
#'
#' @description Cette fonction renvoie un polygone correspondant à un buffer dissymétrique amont aval.
#' Elle permet de cartographier les zones exploitables par skidder en montagne.
#'
#' @param line = objet de type LINE correspondant à une piste ou une route.
#' @param r = raster correspondant à un MNT.
#' @param amont = distance selon la pente exploitable en amont. Par défaut amont = 50m.
#' @param aval = distance selon la pente exploitable en aval. Par défaut aval = 150m.
#' @param pas = résolution.  Par défaut pas = 25m
#'
#' @import sf
#' @import raster
#' @import dplyr
#'
#' @author Bruciamacchie Max
#'
#' @examples
#'
#' @export
#'

BufferDis <- function (line, r, amont=50, aval=150, pas=50) {
  if(st_geometry_type(line) =="LINESTRING") {
    if(class(r) == "RasterLayer") {
      zone <- st_buffer(line, dist=aval*2)
      r <- crop(r, as(zone, "Spatial"))
      # ------------ Echantillonnage --------------
      # empty <- st_as_sfc("POINT(EMPTY)")
      nb <- as.integer(round(st_length(line)/pas,0))
      pts <- st_line_sample(line, sample=seq(0,1,length.out=nb+1)) %>%
        st_cast("POINT") %>%
        st_sf() %>%
        mutate(Point = 1:(nb+1),
               dist = st_length(line)/nb,
               # dist = st_distance(geometry,lead(geometry, default = empty), by_element = TRUE),
               dist = lag(dist),
               dist = ifelse(is.na(dist),0,dist),
               distcum = cumsum(dist),
               Alti = raster::extract(r, as(., "Spatial")),
               Après = lead(Alti),
               Avant = lag(Alti),
               PenteAp = (Après - Alti)/dist,
               PenteAv = lag(PenteAp),
               Pente = rowMeans(cbind(PenteAp, PenteAv), na.rm=T)) %>%
        dplyr::select(Point, dist, distcum, Alti, Pente)
      out = list(pts)
      names(out) <- c("pts")

      # ----------- Direction -------------
      centre <- do.call(rbind, st_geometry(pts)) %>%
        as_tibble() %>%
        setNames(c("X","Y")) %>%
        mutate(Point = 1:dim(pts)[1]) %>%
        left_join(pts[,c("Point","Alti")], by = "Point") %>%
        mutate(Xap = lead(X),
               Xav = lag(X),
               Yap = lead(Y),
               Yav = lag(Y),
               DirAp = (pi/2-atan2(Yap-Y, Xap-X))/pi*200 + 400,
               DirAp = DirAp %% 400, # modulo
               DirAv = lag(DirAp)) %>%
        mutate(Dir = rowMeans(cbind(DirAp, DirAv), na.rm=T),
               X1 = X + amont*sin((Dir+100)/200*pi),
               Y1 = Y + amont*cos((Dir+100)/200*pi),
               X2 = X + amont*sin((Dir-100)/200*pi),
               Y2 = Y + amont*cos((Dir-100)/200*pi),
               X3 = X + aval*sin((Dir+100)/200*pi),
               Y3 = Y + aval*cos((Dir+100)/200*pi),
               X4 = X + aval*sin((Dir-100)/200*pi),
               Y4 = Y + aval*cos((Dir-100)/200*pi)) %>%
        dplyr::select(Point,X,Y,Alti,Dir,X1:Y4)

      # ------------ Buffer --------------
      # centre <- coords %>% st_as_sf(coords = c('X', 'Y'), crs= st_crs(line))
      buffer1 <- st_buffer(line, dist=amont)
      buffer2 <- st_buffer(line, dist=aval)

      droite <- centre %>%
        filter(!is.na(X1)) %>%
        st_as_sf(coords = c('X1', 'Y1'), crs=st_crs(line)) %>%
        dplyr::select(Point, Alti) %>%
        mutate(Alti1 = raster::extract(r, as(., "Spatial")),
               Position = ifelse(Alti1-Alti >0,1,0)) %>%
        filter(Position == 1) %>%
        dplyr::select(Point)

      droite1 <- centre %>%
        filter(!is.na(X3)) %>%
        st_as_sf(coords = c('X3', 'Y3'), crs=st_crs(line)) %>%
        dplyr::select(Point, Alti) %>%
        mutate(Alti1 = raster::extract(r, as(., "Spatial")),
               Position = ifelse(Alti1-Alti >0,0,1)) %>%
        filter(Position == 1) %>%
        dplyr::select(Point)

      bord <- rbind(pts[1, "Point"],droite, droite1) %>%
        mutate(Bord = "Droit") %>%
        arrange(Point) %>%
        group_by(Bord) %>%
        summarise(do_union = FALSE) %>%
        # st_sf() %>%
        st_cast("LINESTRING")

      st_is_valid(bord)
      sf_extSoftVersion()["lwgeom"]
      st_make_valid(bord)

      gauche <- centre %>%
        filter(!is.na(X2)) %>%
        st_as_sf(coords = c('X2', 'Y2'), crs=st_crs(line)) %>%
        dplyr::select(Point, Alti) %>%
        mutate(AltiG1 = raster::extract(r, as(., "Spatial")))

      plot(st_geometry(buffer2))
      plot(st_geometry(buffer1), add=T)
      plot(st_geometry(line), add=T)

      plot(st_geometry(line))
      plot(st_geometry(pts), add=T, col='red')
      plot(st_geometry(bord), add=T, col='blue', pch=3)
      # plot(st_geometry(centre), add=T, col='red', pch=3)
      # plot(st_geometry(droite), add=T, col='blue', pch=3)
      # plot(st_geometry(droite1), add=T, col='green', pch=3)




      # ----------- Perpendiculaires


      gauche <- coords %>%
        filter(!is.na(X2)) %>%
        st_as_sf(coords = c('X2', 'Y2'), crs=2154) %>%
        dplyr::select(Point)

      Perpend <- rbind(droite, gauche) %>%
        group_by(Point) %>%
        summarize() %>%
        st_cast("LINESTRING")

      return(out)
    } else {cat("l'argument r doit être un RasterLayer")}
  } else{
    cat("La piste doit être un objet sf au format LINESTRING")
  }
}
