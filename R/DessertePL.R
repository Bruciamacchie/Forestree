#' Profil en long
#'
#' @description Cette fonction permet de visualiser le profil en long de tout élément de la desserte.
#'
#' @param shp = objet sf de type LINESTRING correspondant à une piste ou une route.
#' @param r = raster correspondant à un MNT.
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

DessertePL <- function(shp, r, pas = 25) {
  if (st_geometry_type(shp) != "LINESTRING") {
    stop("La fonction nécessite en entrée un objet de type LINESTRING")
  }

  shp <- shp %>% dplyr::select(geometry)
  # ----------- Echantillon points
  ecart = as.numeric(pas/st_length(shp))
  echan <- seq(0,1,by=ecart)
  echan <- c(echan[-length(echan)],1)
  pts <- st_line_sample(shp, sample=echan) %>%
    st_sf() %>%
    st_cast("POINT")

  # ----------- Pentes
  pts <- pts %>%
    mutate(Point = 1:dim(pts)[1],
           dist = echan*st_length(shp),
           dist = as.numeric(dist),
           Alti = raster::extract(r, as(., "Spatial")))

  profil <- pts %>%
    st_drop_geometry() %>%
    dplyr::select(Point,dist,Alti) %>%
    mutate(Pente = (lead(Alti)-lag(Alti))/(lead(dist)-lag(dist)),
           Pente = ifelse(is.na(lag(Alti)), (lead(Alti)-Alti)/(lead(dist)-dist), Pente),
           Pente = ifelse(is.na(lead(Alti)), (Alti-lag(Alti))/(dist-lag(dist)), Pente)) %>%
    pivot_longer(cols =c(Alti,Pente), names_to="variables", values_to="value")

  gprofil <- ggplot(profil, aes(x=dist, y=value)) +
    facet_wrap(~ variables, scales = "free_y", ncol=1) +
    geom_line() +
    theme_bw()

  return(gprofil)
}
