#' Cartographie du rayonnement maximal sur un territoire
#'
#' @description Cette fonction nécessite en entrée le périmètre du territoire sous forme d'objet sf
#' (library sf) ainsi que son MNT. Elle renvoie un raster des écarts de rayonnement.
#'
#' @param shp = périmètre de la portion de territoire.
#' @param dem = raster correspondant au MNT du périmètre.
#' @param buffer = buffer à appliquer au périmètre. Par défaut buffer=100 m.
#'
#' @import raster
#' @import insol
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' \donttest{
#' library(sf)
#' library(tidyverse)
#' library(raster)
#' library(insol)
#'
#' data(razel)
#' r <- SiteMnt(razel)
#' rad <- Rayonnement(razel, r)
#'
#' plot(rad, axes=F, box=F)
#' plot(st_geometry(razel), add=T)
#' # writeRaster(rad, "Rad.tif", format="GTiff", overwrite=TRUE)
#' }
#
#' @export

Rayonnement <- function(shp, dem, buffer=100) {
  # ------- Position zone étude
  shp1 = shp %>% st_buffer(dist=buffer)
  Centre <- shp %>%
    st_centroid() %>%
    st_transform(4326) %>%
    st_coordinates()
  # ------- Decoupage raster
  crs(dem) = st_crs(shp)$proj4string
  dem = crop(dem, as(shp1, "Spatial"))
  # ------- Parametres
  demm = raster::as.matrix(dem) # transformation du MNT en matrice
  dl = res(dem)[1]
  height = mean(demm)
  visibility=40
  RH=20
  tempK=293
  tmz = 1 # fuseau horaire par rapport à Greenwich
  year = 2018
  month = 6
  day = 21
  timeh = 12
  jd = JDymd(year,month,day,hour=timeh)
  Iglobal = array(0,dim=dim(demm))
  deltat = 0.5
  lat = Centre[2]
  lon = Centre[1]
  dayl = daylength(lat,lon,jd,0) # durée du jour
  # ------- Calculs
  cgr = cgrad(dem) # calcul de la normale à chaque pixel
  for (srs in seq(dayl[1],dayl[2],deltat)){
    jd = JDymd(year,month,day,hour=srs)
    sv = sunvector(jd,lat,lon,tmz)
    hsh = hillshading(cgr,sv)
    sh = doshade(demm,sv,dl)
    zenith = sunpos(sv)[2]
    Idirdif = insolation(zenith,jd,height,visibility,RH,tempK,0.002,0.15)
    ## direct radiation modified by terrain + diffuse irradiation (skyviewfactor ignored)
    ## values in J/m^2
    Iglobal = Iglobal + (Idirdif[,1] * hsh + Idirdif[,2] )*3600*deltat
  }
  Iglobal = Iglobal*1e-6

  Iglobal = raster(Iglobal,crs=projection(shp))
  extent(Iglobal) = extent(dem)
  Iglobal = scale(Iglobal, scale=F)
  # Iglobal = Iglobal/max(values(Iglobal))
  Iglobal = mask(Iglobal, shp)
  return(Iglobal)
}
