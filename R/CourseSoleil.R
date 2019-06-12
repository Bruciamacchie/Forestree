#' Course du soleil
#'
#' @description Cette fonction renvoie la hauteur du soleil exprimée en degré au premier jour du printemps
#' (20 mars) et de l'été (20 juin).
#'
#' @param Lat = latitude.
#' @param Lon = longitude.
#'
#' @import tidyverse
#' @import suncalc
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' \donttest{
#' library(tidyverse)
#' library(suncalc)
#'
#' res <- CourseSoleil(Lat = 50.1, Lon = 1.83)
#' res$graph
#' }
#
#' @export

CourseSoleil <- function(Lat, Lon) {
  Ete <- getSunlightTimes(date = as.Date("2019-06-20"), lat = Lat, lon = Lon,
                          keep = c("sunriseEnd", "sunsetStart"), tz = "UTC")
  Printemps <- getSunlightTimes(date = as.Date("2019-03-20"), lat = Lat, lon = Lon,
                                keep = c("sunriseEnd", "sunsetStart"), tz = "UTC")

  tab1 <- getSunlightPosition(date = seq(Ete$sunriseEnd, Ete$sunsetStart, length.out=100),
                              lat = Lat, lon = Lon) %>%
    mutate(Saison = "Eté")

  tab2 <- getSunlightPosition(date = seq(Printemps$sunriseEnd, Printemps$sunsetStart, length.out=50),
                              lat = Lat, lon = Lon) %>%
    mutate(Saison = "Printemps")

  tab <- rbind(tab1, tab2) %>%
    mutate(altitude = altitude/pi*180,
           azimuth = azimuth/pi*200 + 200)

  g <- ggplot(tab, aes(x=azimuth, y=altitude, group=Saison)) +
    geom_line(aes(linetype=Saison)) + theme_bw() +
    theme(legend.position="top") + labs(linetype="", y="Hauteur (°)")

  out <- list(tab, g)
  names(out) <- c("tab", "graph")
  return(out)
}
