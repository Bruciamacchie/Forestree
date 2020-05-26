#' Cartographie du volume d'une essence par sylvoécorégion.
#'
#' @description La fonction renvoie une carte de la France découpée en sylvoécorégion. La couleur des sylvoécorégions
#' est proportionnelle au volume moyen à l'hectare de l'essence retenue (rouge d'autant plus intense que
#' le volume à l'hectare est élevé). Les points bleus permettent de localiser les placettes IFN.
#' Leur densité permet d'estimer la précision du volume.
#'
#' @return La fonction renvoie une carte des sylvoécorégions avec un niveau de rouge proportionnel
#' au volume de l'essence retenue. Les sylvoécorégions qui ne contiennent pas l'essence retenue sont en gris foncé.
#' Les placettes IFN utilisées sont en bleu.
#'
#' @param ess = Code essence de l'IFN. Par défaut ce code est égal à "09". Il correspond au hêtre.
#'
#' @import tidyverse
#' @import sf
#' @import DataForet
#' @import ggthemes
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' library(tidyverse)
#' library(PPtools)
#' library(DataForet)
#' library(sf)
#' library(ggthemes)
#' #############
#' EssenceSer("09")
#'
#' @export
#'
# ----------- Selection essence
EssenceSer <- function(ess="09") {
  # ---- Test -------------
  ess <- ess[which(ess %in% CodesEssIFN$code)]
  if (length(ess) == 0) {
    stop("Les essences doivent être désignées par leur code IFN")
  }
  # ---- Données -------------
  data(IFNarbres)
  data(IFNplacettes)
  data(ser)

  t1 <- IFNarbres %>%
    filter(espar==ess) %>%
    group_by(idp) %>%
    summarise(Vol = sum(v*w))

  t3 <- IFNplacettes %>%
    left_join(t1, by="idp") %>%
    filter(!is.na(Vol))

  t2 <- IFNplacettes %>%
    dplyr::select(idp,ser) %>%
    left_join(t1, by = "idp") %>%
    mutate(Vol = replace(Vol, which(is.na(Vol)), 0)) %>%
    group_by(ser) %>%
    summarise(Vol = mean(Vol)) %>%
    rename(codeser = ser) %>%
    mutate(codeser = as.character(codeser))

  ser.f <- ser %>%
    left_join(t2, by = "codeser")

  g <- ggplot() +
    geom_sf(data=ser.f, aes(fill = Vol), colour = "gray50", size = 0.2) +
    scale_fill_gradient(low = "white", high = "darkred") +
    theme_map() + theme(legend.position="right") +
    geom_point(data=t3, aes(x=xl93, y=yl93), size=0.5, alpha=0.2, color="blue")

  return(g)
}
