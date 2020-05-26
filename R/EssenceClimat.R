#' Carte des essences par type de climat.
#'
#' @description Cartographie du volume d'une ou plusieurs essences par type de climat.
#'
#' @param ess = code d'une ou plusieurs essences selon la codification de l'IFN
#' @param laps = laps de temps sur lequel est calculée la moyenne des volumes. Il peut être compris entre 1 et 10,
#'  Par défaut les calculs portent sur les 7 dernières années.
#'
#' @import tidyverse
#' @import sf
#' @import DataForet
#'
#' @examples
#' library(sf)
#' library(tidyverse)
#' library(DataForet)
#' EssenceClimat(c("02","03","05","06"))
#' EssenceClimat(c("09","61","62","52"))
#' EssenceClimat("09")
#' EssenceClimat(c("10","11"))
#'
#' @author BRUCIAMACCHIE Max

#' @export EssenceClimat

EssenceClimat <- function(ess, laps = 7) {
  # ---- Test -------------
  ess <- ess[which(ess %in% CodesEssIFN$code)]
  if (length(ess) == 0) {
    stop("Les essences doivent être désignées par leur code IFN")
  }
  if (!laps %in% 1:10) {
    stop("Le laps de temps doit être compris entre 1 et 10.")
  }

  # ---- Données -------------
  data(IFNarbres)
  data(IFNplacettes)

  # ---- Ajout table attributaire ----------
  plac <- IFNplacettes %>%
    filter(!is.na(Type)) %>%
    dplyr::select(idp,Type, Annee)

  # ---- Nombre placettes
  Nb <- plac %>%
    group_by(Annee, Type) %>%
    summarise(Freq = n())

  tab <- IFNarbres %>%
    filter(espar %in% ess) %>%
    filter(Annee > max(Annee)-laps) %>% # Nouveau
    filter(!is.na(v)) %>%
    group_by(idp, Annee, espar) %>%
    summarise(Vha = sum(v*w)) %>%
    left_join(plac, by = c("idp", "Annee")) %>%
    filter(!is.na(Type)) %>%
    group_by(Type,Annee,espar) %>%
    summarise(Vha = sum(Vha)) %>%
    left_join(Nb, by = c("Annee", "Type")) %>%
    mutate(Vha = Vha / Freq) %>%
    group_by(Type,espar) %>%
    summarise(Vha = sum(Vha) / laps)

  TypoClimatEss <- TypoClimat %>%
    left_join(tab, by = "Type") %>%
    filter(!is.na(espar)) %>%
    left_join(CodesEssIFN, by=c("espar"="code"))


  g1 <- ggplot() +
    geom_sf(data=TypoClimat, color='grey80', lwd=0.5) +
    geom_sf(data = TypoClimatEss, aes(fill=Vha), color='grey80', lwd=0.5) +
    scale_fill_gradient(low = "white", high = "green4", na.value = "grey40") +
    facet_wrap(~ libelle, ncol=2) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())

  return(g1)
}

