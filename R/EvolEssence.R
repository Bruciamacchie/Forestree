#' Evolution de la répartition du volume d'une essence.
#'
#' @description Evolution de la répartition du volume d'une essence par classe de diamètre et par
#' région.
#'
#' @param ess = code d'une ou plusieurs essences selon la codification de l'IFN
#' @param group = l'argument group n'admet que les modalités greco (11 grandes régions écologiques)
#' ou climat (décomposition du territoire national en 8 climats). Par défaut l'argument group pend la valeur
#' "greco".
#
#' @examples
#' data(IFNarbres)
#' data(IFNplacettes)
#' data(ser)
#' EvolEssence(ess="05", group="climat")
#'
#' @author BRUCIAMACCHIE Max

#' @export EvolEssence

EvolEssence <- function(ess="09", group="greco") {
  if (!(group %in% c("greco", "climat"))) {
    stop(" l'argumen group n'admet que les modalités greco ou climat")
  }

  # ---- regroup POLYGON
  if(group=="greco") {
    shp <- ser %>%
      group_by(greco) %>%
      summarise() %>%
      st_sf() %>%
      rename(group = greco)
  }
  if(group=="climat") {
    shp <- TypoClimat %>%
      rename(group = Type)
  }

  # ---- Ajout table attributaire
  plac <- IFNplacettes %>%
    st_as_sf(coords=c("xl93", "yl93"), crs = 2154) %>%
    st_join(shp) %>%
    filter(!is.na(group)) %>%
    st_drop_geometry() %>%
    dplyr::select(idp,group, Annee)

  # ---- Nombre placettes
  Nb <- plac %>%
    group_by(Annee, group) %>%
    summarise(Freq = n())

  tab <- IFNarbres %>%
    filter(espar == ess) %>%
    filter(!is.na(v)) %>%
    mutate(classe = floor(c13/pi/5 + 0.5)*5,
           Vha = v*w) %>%
    group_by(idp, Annee, classe) %>%
    summarise(Vha = sum(Vha)) %>%
    left_join(plac, by = c("idp", "Annee")) %>%
    filter(!is.na(group)) %>%
    mutate(Période = ifelse(Annee < 2012, "2005-2011", NA),
           Période = ifelse(Annee > 2011, "2012-2018", Période)) %>%
    filter(!is.na(Période)) %>%
    group_by(group,Annee, Période, classe) %>%
    summarise(Vha = sum(Vha)) %>%
    left_join(Nb, by = c("Annee", "group")) %>%
    mutate(Vha = Vha / Freq) %>%
    group_by(group,Période,classe) %>%
    summarise(Vha = sum(Vha) / 7)

  g <- ggplot(tab, aes(x=classe, y=Vha, fill=Période, group=Période)) +
    facet_wrap(~ group, ncol=2, scales = "free") +
    geom_bar(stat='identity', position = "dodge") +
    theme_bw()

  return(g)
}
