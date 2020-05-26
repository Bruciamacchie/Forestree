#' Ratio entre volume EMERGE et volume IFN
#'
#' @description La fonction Vifn_Vonf cartographie par sylvoécorégion le ratio entre le volume
#' EMERGE et le volume géométrique bois fort tige de l'IFN.
#'
#' @format Renvoie le ratio sous forme d'objet sf ou de carte.
#'
#' @param ess = essence selon la codification de l'IFN
#'
#' @import tidyverse
#' @import sf
#' @import rmapshaper
#'
#' @examples
#' df <- Tarif_EMERGE(df, df$Haut, df$C130)
#'
#' @author Martin Marie-Laure
#'
#' @source ONF
#'
#' @export

Vifn_Vonf <- function(ess = "09"){
  # ---- Test -------------
  ess <- ess[which(ess %in% CodesEssIFN$code)]
  if (length(ess) == 0) {
    stop("Les essences doivent être désignées par leur code IFN")
  }

  # ---- Données -------------
  data(IFNarbres)
  data(IFNplacettes)
  data(ser25)

  df <- IFNarbres %>%
    filter(espar %in% ess) %>%
    mutate(espar = as.character(espar)) %>%
    filter(!is.na(v)) %>%
    filter(!is.na(htot)) %>%
    filter(htot > 1.3) %>%
    filter(v > 0.02)

  df <- Tarif_EMERGE(df, df$htot, df$c13) %>%
    left_join(IFNplacettes, by = c("idp", "Annee")) %>%
    filter(!is.na(ser)) %>%
    mutate(Vifn = v*w,
           Vonf = VTot*w) %>%
    group_by(ser, espar, libelle) %>%
    summarise(Vifn = sum(Vifn),
              Vonf = sum(Vonf)) %>%
    ungroup() %>%
    mutate(ratio = Vonf / Vifn,
           codeser = as.character(ser)) %>%
    dplyr::select(codeser,libelle,ratio)


  ser <- ser25 %>%
    group_by(codeser) %>%
    summarise()

  df <- df %>%
    left_join(ser, by = "codeser") %>%
    st_sf()

  g <- ggplot() +
    geom_sf(data=ser, fill='grey60', color='grey80', lwd=0.5) +
    geom_sf(data=df, aes(fill=ratio), color='grey90', lwd=0.5) +
    scale_fill_gradient(low = "white", high = "green4", na.value = "grey60") +
    theme_bw() +
    facet_wrap(~ libelle, ncol=2)

  out=list(ser, g)
  names(out) <- c("ser", "g")
  return(out)
}
