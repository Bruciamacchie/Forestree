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
  data(IFNarbres)
  data(IFNplacettes)
  data(ser)

  df <- IFNarbres %>%
    filter(espar %in% ess) %>%
    filter(!is.na(v)) %>%
    filter(!is.na(htot)) %>%
    filter(htot > 1.3) %>%
    filter(v > 0.02)

  df <- Tarif_EMERGE(df, df$htot, df$c13) %>%
    left_join(IFNplacettes, by = c("idp", "Annee")) %>%
    filter(!is.na(ser)) %>%
    mutate(Vifn = v*w,
           Vonf = VTot*w) %>%
    group_by(ser,Essence) %>%
    summarise(Vifn = sum(Vifn),
              Vonf = sum(Vonf)) %>%
    ungroup() %>%
    mutate(ratio = Vonf / Vifn,
           codeser = as.character(ser)) %>%
    dplyr::select(codeser,Essence,ratio)

  ser <- ser %>%
    group_by(codeser) %>%
    summarise() %>%
    ms_sim

  df <- df %>%
    left_join(ser, by = "codeser") %>%
    st_sf()

  g <- ggplot(df) +
    geom_sf(aes(fill=ratio), color='grey90', lwd=0.5) +
    scale_fill_gradient(low = "white", high = "green4", na.value = "grey80") +
    theme_bw() +
    facet_wrap(~ Essence, ncol=2)

  out=list(ser, g)
  names(out) <- c("ser", "g")
  return(out)
}
