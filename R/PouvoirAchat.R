#' Pouvoir d'achat
#'
#' @description Calcul du tableau permet d'exprimer des flux financiers en euros constants.
#'
#' @return La fonction renvoie un data frame.
#'
#' @param an = année retenue
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' library(Forestree)
#' library(DataForet)
#'
#' Coeft <- PouvoirAchat(2018)
#'
#' @export

PouvoirAchat <- function(an) {
  data("INSEEpa")
  if (is.numeric(an) & an >=1950 & an <= max(INSEEpa$Année)) {

    euro=6.55957
    tab <- INSEEpa %>% filter(Année <= an)
    valmax = tab$Infla[dim(tab)[1]]
    tab <- tab %>%
      mutate(Coefft=1,
             Coefft = ifelse(Année < 2002, 1/euro, Coefft),
             Coefft = ifelse(Année < 1960, 1/euro/100, Coefft),
             Infla = valmax/Infla,
             Coefft = Coefft * Infla) %>%
      dplyr::select(Année, Coefft)
    return(tab)
  } else { print(paste0("l'année doit être un entier compris entre 1950 et ",
                       max(INSEEpa$Année), "."))}
}
