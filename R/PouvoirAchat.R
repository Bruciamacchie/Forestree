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
#' data(PA)
#' Coeft <- PouvoirAchat(2018)
#'
#' @export

PouvoirAchat <- function(an) {
  if (is.numeric(an) & an <= 2019 & an >=1950 ) {
    data(PA)
    euro=6.55957
    tab <- PA %>% filter(Année <= an)
    valmax = tab$Infla[dim(tab)[1]]
    tab <- tab %>%
      mutate(Coefft=1,
             Coefft = ifelse(Année < 2002, 1/euro, Coefft),
             Coefft = ifelse(Année < 1960, 1/euro/100, Coefft),
             Infla = valmax/Infla,
             Coefft = Coefft * Infla) %>%
      dplyr::select(Année, Coefft)
    return(tab)
  } else { print("l'année doit être un entier compris entre 1950 et 2018.")}
}
