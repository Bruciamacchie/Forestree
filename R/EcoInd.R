#' Calcul du Taux Interne de Rentabilite
#'
#' @description Cette fonction calcule le taux interne de rentabilite d'une série de cashflows.
#'
#' @param age = vecteur d'années d'apparition des flux financiers.
#' @param Montant = vecteur des montants annuels des différents flux financiers. Il s'agit de montants nets
#' (recettes - dépenses). Ce vecteur doit forcément contenir des valeurs négatives et positives.
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' df <- data.frame(Année = c(2, 4, 6, 30),
#'                  Montant = c(-0.6, -0.9, -1.5, 9))
#' EcoTIR(df$Montant, df$Année)
#'
#' @export
#'

EcoTIR <- function (age, cash) {
  f <- function(r, Montant, Annee) sum(Montant/(1+r)^Annee)
  TIR = uniroot(f, lower = 0, upper = 1, tol = 1e-9, Montant=cash, Annee=age)$root
  return(TIR)
}

