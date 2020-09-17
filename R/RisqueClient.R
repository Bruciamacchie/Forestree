#' Calcul du Taux Interne de Rentabilite
#'
#' @description Cette fonction calcule le temps nécessaire pour que la somme cumulée des cotisations
#' actualisées au taux donné soit égale au montant des dégâts.
#'
#' @param tauxAss = taux de l'assurance, fixé par défaut à 0.003.
#' @param tauxAct = taux d'actualisation, fixé par défaut à 0.03.
#' @param tauxSauv = taux de sauvegarde. Il correspond au pourcentage du capital
#' qui n'est pas atteint par l'aléa.
#' @param Cycle = temps de retour (en années) de l'aléa.
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' RisqueClient(0.003, 0.03, 0.2, 150)
#' RisqueClient(tauxSauv = 0.2, Cycle= 150)
#'
#' @export
#'

RisqueClient <- function(tauxAss=0.003, tauxAct=0.03, tauxSauv, Cycle) {
  n      <- 1000000
  seuil  <- round(log((1-tauxSauv)*tauxAct/tauxAss+1)/log(1+tauxAct),0)
  risque <- round(sum(rexp(n, 1/Cycle) > seuil)/n, 2)
  c(seuil,risque)
}
