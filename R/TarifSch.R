#'Tarifs de cubage Schaeffer
#'
#' @description Calcul du volume d'un arbre ou d'une grume à l'aide d'un tarif Schaeffer à une entrée.
#'
##'
#' @return La fonction renvoie le volume calculé avec un tarif Schaeffer à une entrée, que ce soit un tarif
#' rapide, intermédiaire, lent ou très lent. Le résultat est exprimé en m3.
#'
#'
#' @param type = type de tarif. Par défaut type = "SchR", sinon "SchI", "SchL" ou "SchTL"
#' @param num = numéro de tarif.
#' @param diam = diamètre à 1,3 m (en cm).
#'
#' @examples
#' \donttest{
#' # -------- Tarifs Schaeffer
#' Types = c("SchR","SchR","SchL")
#' Types2 = c("SchR","Sch","SchL")
#' Nums = c(9,8,9)
#' Diams = c(45,45,50)
#' TarifSch(Types,Nums,Diams)
#' TarifSch(Types2,Nums,Diams)
#' }
#'
#' @author Bruciamacchie Max
#'
#' @export

# ------ Schaeffer -----------------
TarifSch <- function(type="SchR", num=9, diam=45) {
  if ((length(type)+length(num)+length(diam)) ==3*length(type)) {
    if(class(type)=="character"&class(num)=="numeric"&class(diam)=="numeric") {
      x <- rep(NA,length(type))
      pos <- which(type=="SchR")
      if (length(pos) > 0) {x[pos] <- 5/70000*(8+num[pos])*(diam[pos]-5)*(diam[pos]-10)}
      pos <- which(type=="SchI")
      if (length(pos) > 0) {x[pos] <- 5/80000*(8+num[pos])*(diam[pos]-2.5)*(diam[pos]-7.5)}
      pos <- which(type=="SchL")
      if (length(pos) > 0) {x[pos] <- 5/90000*(8+num[pos])*diam[pos]*(diam[pos]-5)}
      pos <- which(type=="SchTL")
      if (length(pos) > 0) {x[pos] <- 5/101250*(8+num[pos])*diam[pos]^2}
      x
    } else {print("les arguments doivent être character, numeric et numeric.")}
  } else {print("Les arguments doivent avoir même longueur.")}
}

