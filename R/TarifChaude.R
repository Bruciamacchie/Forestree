#'Tarifs de cubage Chaudé
#'
#' @description Calcul du volume d'un arbre ou d'une grume à l'aide d'un tarif Chaudé.
#'
#' @return La fonction renvoie le volume calculé avec un tarif Chaudé à deux entrées. Le résultat est exprimé en m3.
#'
#' @param num = numéro de tarif. Ce numéro permet d'établir le lien avec la décroissance par classe de diamètre.
#' @param diam = diamètre à 1,3 m (en cm).
#' @param haut = hauteur découpe (en m);
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' \donttest{
#' Hauts <- c(13,14,14)
#' Diams = c(45,45,50)
#' TarifChaude(13, Diams, Hauts)
#' }
#' @export

# ------ Chaudé -----------------
TarifChaude <- function(num=13, diam=45, haut=13) {
  if ((length(diam)+length(haut)) ==2*length(diam)) {
    if(class(num)=="numeric"&class(diam)=="numeric"&class(haut)=="numeric") {
      df <- data.frame(Diams=diam, Hauts=haut)
      df$Classe <- floor(df$Diams/5+0.5)*5
      df2 <- decChaude[,c(1,num+1)]
      names(df2)[2] <- "delta"
      df <- merge(df, df2, all.x=T)
      x <- pi/40000*(df$Diams-df$delta*(df$Hauts/2-1.3))^2*df$Hauts
      x
    } else {print("les arguments doivent tous être numeric.")}
  } else {print("Les arguments doivent avoir même longueur.")}
}
