#'Tarifs de cubage Schaeffer2
#'
#' @description Calcul du volume d'un arbre ou d'une grume à l'aide d'un tarif Schaeffer à 2 entrées.
#'
#' @return La fonction renvoie le volume calculé avec un tarif Schaeffer à deux entrées. Le résultat est exprimé en m3.
#'
#' @param num = numéro de tarif. Ce numéro doit être l'un des suivants :
#' c(1,2,3,4.1,4.2,5.1,5.2,6,7,8.1,8.2,9,10).
#' @param diam = diamètre à 1,3 m (en cm).
#' @param haut = hauteur découpe (en m);
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' \donttest{
#' Hauts <- c(13,14,14)
#' Diams = c(45,45,50)
#' TarifSch2(2, Diams, Hauts)
#' }
#' @export

# ------ Schaeffer2 -----------------
TarifSch2 <- function(num=2, diam=45, haut=13) {
  if ((length(diam)+length(haut)) ==2*length(diam)) {
    if(class(num)=="numeric"&class(diam)=="numeric"&class(haut)=="numeric") {
      param = data.frame(Num = c(1,2,3,4.1,4.2,5.1,5.2,6,7,8.1,8.2,9,10),
                         a = c(100,97,98,100,86.8,96,82.8,93,92,93,82,80,92),
                         b = c(1.1,1,1.5,1.8,0.7,1.8,0.7,0.5,0.5,1.6,0.5,0.25,1.5),
                         k = c(rep(0,7),0.03,0.05,rep(0,4)))
      if (num %in% param$Num) {
        a <- param[which(param$Num == num), "a"]
        b <- param[which(param$Num == num), "b"]
        k <- param[which(param$Num == num), "k"]
        df <- data.frame(Diams=diam, Hauts=haut) %>%
          mutate(Vol = pi/40000*(a - b*Hauts - k*pi*Diams)^2/10000*Diams^2*Hauts)
        return(df$Vol)
      } else {print("Le numéro de tarif ne se trouve pas dans la liste des numéros possibles.")}
    } else {print("les arguments doivent tous être numeric.")}
  } else {print("Les arguments doivent avoir même longueur.")}
}
