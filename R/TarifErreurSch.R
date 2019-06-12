#' Estimation de l'erreur dans l'utilisation d'un tarif Schaeffer
#'
#' @description La fonction calcule l'erreur liée à l'estimation du volume d'un lot par utilisation d'un tarif
#' Schaeffer
#'
#' @return La fonction renvoie un tableau qui fournit par type de tarif Schaeffer rapide, intermédiaire,
#' lent ou très lent le numéro à utiliser, le volume du lot accompagné de son erreur d'estimation.
#'
#' @param df1 = data.frame correspondant à l'échantillon d'arbres sur lequel les numéros de tarif Schaeffer
#' seront recherchés. Ce tableau doit contenir au moins 2 colonnes nommées Diam et Volume.
#' @param df2 = data.frame correspondant au lot d'arbres à cuber. Ce second tableau doit contenir au moins
#' 2 colonnes nommées Diam et Nombre.
#'
#' @import dplyr
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' data(Echan)
#' data(Lot)
#' TarifErreurSch(Echan,Lot)
#' }
#'
#' @export

TarifErreurSch <- function(df1, df2) {
  # -------- Echantillon -----------
  nbre = dim(df1)[1]
  t1 <- df1 %>%
    mutate(Type = "SchR",
           I = Volume*70000/5/(Diam-10)/(Diam-5))
  t2 <- df1 %>%
    mutate(Type = "SchI",
           I = Volume*80000/5/(Diam-7.5)/(Diam-2.5))
  t3 <- df1 %>%
    mutate(Type = "SchL",
           I = Volume*90000/5/(Diam-5)/Diam)
  t4 <- df1 %>%
    mutate(Type = "SchTL",
           I = Volume*101250/5/Diam^2)
  Echan <- rbind(t1,t2,t3,t4) %>%
    group_by(Type) %>%
    summarise(Moy = mean(I),
              Sd = sd(I))
  # -------- Lot -----------
  t1 <- df2 %>%
    mutate(Type = "SchR",
           FD = 5/70000*(Diam-10)*(Diam-5))
  t2 <- df2 %>%
    mutate(Type = "SchI",
           FD = 5/80000*(Diam-7.5)*(Diam-2.5))
  t3 <- df2 %>%
    mutate(Type = "SchL",
           FD = 5/90000*(Diam-5)*Diam)
  t4 <- df2 %>%
    mutate(Type = "SchTL",
           FD = 5/101250*Diam^2)
  # -------- Calcul -----------
  tab <- rbind(t1,t2,t3,t4) %>%
    left_join(Echan[,c("Type","Moy")], by = "Type") %>%
    mutate(FDi = FD*Nombre,
           FD2i = FD^2*Nombre,
           Volume = FD * Moy * Nombre) %>%
    group_by(Type) %>%
    summarise(FD = sum(FD),
              FDi = sum(FDi),
              FD2i = sum(FD2i),
              Volume = sum(Volume),
              Num = mean(Moy)-8)  %>%
    left_join(Echan[,c("Type","Sd")], by = "Type") %>%
    mutate(Variance = Sd^2*(FDi^2/nbre + FD2i),
           Erreur = qt(0.975, nbre-1)*Variance^0.5/Volume) %>%
    dplyr::select(Type, Num, Volume, Erreur)
  return(tab)
}
