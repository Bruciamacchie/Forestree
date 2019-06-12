#' Recherche des tarifs de cubage Schaeffer
#'
#' @description La fonction permet de trouver parmi les tarifs Schaeffer, ceux qui sont le plus adaptés au cubage d'un échantillon
#' d'arbres. La fonction nécessite en entrée un tableau contenant au moins 3 colonnes dénommées Essence, Diam et Vol
#'
#' @return La fonction renvoie un tableau qui fournit par essence le numéro de tarif Schaeffer rapide, intermédiaire,
#' lent ou très lent ainsi que les coeffoicients de variations associés, ainsi qu'un graphique permettant d’apprécier
#' la variabilité des numéros.
#'
#' @param df = tableau contenant au moins 3 colonnes nommées Essence, Vol et Diam.
#' @param enreg = argument permettant de choisir l'enregistrement ou pas du tableau au format .xlsx.
#' Par défaut enreg=FALSE.
#'
#' @import tcltk
#' @import openxlsx
#' @import tidyverse
#' @import Hmisc
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' \donttest{
#' ####### Utilisation
#' library(tcltk)
#' library(openxlsx)
#' file <- tk_choose.files(caption = "Choix du fichier contenant volumes",
#'                         filters=matrix(c(".xlsx",".xlsx"),1,2, byrow = T))
#'   if (length(file) > 0) {
#'     tab <- read.xlsx(file, sheet = 1)
#'     if (sum(c("Essence","Vol","Diam") %in% names(tab)) == 3) {
#'       res <- TarifFindSch(tab)
#'     } else {print("Le fichier doit contenir les colonnes Essence, Vol et Diam")}
#'   } else {print("Recherche tarifs annulée")}
#' ####### Analyse des résultats
#' res$tab
#' res$graph
#' }
#'
#' @export

TarifFindSch <- function(df, enreg=F) {
  df <- df %>%
    filter(Diam >= 17.5 & Diam <= 80) %>%
    mutate(numSchR = Vol/5*70000/(Diam-5)/(Diam-10)-8,
           numSchI = Vol/5*80000/(Diam-2.5)/(Diam-7.5)-8,
           numSchL = Vol/5*90000/Diam/(Diam-5)-8,
           numSchTL = Vol/5*101250/Diam^2-8)
  res <- df %>%
    group_by(Essence) %>%
    summarise_at(vars(numSchR:numSchTL), funs(wtd.mean(., Vol), wtd.var(., Vol)))

  res[,6:9] <- res[,6:9]^0.5/res[,2:5]
  names(res) <- c("Essence","SchR","SchI","SchL","SchTL","SchRcv","SchIcv","SchLcv","SchTLcv")
  res[,-1] <- round(res[,-1],2)

  if (enreg) {
    wb <- createWorkbook()
    addWorksheet(wb, "data")
    writeData(wb, "data", res)
    dir.create("Out", showWarnings = F)
    saveWorkbook(wb, "Out/NumTarifSch.xlsx", overwrite = TRUE)
    print(paste0("Le résultat a été sauvegardé à l'adresse : ",getwd(),"/Out"))
  }

  df.m <- df %>%
    dplyr::select(Essence, Diam, numSchR:numSchTL) %>%
    gather(Type, Num, -Essence, -Diam) %>%
    mutate(Type = factor(Type, levels=c("numSchR","numSchI","numSchL","numSchTL")))

  p <- ggplot(df.m, aes(x=Diam, y=Num)) + geom_point() + theme_bw() +
    facet_grid(Essence ~ Type) + geom_smooth(method='lm')
  out <- list(res, p)
  names(out) <- c("tab", "graph")
  return(out)
}

