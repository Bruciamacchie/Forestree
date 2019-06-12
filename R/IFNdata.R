#' Rechargement des donnees IFN
#'
#' @encoding UTF-8
#'
#' @description Cette fonction permet de télécharger les données brutes de l'IFN à partir du site
#' http://inventaire-forestier.ign.fr/spip/IMG/zip/.
#'
#' @return Cette fonction enregistre dans une archive IFNdata.Rdata deux tables : IFNarbres et IFNplacettes.
#'
#' @param debut = Première année pour laquelle on souhaite obtenir des données.
#' Attention, l'année de début doit être antérieure à 2008
#' @param fin = Dernière année pour laquelle on souhaite obtenir des données
#'
#' @import tidyverse
#' @import sf
#' @import data.table
#'
#' @examples IFNdata(2011,2016)
#'
#'@author HIGUET Valentin, BRUCIAMACCHIE Max

#' @export IFNdata
#'

IFNdata <- function (debut, fin) {
  if(is.numeric(debut) & is.numeric(debut)) {
    debut = as.integer(debut)
    fin   = as.integer(fin)

    IFNarbres <- data.table()
    IFNplacettes <- data.table()

    rep <- "http://inventaire-forestier.ign.fr/spip/IMG/zip/"
    for (i in fin:debut){
      tempRep <- tempdir()
      temp <- tempfile()
      repTour <- paste0(rep,i,".zip")
      # file <- basename(repTour)
      download.file(repTour, temp)
      liste <- unzip(temp, exdir=tempRep)
      tabArbres <- read.csv2(liste[grepl("arbres_foret", liste)])
      tabArbres$Annee <- i
      tabPlacettes <- read.csv2(liste[grepl(paste("placettes_foret_",i,sep=""), liste)])
      tabPlacettes$Annee <- i
      # IFNarbres <- bind_rows(IFNarbres, tabArbres)
      # IFNplacettes <- bind_rows(IFNplacettes, tabPlacettes)

      IFNarbres <- rbindlist(list(IFNarbres, tabArbres), use.names=TRUE, fill=TRUE)
      IFNplacettes <- rbindlist(list(IFNplacettes, tabPlacettes), use.names=TRUE, fill=TRUE)
      unlink(temp); unlink(tempRep)
    }
    IFNarbres$ir5 <- as.numeric(as.character(IFNarbres$ir5))
    IFNarbres$v   <- as.numeric(as.character(IFNarbres$v))
    IFNarbres$w   <- as.numeric(as.character(IFNarbres$w))

    IFNarbres <- IFNarbres %>%
      dplyr::select(idp,a,espar,veget,tige,mortb,acci,ori,mortb,c13,ir5,htot,hdec,v,w,Annee)
    IFNplacettes <- IFNplacettes %>%
      dplyr::select(idp,xl93,yl93,ser,csa,dc,dist,esspre,Annee)

    save(IFNarbres, IFNplacettes, file="IFNdata.Rdata")
  } else{ cat("Les paramètres début et fin doivent être des entiers")}

}
