#' Import Carte Etat-major
#'
#' @description Cette fonction importe pour un ou plusieurs départements la base de données carte de l'Etat-major de l'IFN.
#' Elle ne nécessite aucum paramètre.

#'
#' @import threadr
#' @import tcltk
#' @import rlist
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' ImportEM()
#'
#' @export
#'


ImportEM <- function() {
  rep = "ftp://BD_CARTO_ext:Oor4el8aeM3io0Ji@ftp3.ign.fr/BDCARTO_EM"
  liste <- list_files_ftp(rep)
  debut <- str_locate(liste, pattern ='D0')[,1]
  dept <- substr(liste, debut, debut + 3) %>%
    list.filter(!is.na(.))
  # dept <- substr(liste, 93, 96)
  Choix <- tk_select.list(as.character(dept), multiple = TRUE,
                          title = "Carte d'Etat major : choisir les départements")

  dir.create("ForetEM", showWarnings = FALSE)

  for (i in 1:length(Choix)) {
    url = liste[which(dept == Choix[i])]
    download_ftp_file(url, paste0("ForetEM/",Choix[i],".7z"), verbose = TRUE)
  }
}
