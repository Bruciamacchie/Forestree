###########################################################################################################
#####                                    Fonction ClimatOmbro                                         #####
###########################################################################################################
#' Diagramme ombrothermique
#'
#' @description Edition d'un diagramme ombrothermique à partir d'informations mensuelles sur les températures
#' et les précipitations.
#'
#' @return La fonction renvoie graphique au format ggplot.
#'
#' @import ggplot2
#'
#' @param temp = vecteur contenant les températures
#' @param precip = vecteur contenant les précipitations
#'
#' @examples
#' Precipitation = c(92,82,70,64,49,38,29,31,46,68,96,106)
#' Temperature = c(6.4,7.6,9.6,11.6,15.4,18.3,20.8,20.9,18.1,14.2,9.4,7.3)
#' ClimatOmbro(Precipitation, Temperature)
#'
#' @author Bruciamacchie Max et étudiants FIF
#'
#' @export


ClimatOmbro <- function(temp, precip) {
  Mois = c("Jan.","Fev.","Mar.","Avr.","Mai.","Juin","Jui.","Aou.","Sep.","Oct.","Nov.","Dec.")
  tab <- data.frame(Mois, precip, temp)
  tab$Mois <- factor(tab$Mois, levels=Mois)

  p <- ggplot(tab, aes(x = Mois)) +
    geom_area(aes(y = temp*2, group = 1), fill = "yellow") +
    geom_bar(aes(y = precip), stat='identity', fill='lightblue', color='blue') +
    geom_line(aes(y = temp*2, group = 1), color = "red") +
    geom_point(aes(y = temp*2), colour = "red") +
    scale_y_continuous(sec.axis = sec_axis(~./2, name = "Température")) +
    labs(x="", y="Précipitation") + theme_bw() +
    theme(axis.text.y = element_text(color = "blue")) +
    theme(axis.title.y = element_text(color = "blue")) +
    theme(axis.text.y.right = element_text(color = "red")) +
    theme(axis.title.y.right = element_text(color = "red"))
  return(p)
}



###########################################################################################################
#####                                    Fonction ClimatOmbroMF                                       #####
###########################################################################################################

#' Diagramme ombrothermique données météoFrance
#'
#' @description Edition d'un diagramme ombrothermique à partir d'un tableau contenant les informations
#' classiques de météoFrance : Station, Année, Mois, RR (Précipitations), TN (témpérature minimale) et
#' TX (température maximale). Si le tableau en entrée contient plusieurs stations, une boîte de dialogue
#' permet d'en sélectionner une seule.
#'
#' @return La fonction renvoie graphique au format ggplot.
#'
#' @import ggplot2
#' @import tcltk
#' @import gtable
#'
#' @param df = tableau contenant les informations météo
#'
#' @examples
#' ##### Données
#' data("DataMeteo")
#' ##### Visualisation
#' head(DataMeteo)
#' ##### Utilisation fonction
#' ClimatOmbroMF(DataMeteo)
#'
#' @author Bruciamacchie Max et étudiants FIF
#'
#' @export

ClimatOmbroMF <- function(df){
  if (class(df) == "data.frame") {
    if (sum(c("Station","Annee","Mois","RR","TN","TX") %in% names(df))==6) {
      #------------ choix de la station -----------------
      ListeStations <- unique(df$Station)
      if (length(ListeStations) > 1) {
        Choix <- tk_select.list(ListeStations,
                                title = "Choisir une station météo")
      } else {
        Choix <- ListeStations[[1]]
      }

      NomMois = c("Jan.","Fev.","Mar.","Avr.","Mai.","Juin","Jui.","Aou.","Sep.","Oct.","Nov.","Dec.")

      #------------ extraction et moyennes mensuelles -----------------
      t2 <- df %>%
        filter(Station == Choix) %>%
        mutate(TM = round((TN + TX)/2,2)) %>%
        dplyr::select(Station,Mois,RR,TM) %>%
        group_by(Station,Mois) %>%
        summarise_all(funs(mean)) %>%
        arrange(Mois) %>%
        mutate(Mois = NomMois)

      return(ClimatOmbro(t2$TM, t2$RR))
    } else {
      print("Le fichier en entrée doit contenir les colonnes Station, Annee, Mois, RR, TN, TX")
    }
  } else {
    print("Le fichier en entrée doit être un data frame")
  }
}





