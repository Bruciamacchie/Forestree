#' Valeur des forêts
#'
#' @description Les valeurs des forêts sont issues de la base  DVF (Demandes de valeurs foncières)
#' du site data.gouv.fr. Les deux département de l'Alsace ainsi que la Moselle ne figurent pas dans la
#' base de données. Les DOM-TOM ont été retirés de l'analyse.
#'
#' @param an = année. Ce paramètre n'accepte actuellement que les années 2016, 2017 et 2018.
#' @param minSurf = seuil minimal de surface en ha. Par défault minSurf = 1 ha.
#' @param maxPrix = seuil maximal de prix à l'hectare. Par défault maxPrix = 50000 euro/ha.
#'
#' @import tidyverse
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' \donttest{
#' library(tidyverse)
#'
#' Evol <- data.frame()
#' Detail <- data.frame()
#' for (i in c(2018:2016)) {
#'   res  <- ValeurVenale(i)
#'   Evol <- rbind(Evol, as.data.frame(res$tab))
#'   Detail <- rbind(Detail, as.data.frame(res$Detail))
#' }
#'
#' textes <- Evol %>% filter(Année == 2018)
#' ggplot(Evol, aes(x=Nature, y=Prix, color=factor(Année))) +
#'   geom_point() + geom_text(data=textes, aes(label=round(Prix,0)), size=4, hjust=1.2) +
#'   theme_bw() + labs(color="")
#'
#' ggplot(Detail, aes(x=Nature, y=Prix, fill=factor(Année), color=factor(Année))) +
#' geom_point(position = position_jitterdodge()) +
#'   geom_boxplot(alpha=0.4) +
#'   geom_text(data=textes, aes(label=round(Prix,0)), size=3, hjust=-1.8) +
#'   theme_bw() + labs(fill="", color="", y="Prix (euro/ha)")
#' }
#
#' @export

ValeurVenale <- function(an, minSurf=1, maxPrix=50000) {
  if (is.numeric(an) & (an <=2018 & an >= 2014)){
    liens = data.frame(Année = c(2018:2014),
                       url = c("https://www.data.gouv.fr/fr/datasets/r/1be77ca5-dc1b-4e50-af2b-0240147e0346",
                               "https://www.data.gouv.fr/fr/datasets/r/7161c9f2-3d91-4caf-afa2-cfe535807f04",
                               "https://www.data.gouv.fr/fr/datasets/r/0ab442c5-57d1-4139-92c2-19672336401c",
                               "https://www.data.gouv.fr/fr/datasets/r/09f013c5-9531-444b-ab6c-7a0e88efd77d",
                               "https://www.data.gouv.fr/fr/datasets/r/dc13282f-3c7a-4fac-b1f3-3939e39d45f6"))

    # ------------- Import
    print(paste("Traitement de l'année", an))
    pos <- which(liens$Année == an)
    url <- as.character(liens$url[pos])
    print("Recherche du lien internet")

    # t1 <- read_delim(url, delim="|")
    t1 <- read_delim(url, delim="|",locale=locale(decimal_mark = ","))
    print("Fin de l'import")

    tab <- t1 %>%
      rename(Date = `Date mutation`,
             Valeur = `Valeur fonciere`,
             Nature = `Nature culture`,
             NatureMut = `Nature mutation`,
             Surf = `Surface terrain`,
             CodePos = `Code postal`,
             NumDis = `No disposition`,
             Type = `Type local`) %>%
      filter(NatureMut == "Vente") %>%
      filter(CodePos <= 95000) %>%
      filter(!is.na(Nature)) %>%
      filter(!is.na(Valeur)) %>%
      mutate(ID = paste(CodePos, Date, Valeur, sep="_")) %>%
      mutate(Foret = ifelse(Nature %in% c("B","BF","BM","BP","BR","BS","BT","L","LB"),"Avec","Sans"))

    tabID <- tab %>%
      distinct(ID) %>%
      mutate(Iden = row_number())

    tab <- tab %>%
      left_join(tabID, by = "ID")

    ListeForet <- tab %>%
      dplyr::select(Iden, Foret) %>%
      filter(Foret == "Avec") %>%
      distinct(Iden)

    tab <- tab %>%
      filter(Iden %in% ListeForet$Iden) %>%
      dplyr::select(Iden,Foret,CodePos,NumDis,Nature,Type,Surf,Valeur) %>%
      mutate(Surf = Surf/10000) %>%
      # mutate(Valeur = as.numeric(Valeur)/100,
      #        Surf = as.numeric(Surf)/10000) %>%
      filter(!is.na(Surf))

    print("Fin premières mises en forme")

    print("Début des extractions")
    # ------------- Liste ventes de l'année
    ventes <- tab %>%
      as.data.table() %>%
      group_by(Iden) %>%
      summarise(Surf = sum(Surf),
                Valeur = mean(Valeur))

    # ------------- Liste ventes foret de l'année
    ventesForet <- tab %>%
      group_by(Iden, Foret) %>%
      summarise(nb = n()) %>%
      spread(Foret, nb, fill=0) %>%
      filter(Sans == 0) %>%
      dplyr::select(-Sans) %>%
      left_join(ventes, by = "Iden") %>%
      mutate(Prix = Valeur / Surf) %>%
      filter(Avec==1) %>%
      left_join(tab[,c("Iden","CodePos","Nature")], by = "Iden") %>%
      filter(Surf >= minSurf) %>%
      filter(Prix <= maxPrix) %>%
      mutate(Année = an)
    print("Fin des extractions")

    # ------------- Valeurs foret de l'année
    valeurs <- ventesForet %>%
      group_by(Nature, Année) %>%
      summarise(Prix = median(Prix),
                Nbre = n())

    g <- ggplot(ventesForet, aes(x=Nature, y=Prix)) +
      geom_boxplot() +
      theme_bw() + ylim(0, 50000)
    print("Calcul sorties")

    # ------------- Sorties
    out <- list(valeurs, g, ventesForet)
    names(out) <- c("tab", "graph", "Detail")

    return(out)

  } else {cat("L'année doit être comprise entre 2014 et 2018")}
}
