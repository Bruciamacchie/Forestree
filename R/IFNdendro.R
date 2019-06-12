#' Informations dendrométriques déduites de l'IFN
#'
#' @description Calcul de la surface terrière des principales essences, de la structure en PER, PB, BM, GB et de
#' l'importance en BM + GB des deux essences contribuant le plus à la surface terrière totale. Toutes ces informations sont
#' accompagnées de leur coefficient de variation. Les calculs ont été faits en supposant que toutes les placettes
#' ont le même poids.
#' La fonction nécessite en entrée un shape correspondant au périmètre retenu : forêt, massif, sylvoécorégion, etc.
#'
#' @return La fonction renvoie un tableau des surfaces terrières : totale, par essence, par structure et
#' pour les BM + GB des 2 essences les plus représentées.
#'
#' @param shp = fichier géoréférencé sous forme de polygone(s).
#' @param TailleBuffer = Taille du buffer en mètre. Par défaut elle est égale à 450 m.
#' @param enreg = argument permettant de choisir l'enregistrement ou pas du tableau au format .csv.
#' Par défaut enreg=FALSE.
#'
#' @import dplyr
#' @import tools
#' @import tcltk
#' @import openxlsx
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' library(sf)
#' data(perim)
#' load("IFNdata.Rdata")
#' res <- IFNdendro(shp=perim)
#' res$tab        # Répartition de la surface terrière
#' res$Nb         # nombre de placettes IFN
#' res$perimetre  # périmètre retenu
#' res$placettes  # placettes IFN
#' plot(st_geometry(perim))
#' plot(st_geometry(res$placettes), add=T, pch=3, col='red')
#'
#' @export

IFNdendro <- function(shp = NULL, rep = NULL, TailleBuffer=450, enreg=F) {
  if (is.null(shp)) {
    file <- tk_choose.files(caption = "Choix du périmètre",
                            filters=matrix(c(".shp",".shp"),1,2, byrow = T))
    if (length(file) > 0) {
      rep = dirname(file)
      shp <- st_read(dsn=rep, layer=basename(file_path_sans_ext(file)), quiet=T)
    } else {
      print("Import du périmètre annulé")
      return()
      }
  }
    shp <- st_transform(shp, 2154)
    shp <- st_union(shp)
    zone <- st_buffer(shp, dist=TailleBuffer)
    ser <- st_transform(st_as_sf(ser), 2154)

    IFNpla = st_as_sf(IFNplacettes, coords = c("xl93", "yl93"), crs = 2154)
    placettes <- IFNpla[zone, ]
    # ------------------- Arbres
    Gha <- IFNarbres %>%
      filter(idp %in% placettes$idp) %>%
      filter(!is.na(w)) %>%
      dplyr::select(idp, espar,c13,w) %>%
      mutate(Diam = round(c13/pi,0),
             Classe=floor(c13/pi/5+0.5)*5,
             Gha = pi/40000*Diam^2*w,
             espar = as.character(espar)) %>%
      left_join(CodesEssIFN, by=c("espar"="code"))
    # ------------------- CV Total
    GhaPla <- Gha %>%
      group_by(idp) %>%
      summarise(Gtot = sum(Gha))
    n <- dim(placettes)[1]
    tab <- data.frame(Population = "Totale",
                      Gha = mean(GhaPla$Gtot),
                      Cv = sd(GhaPla$Gtot)/mean(GhaPla$Gtot))
    # ------------------- CV essences principales
    GhaEss <- Gha %>%
      # filter(espar %in% ListeEss) %>%
      group_by(idp, espar) %>%
      summarise(Gtot = sum(Gha)) %>%
      group_by(espar) %>%
      summarise(Moy = sum(Gtot)/n,
                Sd = ((n*sum(Gtot^2)-sum(Gtot)^2)/n/(n-1))^0.5,
                Cv = Sd/Moy)  %>%
      left_join(CodesEssIFN, by=c("espar"="code")) %>%
      arrange(desc(Moy))
    ListeEss <- GhaEss$espar
    GhaEss <- GhaEss %>%
      dplyr::select(libelle, Moy, Cv) %>%
      rename(Population = libelle,
             Gha = Moy)
    tab <- rbind(tab, GhaEss)
    # ------------------- CV categories diametre
    GhaPla <- Gha %>%
      mutate(Cat = cut(Classe, breaks=c(0, 17.5, 27.5, 47.5, 200),
                       labels=c("PER","PB","BM", "GB"))) %>%
      group_by(idp, Cat) %>%
      summarise(Gtot = sum(Gha)) %>%
      group_by(Cat) %>%
      summarise(Moy = sum(Gtot)/n,
                Sd = ((n*sum(Gtot^2)-sum(Gtot)^2)/n/(n-1))^0.5,
                Cv = Sd/Moy)  %>%
      dplyr::select(Cat, Moy, Cv) %>%
      rename(Population = Cat,
             Gha = Moy)
    tab <- rbind(tab, GhaPla)

    # ------------------- CV GB+BM 2 essences principales
    GhaPla <- Gha %>%
      mutate(Cat = cut(Classe, breaks=c(0, 17.5, 27.5, 47.5, 200),
                       labels=c("PER","PB","BM", "GB"))) %>%
      # filter(espar==ListeEss[1] & (Cat=="GB"| Cat=="BM")) %>%
      filter(espar %in% ListeEss[1:2] & (Cat=="GB"| Cat=="BM")) %>%
      mutate(Gha2 = Gha^2) %>%
      group_by(libelle) %>%
      summarise(Somme = sum(Gha),
                Somme2 = sum(Gha2),
                Moy = sum(Gha)/n,
                Sd = ((n*sum(Gha2)-sum(Gha)^2)/n/(n-1))^0.5,
                Sd2 = ((n*Somme2-Somme^2)/n/(n-1))^0.5,
                Cv = Sd/Moy)  %>%
      mutate(Population=paste(libelle, "GB + BM")) %>%
      dplyr::select(Population, Moy, Cv) %>%
      rename(Gha = Moy)
    tab <- rbind(tab, GhaPla)

    if (enreg){
      dir.create("OutIFN", showWarnings = F)
      repOut <- paste(getwd(), "OutIFN", sep="/")
      fichOut <- paste("OutIFN","Dendroifn.csv", sep="/")
      write.csv(tab, fichOut, row.names =F)
      print(paste("Les informations dendrométriques ont été enregistrées dans le répertoire : ", repOut))
    }

    # if (enreg) {
    #   wb <- createWorkbook()
    #   addWorksheet(wb, "data")
    #   writeData(wb, "data", tab)
    #   setwd(dirname(dirname(dirname(file))))
    #   dir.create("Out", showWarnings = F)
    #   saveWorkbook(wb, "Out/CV.xlsx", overwrite = T)
    #   print(paste0("Le résultat a été sauvegardé à l'adresse : ",getwd(),"/Out"))
    # }
    tab$Cv <- round(tab$Cv,2)
    tab$Gha <- round(tab$Gha,2)
    out <- list(tab, n, shp, placettes)
    names(out) <- c("tab","Nb","perimetre","placettes")
    return(out)
}
