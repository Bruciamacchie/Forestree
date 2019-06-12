#' Accroissement sur le diamètre
#'
#' @description La fonction renvoie les accroissements sur le diamètre par essence et classe de diamètre
#' en utilisant la base de données arbres de l'IFN.
#' La fonction nécessite en entrée un shape correspondant au périmètre retenu : forêt, massif,
#' sylvoécorégion, etc.
#'
#' @return La fonction renvoie un tableau des accroissements sur le diamètre par essence et classe de diamètre.
#' Elle intègre les coefficients d'écorce, les accroissements sont donc sur écorce.
#'
#' @param file = nom du fichier au format shp. Si NULL ouverture d'une boîte de dialogue
#' @param TailleBuffer = Taille du buffer en mètre. Par défaut elle est égale à 3000 m.
#' @param SeuilCircf = seuil minimal de circonférence en cm en dessous duquel l'arbre n'est pas retenu
#' pour le calcul du tarif. Par défaut ce seuil est fixé à 50 cm.
#' @param SeuilNb = seuil minimal du nombre d'arbres dans la base IFN pour qu'une essence soit retenue.
#' Par défaut ce seuil est fixé à 10 tiges.
#' @param res = résolution (par défaut = 1000 m)
#' @param UseSer = argument permettant de choisir si le calcul des numéros de tarif doit se faire
#' au sein d'une même sylvoécorégion. Par défaut, UseSer=TRUE.
#' @param enreg = argument permettant de choisir l'enregistrement ou pas du tableau au format .csv.
#' Par défaut enreg=FALSE.
#'
#' @import tidyverse
#' @import sf
#' @import tools
#' @import tcltk
#' @import sp
#' @import rgeos
#' @import raster
#'
#' @author Bruciamacchie Max
#'
#' @examples
#' data(perim)
#' res <- IFNacctD(file=perim, TailleBuffer=500)
#' res$tab
#' res$Effectif
#' res$Graph
#' @export

IFNacctD <- function(file=NULL, TailleBuffer=10000, SeuilCircf=50, SeuilNb=10, UseSer=T, enreg=F) {
  if(is.null(file)) {
    file <- tk_choose.files(caption = "Choix du périmètre",
                            filters=matrix(c(".shp",".shp"),1,2, byrow = T))
    if (length(file) > 0) {
      perimetre <- st_read(dsn=dirname(file), layer=basename(file_path_sans_ext(file)), quiet=T)
    } else {
      print("Import du périmètre annulé")
      return()
    }
  } else {
    perimetre <- file
    perimetre <- st_transform(perimetre, 2154)
    perimetre <- st_union(perimetre)
    zone      <- st_buffer(perimetre, dist=TailleBuffer)
    if (UseSer) {
      data(ser)
      ser <- st_as_sf(ser)
      ser <- st_transform(ser, 2154)
      zone <- st_intersection(zone, ser)
    }
    # data("IFNplacettes")
    placettes <- st_as_sf(IFNplacettes,
                          coords = c("xl93", "yl93"),
                          crs = st_crs(ser))
    placettes <- st_intersection(placettes, zone)

    load("IFNdata.Rdata")
    AcctDs <- IFNarbres %>%
      filter(idp %in% placettes$idp) %>%
      filter(veget=="0" & tige!=7 & mortb<=2 & acci==0) %>%
      filter(!is.na(ir5) & c13 >=SeuilCircf) %>%
      dplyr::select(espar,c13,ir5) %>%
      mutate(Diam = round(c13/pi,0)) %>%
      mutate(Classe=floor(c13/pi/5+0.5)*5) %>%
      mutate(espar = as.character(espar)) %>%
      left_join(CodesEssIFN, by=c("espar"="code")) %>%
      left_join(Ecorces[,c(1,4)], by=c("espar"="codeIFN")) %>%
      mutate(AcctD = round(ir5*2/50 /(1-2*pi*b/1000),3))

    df <- AcctDs %>%
      group_by(espar) %>%
      dplyr::summarise(Freq = n()) %>%
      filter(Freq >= SeuilNb) %>%
      dplyr::rename(code = espar) %>%
      left_join(CodesEssIFN, by = "code") %>%
      arrange(desc(Freq)) %>%
      dplyr::select(code,libelle,Freq)


    t2 <- AcctDs %>%
      filter(espar %in% df$code)

    local=1
    p <- ggplot(t2, aes(x=Diam, y=AcctD)) +
      geom_point() + geom_smooth(span=local) +
      facet_wrap(~ libelle, ncol=3) +
      theme_bw() +
      coord_cartesian(xlim = c(10, 90))

    ListeEss <- unique(t2[,c("espar","libelle")])
    Xnew <- data.frame(Diam = seq(20,100,5))

    tab <- Xnew
    for(i in 1:dim(ListeEss)[1]) {
      t3 <- subset(t2, espar==ListeEss[i,1])
      model <- loess(AcctD ~ Diam, data = t3, span=local)
      res <- data.frame(round(predict(model, newdata = Xnew),2))
      names(res) <- ListeEss[i,2]
      tab <- cbind(tab, res)
    }

    if (enreg){
      dir.create("OutIFN", showWarnings = F)
      repOut <- paste(getwd(), "OutIFN", sep="/")
      fichOut <- paste("OutIFN","AcctDifn.csv", sep="/")
      write.csv(tab, fichOut, row.names =F)
      print(paste("Les accroissements sur le diamètre ont été enregistrés dans le répertoire : ", repOut))
    }

    # if (enreg) {
    #   wb <- createWorkbook()
    #   addWorksheet(wb, "data")
    #   writeData(wb, "data", tab)
    #   setwd(dirname(dirname(dirname(file))))
    #   dir.create("Out", showWarnings = F)
    #   saveWorkbook(wb, "Out/AccD.xlsx", overwrite = T)
    #   print(paste0("Le résultat a été sauvegardé à l'adresse : ",getwd(),"/Out"))
    # }
    out <- list(tab, df, p, file)
    names(out) <- c("tab", "Effectif", "Graph", "file")
    return(out)
  }

}
