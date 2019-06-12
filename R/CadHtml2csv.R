#' Lecture matrices cadastrales .html
#'
#' @description Lecture de matrices cadastrales au format html contenues dans un dossier,
#' construction d'un tableau contenant le nom de la commune, la section, le numéro de parcelle,
#' la nature d'occupation du sol et la surface.
#'
#' @return La fonction retourne un data frame et permet de l'enregistrer au format csv.
#'
#' @author Bruciamacchie Max & Chevereau Matthieu
#'
#' @param enrg = si ce paramètre est TRUE, le tableau est enregistré au format .csv
#'
#' @import XML
#' @import RCurl
#' @import rlist
#' @import tcltk
#' @import dplyr
#' @import openxlsx
#' @import stringr
#'
#' @examples
#' \donttest{
#' ####### Utilisation
#' tab <- CadHtml2csv(enrg=TRUE)
#' }
#'
#' @export


CadHtml2csv <- function(enrg=FALSE){

  # Sélection et lecture du répertoire d'entrée
  rep <- tk_choose.dir(caption = "Choisir le dossier contenant les extraits de matrice cadastrale")

  if(is.na(rep)) { # Si le répertoire est vide : Absence de traitement
    print("Traitement annulé")
  } else { # Si le répertoire est plein : Traitement en boucle
    ListeFich <- list.files(rep, "*.html") # Création d'une liste de fichier .html

    if(length(ListeFich > 0)){ # Si la liste contient au moins un fichiers
      print(paste0("Le dossier choisi contient ",length(ListeFich)," fichiers."))

      # Boucle de traitement
      sortie <- data.frame() # Création d'un dataframe vierge de sortie

      for (i in ListeFich) { # Pour chaque fichier .html de la liste
        urldata <- getURL(paste("file:/",rep,i, sep="/")) # Chargement du fichier .html

        ## Récupération des données générales
        ### Lecture des listes d'en-tête
        data <- readHTMLTable(urldata, header=F, stringsAsFactors = F) # Chargement des listes d'en-têtes
        data <- list.clean(data, fun = is.null, recursive = F)         # Suppression des listes vides

        ### Traitement sur la commune
        Commune    <- data[[1]][1, 6]  # Récupération de cellule commune
        Com_Nom  <- str_sub(Commune,5) # Récupération du nom de la commune
        Com_Code <- as.numeric(str_sub(Commune,1,3)) # Récupération du code la commune
        Com_Code <- str_pad(Com_Code, 3, "left", pad = "0")

        ### Traitement sur l'adresse du/des propriétaires
        COORD_list <- list.clean(data[[2]][, 1], fun = is.na, recursive = T) # Sélection de la liste contenant les adresses + statuts propriétaires
        Coord <- COORD_list[seq(2,length(COORD_list),2)]   # Sélection des cellules contenants les adresse
        Coord <- paste(Coord,collapse = " & ")             # Concaténation des adresses dans une même variable
        Coord <- str_replace_all(Coord, "Â", "")           # Supression des caractères de Â
        Coord <- str_replace_all(Coord, "[:blank:]+", " ") # Suppression des caractères vides

        ### Traitement sur l'adresse du/des propriétaires
        PROP_list <- list.clean(data[[2]][, 3], fun = is.na, recursive = T)  # Sélection de la liste contenant les propriétaires
        if(length(PROP_list)>1){                           # Quand la liste contient plusieurs noms
          Nom <- COORD_list[seq(1,length(COORD_list),2)]   # Sélection des cellules contenants les statuts des propriétaires
          Statut <- list()                                 # Création d'une liste vierge
          for (k in 1:length(PROP_list)) { Statut <- c(Statut,paste(PROP_list[k],"(",Nom[k],")"))} # Agrégation des noms avec leur statuts
          Nom <- paste(Statut,collapse = " & ")            # Concaténation des noms+statut dans une même variable
        } else {
          Nom <- paste(PROP_list,collapse = " & ")         # ?
        }

        ## Récupération des données cadastrales
        ### Lecture des listes du corps
        data <- readHTMLTable(urldata, stringsAsFactors = F)   # Chargement des listes concernées
        data <- list.clean(data, fun = is.null, recursive = F) # Suppression des listes vides

        df <- data.frame() # Création d'un tableau de données

        ### Sélection des colonnes nommées ci-dessous
        for (j in seq(4, length(data), by=2)){ # Sélection des données
          t1 <- as.data.frame(data[[j]]) %>%   # Inscription dans un dataframe
            dplyr::select(2,3,5,11,14,15) %>%  # Sélection des colonnes inétressantes
            filter(row_number() > 2)           # Sélection des lignes hors des entêtes

          df <- rbind(df, t1) # Mise du dataframe dans le précédent
        }
        names(df) <- c("SECTION","N_PARCA","LIEUDIT","NATURE","SURFACE","REV_CAD") # Changement des noms des champs

        ### Suppression des lignes dont la contenance contient des tags .html
        df <- df %>% filter(SURFACE !="Â")

        ### Inscription des données de la parcelle à ses subdivisions
        for (k in which(df[,1] == "Â")){
          df[k, 1:3] <- df[k-1, 1:3]
        }

        ### Inscription des données générales
        df <- df %>% filter(NATURE !="Â") %>% # Conservation des seules subdivisions fiscales
          mutate(COM_CODE = Com_Code,         # Création du champs COM_CODE
                 COM_NOM  = Com_Nom,          # Création du champs COM_NOM
                 PROP     = Nom,              # Création du champs PROP
                 ADRESSE  = Coord) %>%        # Création du champs ADRESSE
          dplyr::select(COM_CODE,COM_NOM,PROP,ADRESSE,SECTION:NATURE,REV_CAD,SURFACE) # Mise en ordre des champs


        ### Création de la dataframe de données des subdivisions fiscales
        tab <- data.frame()   # Création d'un tableau de données vierge
        tab <- rbind(tab, df) # Combinaison des données récoltées

        ### Si la matrice ne contient qu'une parcelle
        if (nrow(tab)<2){ # /!\ Génération d'une ligne fictive pour conservation du type numeric pour la variable surface
          tab[2,]<-"999"  # /!\ Conversion en unknow et traitement suivant impossible sinon
        }

        ## Traitement de la table (avec subdivision) ---
        ### Mise au format des variables SURFACES, REVCAD, N_PARCA + Création des variables PREFIXE et GROUPE
        tab <- tab %>%
          mutate(SURFACE   = as.numeric(paste(str_replace_all(SURFACE, " ", "")))/10000, # Conversion de SURF_CA en données numériques exprimées en Ha
                 REV_CAD   = as.numeric(gsub("[,]",".", REV_CAD)),     # Conversion de REV_CAD en données numériques
                 N_PARCA   = str_pad(N_PARCA, 4, "left", pad = "0"),   # Complément de N_PARCA avec "0"
                 PREFIXE   = "000",                                    # Création du champ PREFIXE
                 GROUPE    = "") %>%
          dplyr::select(COM_CODE:ADRESSE,PREFIXE,SECTION:LIEUDIT,GROUPE,NATURE:SURFACE) # Mise en ordre des champs

        ### Calcul des variables PREFIXE et SECTION
        for (k in 1:nrow(tab)){                            # Boucle sur les lignes
          if (grepl("[0-9]",tab[k,6])==T){                 # Lorsqu'un préfixe est détecté, séparation du préfixe et des champs
            tab[k,5] = str_split_fixed(tab[k,6]," ",2)[1]  # Déplacement du préfixe dans le champ dédié
            tab[k,6] = str_split_fixed(tab[k,6]," ",2)[2]  # Déplacement de la section dans le camps dédié
          }
          if (str_sub(tab[k,10],1,1)=="B"){                # Lorsque la subdivision fiscale est de type boisée
            tab[k,9] = "BOISEE" } else { tab[k,9]="NON_BOISEE" # Inscription de cetype dans GROUPE
            }
        }

        ### Mise au format des variables SECTION + Création de la variable d'identifiant unique
        tab <- tab %>%
          mutate(SECTION = str_pad(SECTION, 2, "left", pad = "0"),         # Complément de SECTION avec "0"
                 IDU = paste(COM_CODE,PREFIXE,SECTION,N_PARCA,sep="")) %>% # Création de l'identifiant unique
          dplyr::select(COM_CODE:ADRESSE,IDU,PREFIXE:SURFACE)           # Mise en ordre des champs

        # /!\ tab contient l'ensemble des données des subdivisions cadastrales. Les parcelles n'y sont pas présentes

        ## Création de la matrice finale (sans subdivisions)
        ### Création d'un tableau récapitualtif des SURFACE par GROUPE par IDU
        tab2 <- as.data.frame.matrix(xtabs(SURFACE ~ IDU + GROUPE, tab)) # dataframe SURFACE par GROUPE par IDU

        #### Ajout de la colonne manquante en cas d'absence de données
        if(ncol(tab2)<2){
          if(grepl("NON",names(tab2))) {  # Si la table ne contient qu'une colonne au lieu des deux attendus
            tab2$BOISEE<- as.numeric(0.0) # Création de la variable BOISEE
          } else {
            tab2$NON_BOISEE<- as.numeric(0.0)} # Création de la variable NON_BOISEE
        }

        #### Ajout de l'IDU + Calcul de la surface cadastrale + Création d'un champs d'occupation du sol
        tab2$IDU <- row.names(tab2) # Création du champ IDU dans la nouvelle table
        tab2$SURF_CA <- as.numeric(tab2$BOISEE+tab2$NON_BOISEE) # Détermination de la surface cadastrale totale SURF_CA
        tab2$OCCUP_SOL <- ""        # Création d'un champ équivalent à GROUPE

        #### Détermination de la nature de la propriété
        Seuil <- 0.5 # Proportion de surface au delà de laquelle la parcelle est considérée comme forestière
        for (k in 1:nrow(tab2)) {
          if ((as.numeric(tab2$BOISEE[k])/tab2$SURF_CA[k])>=Seuil){ # Lorsque la proportion de surface boisée dépasse 50% de la surface cadastrale totale
            tab2[k,5] = "BOISEE"                                    # la parcelle est jugée boisée
          } else { tab2[k,5] = "NON BOISEE"}                        # ou non boisée sinon
        }
        tab2 <- tab2[,-c(1:2)] # Suppression des champs GROUPE

        ### Création d'un tableau récapitualtif du REV_CAD par IDU
        tab3 <- aggregate(data=tab[,c("IDU","REV_CAD")], .~IDU, FUN=sum)
        names(tab3) <- c("IDU","REV_CA")

        ## Création du tableur de sortie
        matrice <- merge(x = tab, y = tab2, by = "IDU", all.y = TRUE) # Jointure des tables 1 et 2
        matrice <- merge(x = matrice, y = tab3, by = "IDU", all.y = TRUE) # Jointure des tables 1/2 et 3
        matrice <- matrice[,-c(10,11,12,13)] # Suppression des champs liés au subdivisions : NATURE, GROUPE, SURFACE, REV_CAD
        matrice <- unique(matrice) %>% # Suppression des doublons
          filter(COM_CODE!="999") %>%  # Suppression de la ligne crée dans le cas d'une matrice à une parcelle
          dplyr::select(COM_CODE:ADRESSE,IDU,PREFIXE:LIEUDIT,OCCUP_SOL,REV_CA,SURF_CA) # Mise en ordre des champs

        # /!\  tab contient l'ensemble des données des parcelles cadastrales. Les subdivisions n'y sont plus présentes
        # /!\  si on souhaite exporté tab et non matrice --> remplacement de la ligne suivante par la seconde:

        if (is.null(nrow(sortie))){
          sortie=matrice
        }else{
          sortie = rbind(sortie, matrice)} # Concatene dans sortie l'ensemble des matrices
        # # if (is.null(nrow(sortie))){sortie=tab}else{sortie = rbind(sortie, tab)}
        print(paste("Traitement terminé du fichier ", i))
      }

      ## Sortie définitive d'un .csv
      #enrg=TRUE
      if (enrg){
        repOut <- paste(rep,"MatriceCad.csv", sep="/")
        write.csv(sortie, repOut, row.names =F)
        print(paste("Liste parcelles cadastrales enregistree dans le repertoire : ", repOut))
      }
      assign("CSV",sortie,envir=globalenv())
    }
  }
}
