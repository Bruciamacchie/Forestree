library(xml2)
library(rvest)


file <- tk_choose.files(caption = "Choisir le dossier contenant les extraits de matrice cadastrale",
                        filters=matrix(c(".html",".htm"),1,2, byrow = T))
if (is.na(file)) {
  stop("Traitement annulé")
}
rep <- dirname(file)
ListeFich <- list.files(rep, "*.htm", full.names=T) # Création d'une liste de fichier .html
print(paste0("Le dossier choisi contient ",length(ListeFich)," fichiers au format htm."))

for (i in 1:length(ListeFich)) {
  i=1

  file <- read_html(ListeFich[i])
  tables <- html_nodes(file, "table")

  ### Traitement commune
  Commune    <- html_table(tables[1], fill = TRUE) %>%
    as.data.frame()
  Com_Nom  <- str_sub(Commune[6],5) # Récupération du nom de la commune
  Com_Code <- as.numeric(str_sub(Commune[6],1,3)) # Récupération du code la commune
  Com_Code <- str_pad(Com_Code, 3, "left", pad = "0")


  Cad <- html_table(tables[5], fill = TRUE) %>%
    as.data.frame()
  Cad <- Cad %>%
    slice(-1:-3) %>%
    select(c(1:3,5,10,11,14,15))
  names(Cad) <- c("AN","Section","Num","Adresse","SUF","Nature","Surf","Revenu")
  Cad <- Cad%>%
    mutate(Nb=nchar(Num)) %>%
    filter(Nb > 0) %>%
    select(-Nb) %>%
    mutate(Surf = as.numeric(gsub(" ", "", Surf, fixed = TRUE)),
           Revenu = as.numeric(gsub(",", ".", Revenu)))



}

tab <- html_table(tables[7], fill = TRUE) %>%
  as.data.frame()
