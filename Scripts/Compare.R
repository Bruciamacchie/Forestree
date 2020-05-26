library(readxl)
library(tidyverse)
library(tcltk)

delta = 5
début = 1978
fin = 1991

Diams = seq(20,60,5)
Ini = c(30,51,34,36,37,9,4,0,0)
Fin = c(41,41,35,32,35,32,13,4,1)
tab <- data.frame(Diam = Diams,
                  Initial = Ini,
                  Final = Fin)

################################################################
# rep <- "~/Desktop/Packages/Forestry/Exemples"
# setwd(rep)

fich <- tk_choose.files(caption = "Choix du fichier contenant volumes",
                        filters=matrix(c(".xlsx",".xlsx"),1,2, byrow = T))
UG   <- read_excel(fich, sheet="UG")
Inv  <- read_excel(fich, sheet="Inv")
Nb   <- read_excel(fich, sheet="Data")

Choix <- Inv %>%
  left_join(UG, by = "IDUG") %>%
  filter(Type == "Inv") %>%
  mutate(Possibles = paste(Forêt, Parcelle,Année, sep=" - "))

ListInv <- tk_select.list(Choix$Possibles,
                          title = "Choisir 2 inventaires à comparer",
                          multi = T)
Choix <- dispositifs$NumDisp[which(Noms==ListDisp)]

IdUG <- UG$IDUG[1]
inv <- Inv %>%
  filter(IDUG == IdUG) %>%
  filter(Type == "Inv") %>%
  arrange(Année)

tab <- Nb %>%
  filter(IdInv %in% inv$IDInv) %>%
  spread(IdInv, Nb)
names(tab) <- c("Diam","Initial","Final")
tab[is.na(tab)] <- 0

Corres <- data.frame(Ecart = c(0,1,2,3),
                     var = c("Stat","Promus","DPromus","TPromus"))

TabProm = data.frame()
Ini <- tab$Initial
Fin <- tab$Final
i=max(which(Fin>0))
while (i > 1) {
  j = max(which(Ini>0))
  t1 = data.frame(Diam = tab$Diam[j],
                  Ecart = i-j,
                  Nb = min(Fin[i], Ini[j]))
  TabProm <- rbind(TabProm, t1)
  Ini[j] = Ini[j] - t1$Nb[1]
  Fin[i] = Fin[i] - t1$Nb[1]
  if (Fin[i] == 0) { i = i - 1}
}
t1 = data.frame(Diam = tab$Diam[j],
                Ecart = i-j,
                Nb = min(Fin[i], Ini[j]))
TabProm <- rbind(TabProm, t1)

TabProm <- TabProm %>%
  left_join(Corres, by = "Ecart") %>%
  dplyr::select(-Ecart) %>%
  mutate(var = factor(var, levels = c("Stat","Promus","DPromus","TPromus") ))

t1 <- TabProm %>%
  tidyr::spread(var, Nb, fill=0)

tab <- tab %>%
  left_join(t1, by = "Diam", fill=0)
tab[is.na(tab)] <- 0
tab <- tab %>%
  mutate(AcctD = delta/Initial)

