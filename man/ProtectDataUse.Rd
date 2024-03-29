% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ProtectDataUse.R
\name{ProtectDataUse}
\alias{ProtectDataUse}
\title{Recherche statuts de protection}
\usage{
ProtectDataUse(shp1, shp2, width = 50, nom = "Protect")
}
\arguments{
\item{shp1}{= périmètre du territoire au format sf ou shp.}

\item{shp2}{= périmètre du territoire au format sf ou shp.}

\item{width}{= taille du buffer en mètre. Par défaut width= 50 m.}

\item{nom}{= nom du fichier en sortie. Par défaut il s'appellera Protect.shp et sera enregistré
sur le répertoire en cours.}
}
\value{
La fonction enregistre un fichier au format .shp contenant tous les statuts de protection
existant sur la propriété. Il renvoie également une carte réalisée avec ggplot.
}
\description{
La fonction recherche tous les statuts de protection situés sur ou à proximité immédiate
d'une propriété. La fonction nécessite en entrée
- un fichier géoréférencé au format sf contenant tous les statuts de protection en France métropolitaine.
Ce fichier peut être construit avec la fonction ProtectDataCreate
- un fichier géoréférencé du périmètre au format sf.
}
\examples{
library(DataForet)
library(PPtools)
load("~/pCloud Sync/Packages/Forestree/StatutProtec.Rdata")
data(FD, package = "PPtools")
perim <- FD \%>\% filter(IIDTN_FRT =="F09844P")
graph <- ProtectDataUse(Protect, perim, width=100)
graph

}
\author{
Bruciamacchie Max
}
