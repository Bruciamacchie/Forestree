% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CreateStack.R
\name{CreateStack}
\alias{CreateStack}
\title{Fabrication d'un stack}
\usage{
CreateStack(listeRaster, noms, shp1, shp2 = NULL)
}
\arguments{
\item{listeRaster}{Liste des rasters qui consitueront le stack.}

\item{noms}{Noms des rasters à utiliser.}

\item{shp1}{Zone d'étude au format.shp, les rasters seront découpés selon cette zone détude
elle doit donc être plus petite que les rasters.}

\item{shp2}{Partie à exclure de la zone d'étude, par défaut shp2 = NULL.}
}
\value{
La fonction renvoie une pile de raster sous forme d'objet stack.
}
\description{
Fabrication stack à partir de couches raster, harmonisation des rasters,
adaptation des rasters à une zone d'étude.
}
\author{
Max Bruciamacchie
}
