% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PouvoirAchat.R
\name{PouvoirAchat}
\alias{PouvoirAchat}
\title{Pouvoir d'achat}
\usage{
PouvoirAchat(an)
}
\arguments{
\item{an}{= année retenue}
}
\value{
La fonction renvoie un data frame.
}
\description{
Calcul du tableau permet d'exprimer des flux financiers en euros constants.
}
\examples{
library(Forestree)
library(DataForet)

Coeft <- PouvoirAchat(2018)

}
\author{
Bruciamacchie Max
}
