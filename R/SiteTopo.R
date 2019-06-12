#' Indicateurs topographiques
#'
#' @description La fonction permet de calculer des indicateurs topographiques sur un périmètre donné.
#'
#' @return La fonction renvoie trois indicateurs topographiques : pente, classes de pente par exposition,
#' direction de la ligne de plus grande pente.
#'
#' @param shp = périmètre de la zone au format sf.
#' @param r = MNT sous forme de raster.
#'
#' @import raster
#' @import sf
#' @import tidyverse
#' @import scales
#'
#' @examples
#' \donttest{
#' library(raster)
#' library(sf)
#' library(tidyverse)
#' library(scales)
#' res <- SiteTopo(perim, mnt)
#' res$Pente
#' res$ExpoPente
#' res$Ecoul
#' }
#'
#' @author Bruciamacchie Max
#'
#' @export

SiteTopo <- function(shp, r) {
  if (is(shp, "sf") & is(r, "RasterLayer")){
    crs(r) <- st_crs(shp)[[2]]
    shp <- as(shp, 'Spatial')
    r1 <- crop(r, shp)
    r1 <- mask(r1, shp)
    # ---------- Indicateurs
    pente   <- terrain(r1, opt='slope', unit='tangent')
    expo    <- terrain(r1, opt='aspect', unit='degrees')
    flowdir <- terrain(r1, opt='flowdir', unit='degrees')
    tab <- data.frame(Pente = values(pente)*100,
                      Exposition = values(expo),
                      Ecoulement = values(flowdir))
    # ---------- Pente
    Pente <- tab %>%
      filter(!is.na(Pente)) %>%
      mutate(ClassePente = floor(Pente/5+0.5)*5) %>%
      dplyr::select(ClassePente) %>%
      group_by(ClassePente) %>%
      summarise(Nb = n()) %>%
      mutate(Total = sum(Nb),
             Freq = Nb/Total)
    RangePente = range()
    ech <- floor((max(Pente$Pente)- min(Pente$Pente))/100)*10

    gPente <- ggplot(Pente, aes(x=ClassePente, y=Freq)) + geom_bar(stat='identity', fill='grey80') +
      theme_bw() + scale_y_continuous(labels = scales::percent) +
      labs(y="", x="Pente (%)")
    # ---------- Exposition
    Exposition <- tab %>%
      filter(!is.na(Exposition)) %>%
      mutate(ClasseExpo = floor(Exposition/360*400/5+0.5)*5,
             ClasseExpo = ClasseExpo %% 400,
             ClassePente = floor(Pente/ech+0.5)*ech,
             ClassePente = ifelse(ClassePente > 100, 100, ClassePente)) %>%
      dplyr::select(ClasseExpo, ClassePente) %>%
      group_by(ClasseExpo, ClassePente) %>%
      summarise(Nb = n()) %>%
      ungroup() %>%
      mutate(Total = sum(Nb),
             Freq = Nb/Total)

    gExpoPente <- ggplot(data = Exposition, aes(x=ClasseExpo, y = Freq, fill=factor(ClassePente))) +
      geom_bar(stat='identity') + coord_polar() +
      theme_bw() + labs(x="Exposition (gr)", fill="Pente (%)") +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x  = element_text(size=9),
            legend.title = element_text(size=9),
            legend.text = element_text(size=8))
    # ---------- Ecoulement
    corresp <- data.frame(Ecoulement = c(1,2,4,8,16,32,64,128),
                          Exposition = c(100,150,200,250,300,350,0,50))
    Ecoulement <- tab %>%
      filter(!is.na(Ecoulement)) %>%
      group_by(Ecoulement) %>%
      summarise(Nb = n()) %>%
      left_join(corresp, by = "Ecoulement") %>%
      mutate(Total = sum(Nb),
             Freq = Nb/Total) %>%
      arrange(Exposition)

    gEcoul <- ggplot(Ecoulement, aes(x=Exposition, y = Freq)) +
      geom_bar(stat='identity', fill='grey80') + coord_polar(start=75) +
      theme_bw() + labs(x="Ecoulement (gr)") +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x  = element_text(size=9),
            legend.text = element_text(size=8))
    out <- list(gPente,gExpoPente,gEcoul)
    names(out) <- c("Pente","ExpoPente","Ecoul")
    return(out)
  }
}




