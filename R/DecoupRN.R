#' Régions naturelles
#'
#' @description La fonction met à disposition le découpage de la France en régions naturelles détaillées ou non
#' par département, ainsi que le découpage en sylvoécorégions.
#'
#' @return La fonction renvoie un objet géoréférencé de la France au format shp ou sf.
#'
#' @import sf
#' @import dplyr
#'
#' @param formatSf = choix du format. Si formatSf = TRUE l'objet sera au format sf, sinon il sera au format shp.
#' Par défaut formatSf = TRUE.
#' @param dept = échelle du découpage. Si dept = TRUE, la France sera découpée par département. Par défaut dept = TRUE
#'
#' @examples
#'  rn <- DecoupRN(formatSf = T, dept = F)
#'  class(rn)
#'  rn <- DecoupRN(formatSf = F, dept = F)
#'  class(rn)
#'
#' @author Bruciamacchie Max
#'
#' @export

DecoupRN <- function(formatSf = TRUE, dept = TRUE) {
  data(rfifn)
  if(!dept) {
    tab <- rfifn %>%
      st_set_geometry(NULL) %>%
      filter(!is.na(REGN)) %>%
      dplyr::select(REGN,REGIONN) %>%
      distinct()
    rfifn <- rfifn %>%
      filter(!is.na(REGN)) %>%
      group_by(REGN) %>%
      summarise(AREA = sum(AREA)) %>%
      left_join(tab, by = "REGN")
  }
  if(!formatSf) {
    rfifn <- as(rfifn, "Spatial")
  }
  return(rfifn)
}
