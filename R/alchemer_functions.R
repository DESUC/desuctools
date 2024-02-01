# Funciones para el trabajo con Alchemer
# Servicio de encuestas web
# https://app.alchemer.com/

#' @title read Alchemer SPSS export
#'
#' @description
#' Lee el archivo .sav a partir del _distribution link_ de un reporte de
#' exportación de una encuesta programada en Alchemer.
#' https://help.alchemer.com/help/spss
#'
#' Compatibility
#' - Comments are not available in SPSS exports.
#' - The Conjoint question is not available in SPSS exports.
#' - "Other, Write-In" rows in grid questions as not available in SPSS exports.
#' - The Time Started field is not includes as part of the SPSS export.
#'
#'
#' @param url `chr` Distribution link del reporte de base de datos en SPSS.
#'
#' @return tibble
#'
#' @importFrom haven read_sav
#'
#' @export
#'
alch_read_spss <- function(url){
  temp_zip <- tempfile(fileext = ".zip") # Descargo archivo zip

  download.file(url,
                destfile = temp_zip)

  temp_dir <- tempdir() # Descomprimo archivo descargado

  unzip(temp_zip,
        exdir = temp_dir)

  haven::read_sav(file.path(temp_dir, "spss.sav"))
}
