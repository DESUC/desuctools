# Funciones para el trabajo con Alchemer
# Servicio de encuestas web
# https://app.alchemer.com/

#' @title read Alchemer SPSS export
#'
#' @description
#' Lee el archivo .sav a partir del _distribution link_ de un reporte de
#' exportación de una encuesta programada en Alchemer.
#'
#' @param url `chr` Distribution link del reporte de base de datos en SPSS.
#'
#' @return tibble
#'
#' @importFrom haven read_sav
#'
#' @export
#'
#' @examples
alch_read_spss <- function(url){
  temp_zip <- tempfile(fileext = ".zip") # Descargo archivo zip

  download.file(url,
                destfile = temp_zip)

  temp_dir <- tempdir() # Descomprimo archivo descargado

  unzip(temp_zip,
        exdir = temp_dir)

  haven::read_sav(file.path(temp_dir, "spss.sav"))
}
