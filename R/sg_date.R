#' Convierte una fecha para su uso en la API de SurveyToGo
#'
#' Esta función toma una fecha y la convierte al formato requerido por la API de SurveyToGo.
#' Si no se especifica una hora, se asume 'mañana' (00:00:00).
#' Si se especifica 'tarde', se asigna el último segundo del día (23:59:59).
#'
#' @param date Fecha en formato Date o POSIXct. Si se proporciona una fecha en formato POSIXct,
#' se mantendrá la hora proporcionada en el objeto.
#' @param hora Cadena de texto que indica si se debe tomar la 'mañana' (00:00:00) o la 'tarde'
#' (23:59:59) como hora de referencia para la fecha. Valor por defecto es 'mañana'.
#'
#' @return Un string con la fecha formateada en el estándar ISO 8601 extendido, incluyendo
#' milisegundos y la zona horaria correspondiente.
#' @export
#'
#' @examples
#' # Convertir una fecha en formato Date para la mañana
#' sg_date(as.Date('2024-08-26'))
#'
#' # Convertir una fecha en formato Date para la tarde
#' sg_date(as.Date('2024-08-26'), hora = 'tarde')
#'
#' # Convertir una fecha en formato POSIXct manteniendo la hora
#' sg_date(as.POSIXct('2024-08-26 14:35:00'))
sg_date <- function(date,
                    hora = 'mañana') {

  # Verificar que date sea de tipo Date o POSIXct
  if (!lubridate::is.Date(date) && !lubridate::is.POSIXct(date)) {
    stop("Error: 'date' debe ser un objeto de tipo Date o POSIXct.")
  }

  # Verificar que hora sea 'mañana' o 'tarde'
  if (!hora %in% c('mañana', 'tarde')) {
    stop("Error: 'hora' debe ser 'mañana' o 'tarde'.")
  }

  timezone_format <- function(date){
    # Función para formatear la zona horaria.
    tz_date <- format(date, '%z')
    paste0(substr(tz_date, 1, 3), ":", substr(tz_date, 4, 5))
  }

  if (lubridate::is.POSIXct(date)) {
    date_format <- format(date,
                          '%Y-%m-%dT%H:%M:%OS7')

    timezone_format <- timezone_format(date)

    date_format <- format(date,
                          paste0('%Y-%m-%dT%H:%M:%OS70',
                                 timezone_format))

  } else {
    timezone_format <- timezone_format(Sys.time())

    hora_format <- switch(hora,
                          'mañana' = '00:00:00.0000000',
                          'tarde' = '23:59:59.9990000')

    date_format <- format(date,
                          paste0('%Y-%m-%dT',
                                 hora_format,
                                 timezone_format))
  }

  return(date_format)
}
