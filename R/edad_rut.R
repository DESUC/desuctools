#' @title edad_rut
#'
#' @description
#' Función para calcular una edad estimada según el rut de la persona.
#' Es solo una aproximación. Puede ser inexacta para personas migrantes.
#'
#' @importFrom lubridate date_decimal
#'
#' @param .rut `ìnt`: Vector numérico con el rut (sin dígito verificador).
#' @param fecha_referencia `date`: Vector que contiene la fecha de referencia que determina la edad.
#'  Pueden ser una fecha pasada, actual o futura.
#'
#' @source
#' https://rutificador-chile.com/wp-content/uploads/2022/06/rut-a-edad.html
#'
#' @return
#' integer
#'
#' @examples
#'
#' # Importante: el rut no debe contar con el dígito verificador
#' x <- 20117419
#' fecha <- as.Date("2024-01-31")
#'
#' edad_rut(.rut = x,
#'          fecha_referencia = fecha)
#'
#' @export
edad_rut <- function(.rut,
                     fecha_referencia){

  slope <- 3.3363697569700348e-06
  intercept <- 1932.2573852507373

  birth_date <- .rut * slope + intercept

  birth_date_year <- floor(birth_date)
  birth_date_month <- ceiling((birth_date - birth_date_year) * 12)

  age <- difftime(fecha_referencia,
                  lubridate::date_decimal(birth_date))

  floor(age / 365.25) |>
    as.integer()
}
