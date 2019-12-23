# Funciones para formato de dinero ----------------------------------------
#
# @title Funcion para cambiar formato de número.
#
# @description
# Uso interno para funciones específicas.
#

format_dinero_prefix <-  function(x, prefix, digits) {
  x <- round(x, digits = digits)
  paste0(prefix,
         ' ',
         format(x,
                big.mark = '.',
                decimal.mark = ',',
                scientific = FALSE)
  )
}

#' @title Dinero en formato CLP
#'
#' @description
#' Transforma número a texto como `CLP` con separación
#' de miles con `.` y decimales con `,`.
#'
#' @name format_clp
#'
#' @param x numeric
#' @param digits cantidad de dígitos del número entero que se quiere obtener.
#'        Por defecto digits = 0.
#'
#' @return character
#'
#' @export
#'
#' @examples
#'
#' format_clp(1000000)
#'
format_clp <-  function(x, digits = 0) {
  format_dinero_prefix(x, prefix = 'CLP', digits = digits)
}


#' @title Dinero en formato $
#'
#' @description
#' Transforma número a texto como `$` con separación
#' de miles con `.` y decimales con `,`.
#'
#' @name format_dinero
#'
#' @param x numeric
#' @param digits cantidad de dígitos del número entero que se quiere obtener.
#'        Por defecto digits = 0.
#'
#' @return character
#'
#' @export
#'
#' @examples
#'
#' format_dinero(1000000)
#'
format_dinero <-  function(x, digits = 0) {
  format_dinero_prefix(x, prefix = '$', digits = digits)
}


#' @title Número en formato para texto
#'
#' @description
#' Transforma número a texto con separación
#' de miles con `.` y decimales con `,`.
#'
#' @name format_dinero
#'
#' @param x numeric
#' @param digits cantidad de dígitos del número entero que se quiere obtener.
#'        Por defecto digits = 0.
#'
#' @return character
#'
#' @export
#'
#' @examples
#'
#' format_dinero(1000000)
#'
format_dinero <-  function(x, digits = 0) {
  format_dinero_prefix(x, prefix = '$', digits = digits)
}
