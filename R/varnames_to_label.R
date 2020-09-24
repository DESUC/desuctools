#' @title varnames_to_label
#'
#' @description Toma nombre de variables y los separa en un nombre breve
#'       y el resto de la etiqueta la integra en el atributo `label` de la
#'       variable.
#'       Además cambia la clase de las variables modificadas a `haven_labelled`
#'
#' @param .df `data.frame` Un data.frame
#' @param pattern_detect `chr` patrón de regex con el que se extrae el nombre breve
#'       de la variable. Este es el que será utilizado posteriormente como
#'       nombre final de la variable en la base
#' @param pattern_extract `chr` patrón de regex opcional.
#'       Por defecto es NULL e implica que se usará el mismo patrón que el dispuesto
#'       en `pattern_detect`.
#' @param var_prefix `chr` opcional. Prefijo para el nombre de las nuevas variables
#'       que se extraigan.
#'
#' @import dplyr
#' @importFrom stringr str_detect str_extract str_remove
#' @importFrom janitor make_clean_names
#' @importFrom labelled var_label
#'
#' @return data.frame
#' @export
#'
#' @examples
#' df <- data.frame(`first variable 1` = 1,
#'                  `second variable 2` = 2,
#'                  var_1 = 3L,
#'                  check.names = FALSE)
#'
#' varnames_to_label(df, pattern_detect = ' \\d$',
#'                   var_prefix = 'p')
#'
#' varnames_to_label(df,
#'                   pattern_detect = ' \\d$',
#'                   pattern_extract = '\\d$',
#'                   var_prefix = 'p')
#'
varnames_to_label <- function(.df,
                              pattern_detect,
                              pattern_extract = NULL,
                              var_prefix = NULL){

  if(is.null(pattern_extract)){
    pattern_extract <- pattern_detect
  }

  # Nombre de variables de toda la data frame
  var_names <- colnames(.df)

  # Vector donde TRUE si la variable tiene el patrón de interés.
  var_detect_l <- stringr::str_detect(var_names, pattern_detect)
  # print(sum(var_detect_l))

  # Chequea que haya variables detectadas según pattern_detect.
  stopifnot(sum(var_detect_l) > 0)

  # Variables que no cumplen con el patrón
  var_no_change <- var_names[!var_detect_l] %>%
    janitor::make_clean_names()

  # Variables que sí cumplen con el patrón
  var_change <- var_names[var_detect_l] %>%
    stringr::str_extract(pattern_extract)

  var_change <- paste0(var_prefix, var_change) %>%
    janitor::make_clean_names()

  # Vector con label para variables que si cumplen con el patrón.
  var_p_labels <- var_names[var_detect_l] %>%
    stringr::str_remove(pattern_detect) %>%
    str_squish()

  # Vector con nombres nuevos y antiguos para rename.
  v_cambio_nombre <- structure(c(var_names[!var_detect_l], var_names[var_detect_l]),
                               names = c(var_no_change, var_change))

  # Cambio de nombres
  df <- .df %>%
    rename(all_of(v_cambio_nombre))

  # Agregar label a las variables que sí cumplen con el patrón.
  labelled::var_label(df) <- as.list(structure(var_p_labels,
                                               names = var_change))

  # Cambiar clase a variables que sí cumplen con el patrón.
  df %>%
    mutate(across(any_of(var_change),
                  function(x) `class<-`(x, 'haven_labelled')))
}
