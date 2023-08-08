# Funciones para análisis de datos de encuestas
# 181228

#' @title Tabla con intervalos de confianza
#'
#' @description
#' Devuelve tabla de frecuencias con intervalos de confianza para un nivel
#' `level`de significancia entre las categorías de respuesta de la
#' variable `.var`.
#'
#' @name svy_tabla_var_segmento
#'
#' @param .data data frame con diseño complejo
#' @param .var Variable en la que interesa comparar categorías de respuesta.
#' @param .segmento Segmentos de interés para ver diferencias en categorías
#'        de variable `.var`. Por defecto NULL
#' @param na.rm boolean. Considera o no missings, por defecto FALSE.
#' @param level double. Nivel de significancia para intervalos de confianza
#'
#' @importFrom rlang %||% .data
#' @importFrom srvyr survey_mean
#'
#' @return data.frame
#'
#' @export
svy_tabla_var_segmento <- function(.data,
                                   .var,
                                   .segmento = NULL,
                                   na.rm = TRUE,
                                   level = 0.95){

  if(!any(class(.data) %in% 'tbl_svy')) stop('Se necesita un data.frame con diseno complejo')

  segmento_quo <- enquo(.segmento)
  var_quo <- enquo(.var)

  to_factor <- function(x){
    if(haven::is.labelled(x)){
      haven::as_factor(x)
    } else {
      x
    }
  }

  # Strinf de la variable de interés. Se usa luego para extraer esta columna.
  var_str <- rlang::as_label(enquo(.var))

  # Construcción de tabla de segmentos y variable de interés
  # Esto era truncate antes de dplyr 1.0.
  tab <- .data %>%
    mutate(segmento_var = rlang::as_label(segmento_quo)       %||% 'Total',
           segmento_lab = labelled::var_label(!!segmento_quo) %||% '-',
           segmento_cat = haven::as_factor(!!segmento_quo     %||% '-') %>%
             forcats::fct_na_value_to_level(level = 'seg_miss') |>
             forcats::fct_drop('seg_miss'),
           pregunta_var = var_str,
           pregunta_lab = labelled::var_label(!!var_quo)      %||% '-',
           pregunta_cat = to_factor(!!var_quo)
           ) %>%
    select("segmento_var":"pregunta_cat")

  # print(head(tab$variables))
  var_labels_length <- length(attr(.data[['variables']][[var_str]], 'labels'))
  var_class <- class(.data[['variables']][[var_str]])

  # Construir la variable de interés según si es una variable escalar o categórica
  if (var_labels_length == 0 & all(var_class != 'factor')) {

    # Variable escalar
    tab <- tab %>%
      group_by(across("segmento_var":"pregunta_lab")) %>%
      summarise(mean = srvyr::survey_mean(.data$pregunta_cat,
                                          na.rm = na.rm,
                                          vartype = c('ci', 'se'), level = level))
  } else {
    # Variable categórica
    # print('categorica')
    tab <- tab %>%
      mutate(pregunta_cat = forcats::fct_na_value_to_level(.data[['pregunta_cat']],
                                                           level = 'cat_miss'),
             pregunta_cat = forcats::fct_drop(pregunta_cat, 'cat_miss')) %>%
      group_by(across(c("segmento_var":"pregunta_lab",
                        "pregunta_cat")),
               .drop = FALSE) %>%
      summarise(prop = srvyr::survey_mean(na.rm = na.rm,
                                          vartype = c('ci', 'se'),
                                          level = level)) %>%
      identity()
  }

  # Determinar si hay diferencias significativas
  tab <- tab %>%
    group_by(.data[['segmento_cat']]) %>%
    svy_diff_sig() %>%
    ungroup()

  if(rlang::quo_is_null(enquo(.segmento))){
    tab %>%
      select(-starts_with('segmento'))
  } else {
    tab
  }
}


#' @title Tabla con intervalos de confianza
#'
#' @description
#' Devuelve tabla de frecuencias con intervalos de confianza para un nivel
#' `level`de significancia entre las categorías de respuesta de la
#' variable `.var`.
#'
#' @name svy_tabla_var_segmentos
#'
#' @param .data `tbl_svy` data.frame con diseño de encuesta.
#' @param .var Variable de interés respecto.
#' @param .segmentos vars(). Lista de variables por las que se quiere segmentar `.var`.
#' @param ... atributos que se pasan a funcion `svy_tabla_var_segmento`.
#'
#' @importFrom rlang !! syms
#' @importFrom tidyselect vars_select
#' @importFrom purrr map reduce
#'
#' @return data.frame
#'
#' @export
svy_tabla_var_segmentos <- function(.data,
                                    .var,
                                    .segmentos = NULL,
                                    ...) {

  segmentos <- tidyselect::vars_select(colnames(.data), !!!.segmentos)
  # Transforma string a expresiones para ser evaluadas luego.
  segmentos <- rlang::syms(segmentos)

  # Funcion para poder pasar ... de la funcion dentro de map.
  # No funcionó agregar ... dentro de una función anónima dentro de map.
  svy_tabla <- function(.seg) {
    svy_tabla_var_segmento(.data,
                           .var = {{ .var }},
                           .segmento = !!.seg,
                           ...)
  }

  purrr::map(segmentos, svy_tabla) %>%
    purrr::map(~mutate(., segmento_cat = as.character(segmento_cat))) %>%
    purrr::reduce(dplyr::bind_rows)
}


#' @title Tabla con intervalos de confianza
#'
#' @description
#' Devuelve tabla de frecuencias con intervalos de confianza para un nivel
#' `level`de significancia entre las categorías de respuesta de la
#' variable `.vars`.
#'
#' A diferencia de svy_tabla_var_segmentos, esta funcion puede procesar varias variables
#' y segmentos a la vez.
#'
#' @name svy_tabla_vars_segmentos
#'
#' @param .data `tbl_svy` data.frame con diseño de encuesta.
#' @param .vars c(). Variables de interés respecto.
#' @param .segmentos c(). Lista de variables por las que se quiere segmentar `.vars`.
#' @param ... atributos que se pasan a funcion `svy_tabla_var_segmento`.
#'
#' @importFrom rlang !! enquo sym
#' @importFrom tidyr unnest expand_grid
#' @importFrom tidyselect eval_select
#' @importFrom purrr map2
#'
#' @return data.frame
#'
#' @export
#'
svy_tabla_vars_segmentos <- function(.data,
                                     .vars,
                                     .segmentos = NULL,
                                     ...) {

  enquo_var <- rlang::enquo(.vars)
  enquo_seg <- rlang::enquo(.segmentos)

  # Posiciones de variables en la df .data
  .var_select <- tidyselect::eval_select(expr = enquo_var,
                                         data = .data[['variables']])

  .seg_select <- tidyselect::eval_select(expr = enquo_seg,
                                         data = .data[['variables']])

  # Nombres de las variables de interés a partir de sus posiciones.
  var_sel_name <- colnames(.data)[.var_select]

  # Nombres de segmentos si hay variables de segmentos.
  if(length(.seg_select) == 0){
    var_seg_name <- NA
  } else {
    var_seg_name <- colnames(.data)[.seg_select]
  }

  # Elaboración de base con todas las combinaciones entre variables y segmentos.
  tab <- tidyr::expand_grid(var = var_sel_name,
                            seg = var_seg_name)

  # print(tab)

  svy_tabla_var_segmento_int <- function(.var, .seg) {
    # Funcion auxiliar para manejar casos en los que .seg == NULL.

    var <- rlang::sym(.var)

    if(is.na(.seg)) {
      seg <- NULL
    } else {
      seg <- rlang::sym(.seg)
    }

    svy_tabla_var_segmento(.data,
                           .var = !!var,
                           .segmento = !!seg,
                           ...)
  }

  # Cálculo de cada combinación de variable y segmento
  l_result <- purrr::map2(tab[['var']], tab[['seg']],
                          ~ svy_tabla_var_segmento_int(.x, .y))

  # Agregar la lista de resultados a tab para poder desarmarla usando unnest.
  df_result <- tab %>%
    mutate(l_result = l_result)

  # Resultado final sin las columnas auxiliares.
  df_result %>%
    tidyr::unnest(l_result) %>%
    select(!c('var', 'seg'))
}


#' @title Comparación entre intervalos de confianza
#'
#' @description
#' Determina diferencias significativas según intervalos de confianza calculados desde
#' `srvyr`.
#'
#' @name svy_diff_sig
#'
#' @param .data data.frame con variables `\\*_upp` y `\\*_low`
#'
#' @return data.frame
#'
#' @export
svy_diff_sig <- function(.data){

  var_data <- colnames(.data)

  if(sum(stringr::str_detect(var_data, '_upp$|_low$')) != 2) stop('Se necesita el intervalo de confianza')

  var_low <- rlang::sym(var_data[stringr::str_which(var_data, '_low$')])
  var_upp <- rlang::sym(var_data[stringr::str_which(var_data, '_upp$')])

  tab <- .data %>%
    mutate(diff_sig = min(!!var_upp) < !!var_low | max(!!var_low) > !!var_upp)
}
