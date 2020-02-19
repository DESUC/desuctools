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

  # Construcción de tabla de segmentos y variable de interés
  tab <- .data %>%
    transmute(segmento_var = rlang::as_label(enquo(.segmento))      %||% 'Total',
              segmento_lab = sjlabelled::get_label({{ .segmento }}) %||% '-',
              segmento_cat = sjlabelled::as_label({{ .segmento }}   %||% '-') %>%
                forcats::fct_explicit_na(na_level = 'seg_miss'),
              pregunta_var = rlang::as_label(enquo(.var)),
              pregunta_lab = sjlabelled::get_label({{ .var }})      %||% '-',
              pregunta_cat = sjlabelled::as_label({{ .var }}, add.non.labelled = TRUE))

  # print(head(tab$variables))

  # Construir la variable de interés según si es una variable escalar o categórica
  if (class(.data$variables[[ rlang::as_label(enquo(.var)) ]] ) %in% c('numeric', 'integer')) {
    # print('escalar')
    # Variable escalar
    tab <- tab %>%
      group_by_at(vars(.data$segmento_var:.data$pregunta_lab)) %>%
      summarise(mean = srvyr::survey_mean(.data$pregunta_cat,
                                          na.rm = na.rm,
                                          vartype = c('ci', 'se'), level = level))
  } else {
    # Variable categórica
    # print('categorica')
    tab <- tab %>%
      mutate(pregunta_cat = forcats::fct_explicit_na(.data$pregunta_cat,
                                                     na_level = 'cat_miss')) %>%
      group_by_at(vars(.data$segmento_var:.data$pregunta_lab, .data$pregunta_cat),
                  .drop = FALSE) %>%
      summarise(prop = srvyr::survey_mean(na.rm = na.rm,
                                          vartype = c('ci', 'se'),
                                          level = level)) %>%
      identity()
  }

  # Determinar si hay diferencias significativas
  tab <- tab %>%
    group_by(.data$segmento_cat) %>%
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
  .var_select <- tidyselect::eval_select(expr = enquo_var, data = .data[['variables']])

  .seg_select <- tidyselect::eval_select(expr = enquo_seg, data = .data[['variables']])

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
  l_result <- purrr::map2(tab$var, tab$seg,
                          ~ svy_tabla_var_segmento_int(.x, .y))

  # Agregar la lista de resultados a tab para poder desarmarla usando unnest.
  df_result <- tab %>%
    mutate(l_result = l_result)

  # Resultado final sin las columnas auxiliares.
  df_result %>%
    tidyr::unnest(l_result) %>%
    select(-var, -seg)
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
