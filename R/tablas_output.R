#' Tablas de resultados ----------------------------------------------------
#'
#' Generación de una data.frame con el número de casos y proporción de las distintas
#' variables de segmentos que se agregen en `...`.
#'
#' @title Tabla de categorías
#'
#' @description
#' Porcentaje de respuesta de categorías de varias variables.
#' Principalmente para mostrar la distribución de casos de variables de segmentación posteriores.
#'
#' @name tabla_categorias
#'
#' @param .data data frame. Base de datos.
#' @param ... Preguntas de las que se quiere saber su proporcion. Se puede utilizar
#' `tidyselect` para facilitar la selección de varias columnas.
#' @param .wt Ponderador o expansor de los datos. Por defecto es NULL.
#'
#' @return tibble
#'
#' @import dplyr
#' @importFrom rlang %||% .data enquo
#' @importFrom forcats as_factor fct_na_value_to_level
#' @importFrom sjmisc to_label
#' @importFrom tidyselect vars_select
#' @importFrom purrr map_chr
#' @importFrom tidyr pivot_longer
#'
#' @export
tabla_categorias <- function(.data,
                             ...,
                             .wt = NULL) {
  # Tabla con número de casos y proporción de respuestas por distintas categorías.
  wt_quo <- enquo(.wt)

  preguntas <- tidyselect::vars_select(names(.data), ...)

  # Vector de etiqueta de variables.
  seg_labels <- map_chr(preguntas, ~attr(.data[[.]], 'label') %||% '')
  names(seg_labels) <- preguntas

  tabla <- .data %>%
    mutate(across(any_of(preguntas), haven::as_factor),
           !!wt_quo,
           .keep = 'none') %>%
    group_by(across(any_of(preguntas))) %>%
    summarise(n = sum(!!wt_quo %||% n()),
              .groups = 'drop') %>%
    tidyr::pivot_longer(cols = -n,
                        names_to = 'pregunta_var',
                        values_to = 'pregunta_cat') %>%
    mutate(pregunta_var = forcats::as_factor(.data$pregunta_var),
           pregunta_cat = forcats::as_factor(.data$pregunta_cat),
           pregunta_cat = forcats::fct_na_value_to_level(.data$pregunta_cat,
                                                         level = 'NA'))

  tabla <- tabla %>%
    count(.data[['pregunta_var']],
          .data[['pregunta_cat']],
          wt = .data[['n']]) %>%
    group_by(.data[['pregunta_var']]) %>%
    mutate(prop = n/sum(n)) %>%
    rename(casos = n) %>%
    ungroup()

  tabla %>%
    mutate(pregunta_lab = forcats::as_factor(seg_labels[.data$pregunta_var])) %>%
    select(starts_with('pregunta'), everything())
}

tabla_orden <- function(.data, .var, .segmento = NULL) {
  # Orden de variables y categorias para la presentación de tablas.

  var_quo <- rlang::enquo(.var)
  segmento_quo <- rlang::enquo(.segmento)

  var_seg_exprs <- rlang::exprs(!!segmento_quo, starts_with(!!rlang::as_label(var_quo)))

  .data %>%
    select_at(vars(!!!var_seg_exprs, everything())) %>%
    arrange_at(vars(!!!var_seg_exprs))
}

tabla_prop <- function(.data, .segmento) {
  # Cálculo de porcentaje de respuestas en tabla con numero de casos.

  segmento_quo <- rlang::enquo(.segmento)

  .data %>%
    group_by_at(vars(!!segmento_quo)) %>%
    mutate(prop = .data$casos / sum(.data$casos)) %>%
    ungroup()
}


tabla_prop_val <- function(.data, .var, .segmento, miss) {
  # Cálculo de porcentaje de respuestas válidas en tabla con numero de casos.

  # Pasar de quosure con texto a string y luego simbolo.
  var_quo <- rlang::sym(rlang::as_name(.var))
  segmento_quo <- enquo(.segmento)

  .data %>%
    group_by_at(vars(!!segmento_quo)) %>%
    mutate(casos_val = replace(.data[["casos"]], (!!var_quo %in% miss), NA_real_),
           prop_val = .data[["casos_val"]] / sum(.data[["casos_val"]], na.rm = TRUE)) %>%
    select(!'casos_val') %>%
    ungroup()
}

tabla_total <- function(.data,
                        .var,
                        .segmento,
                        miss = NULL) {
  # Cálculo de porcetaje para el total de segmento

  tab_total <- .data %>%
    group_by(across({{ .var }})) %>%
    summarise(across('casos', sum)) %>%
    mutate({{ .segmento }} := "Total") %>%
    ungroup()

  tab_total <- tabla_prop(tab_total, .segmento = NULL)

  # Agrega el porcentaje válido si es que se señalan categorias perdidas.
  tab <- bind_rows(.data %>%
                     mutate({{ .segmento }} := as.character({{ .segmento }})),
                   tab_total)

  tab <- tab %>%
    mutate({{ .segmento }} := forcats::as_factor({{ .segmento }}))

  if (!is.null(miss)) {
    tab <- tabla_prop_val(tab,
                          .var = enquo(.var),
                          .segmento = {{ .segmento }},
                          miss = miss)
  }
  return(tab)
}

tabla_var_segmento <- function(.data,
                               .var,
                               .segmento = NULL,
                               .wt = NULL,
                               total = FALSE,
                               miss = NULL) {
  # Tabla con número de casos y proporción de variable
  # Se agrega una variable de de segmentación llamada 'segmento' con valor 'Total'.

  var_quo <- enquo(.var)
  segmento_quo <- enquo(.segmento)
  wt_quo <- enquo(.wt)

  tab <- .data %>%
    transmute(across(c(!!segmento_quo, !!var_quo, !!wt_quo),
                     \(x) sjmisc::to_label(x, add.non.labelled = TRUE))) %>%
    group_by(across(c(!!segmento_quo, !!var_quo))) %>%
    summarise(casos = sum(!!wt_quo %||% n())) %>%
    ungroup()

  # Agrega el porcentaje de respuesta.
  tab <- tabla_prop(tab,
                    .segmento = !!segmento_quo)

  # Agrega el porcentaje válido si es que se señalan categorías perdidas.
  if (!is.null(miss)) {
    tab <- tabla_prop_val(tab,
                          .var = var_quo,
                          .segmento = !!segmento_quo,
                          miss = miss)
  }

  # Agrega el porcentaje total a los segmentos.
  if (total) {
    tab <- tabla_total(tab,
                       .var = !!var_quo,
                       .segmento = !!segmento_quo,
                       miss = miss)
  }

  tabla_orden(tab,
              .var = !!var_quo,
              .segmento = !!segmento_quo)
}


tabla_var_segmentos <- function(.data,
                                .var,
                                .segmentos,
                                .wt = NULL,
                                total = FALSE,
                                miss = NULL) {
  # Resultados de una pregunta `.var` para varios segmentos `.segmentos`

  tabla_var_seg <- function(.data, .seg) {

    seq_quo <- enquo(.seg)

    tab <- tabla_var_segmento(.data,
                              .var = {{ .var }},
                              .segmento = {{ .seg }},
                              total = total,
                              .wt = {{ .wt }},
                              miss = miss) %>%
      mutate(segmento_var = !!rlang::as_label(seq_quo))

    if(rlang::quo_is_null(seq_quo)){
      tab$segmento_cat <- "NULL"
    } else {
      tab <- tab %>%
        rename(segmento_cat = !!rlang::as_label(seq_quo))
    }

    tab %>%
      mutate(across(c('segmento_var', 'segmento_cat'), as.character))
  }

  tab <- map(.segmentos, ~tabla_var_seg(.data, .seg = !!.))

  tab <- reduce(tab, bind_rows) %>%
    mutate(across("segmento_cat", forcats::as_factor))

  # Copia label y labels a variable "var" recién creada.
  # No utilizo esto para no pegar las etiquetas de
  # tab <- sjlabelled::copy_labels(df_new = tab,
  #                                df_origin = .data)

  # Dejar solo el 'label' de la variable recién creada
  var_filtro <- rlang::as_name(enquo(.var))

  tab[[var_filtro]] <- structure(tab[[var_filtro]],
                                 label = attr(.data[[var_filtro]], 'label', exact = TRUE))

  tab %>%
    select("segmento_var",
           "segmento_cat", everything())
}

#' @title Tabla de porcentajes de variables según segmentos.
#'
#' @description
#' Obtiene porcentajes de respuestas de múltiples variables según multiples segmentos.
#'
#' @param .data tibble
#'
#' @param .vars vars(), lista de nombres de variables de las que se quiere saber su proporción de respuestas
#' @param .segmentos vars(), lista de nombres de variables de segmentación de las preguntas de `.vars`
#' @param .wt name, nombre de la variable de ponderación
#' @param total logical, Si total = TRUE, se agrega el total para cada segmento.
#' @param miss integers, Vector de valores que deben coniderarse como missings.
#'
#' @return tibble
#'
#' @import dplyr
#' @importFrom purrr map map2 reduce
#' @importFrom forcats as_factor
#' @importFrom tidyselect vars_select
#' @importFrom rlang .data
#'
#' @export
tabla_vars_segmentos <- function(.data,
                                 .vars,
                                 .segmentos,
                                 .wt = NULL,
                                 total = FALSE,
                                 miss = NULL) {

  variables <- tidyselect::vars_select(names(.data), !!!.vars)

  tab <- purrr::map(variables, ~tabla_var_segmentos(.data,
                                                    .var = !!.,
                                                    .segmentos = .segmentos,
                                                    .wt = {{ .wt }},
                                                    total = total,
                                                    miss = miss))

  tabla_variables <- function(.data, .var) {

    # Captura la etiqueta de la variable. Si no tiene, lo deja en blanco
    var_label <- attr(.data[[.var]], 'label') %||% "-"

    .data %>%
      mutate(pregunta_var = as.character(.var),
             pregunta_lab = var_label) %>%
      rename(pregunta_cat = all_of(.var))
  }

  purrr::map2(tab, variables, ~tabla_variables(.x, .y)) %>%
    purrr::reduce(bind_rows) %>%
    select(starts_with("segmento"),
           'pregunta_var',
           'pregunta_lab',
           'pregunta_cat',
           everything()) %>%
    mutate(across(c("segmento_var",
                    "pregunta_var",
                    "pregunta_lab"),
                  forcats::as_factor))
}
