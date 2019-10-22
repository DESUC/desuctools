#' Tablas de resultados ----------------------------------------------------
#'
#' Generación de una data.frame con el número de casos y proporción de las distintas
#' variables de segmentos que se agregen en `...`.
#'
#' @title Tabla de categorías
#'
#' Porcentaje de respuesta de categorías de varias variables.
#' Principalmente para mostrar la distribución de casos de variables de segmetnación posteriores.
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
#' @importFrom forcats as_factor fct_explicit_na
#' @importFrom sjmisc to_label
#' @importFrom sjlabelled get_label
#' @importFrom tidyselect vars_select
#'
#' @export
tabla_categorias <- function(.data,
                             ...,
                             .wt = NULL) {
    # Tabla con número de casos y proporción de respuestas por distintas categorías.
    preguntas <- tidyselect::vars_select(names(.data), ...)
    wt_quo <- enquo(.wt)

    seg_labels <- sjlabelled::get_label(.data, preguntas)

    tabla <- .data %>%
        transmute_at(vars(preguntas, !!wt_quo), list(sjmisc::to_label)) %>%
        group_by_at(vars(preguntas)) %>%
        summarise(n = sum(!!wt_quo %||% n())) %>%
        tidyr::gather("pregunta_var", "pregunta_cat", -n) %>%
        mutate(pregunta_var = forcats::as_factor(.data$pregunta_var),
               pregunta_cat = forcats::as_factor(.data$pregunta_cat),
               pregunta_cat = forcats::fct_explicit_na(.data$pregunta_cat, na_level = 'NA'))

    tabla <- tabla %>%
        count(.data$pregunta_var, .data$pregunta_cat, wt = .data$n) %>%
        group_by(.data$pregunta_var) %>%
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
    # Cálculo de porcetaje de respuestas en tabla con numero de casos.

    segmento_quo <- rlang::enquo(.segmento)

    .data %>%
        group_by_at(vars(!!segmento_quo)) %>%
        mutate(prop = .data$casos / sum(.data$casos)) %>%
        ungroup()
}


tabla_prop_val <- function(.data, .var, .segmento, miss) {
    # Cálculo de porcetaje de respuestas válidas en tabla con numero de casos.

    # Pasar de quosure con texto a string y luego simbolo.
    var_quo <- rlang::sym(rlang::as_name(.var))
    segmento_quo <- enquo(.segmento)

    .data %>%
        group_by_at(vars(!!segmento_quo)) %>%
        mutate(casos_val = replace(.data$casos, (!!var_quo %in% miss), NA_real_),
               prop_val = .data$casos_val / sum(.data$casos_val, na.rm = TRUE)) %>%
        select(-.data$casos_val) %>%
        ungroup()
}

tabla_total <- function(.data,
                        .var,
                        .segmento,
                        miss = NULL) {
    # Cálculo de porcetaje para el total de segmento

    tab_total <- .data %>%
        group_by_at(vars({{ .var }})) %>%
        summarise_at(vars(.data$casos), ~sum(.)) %>%
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
        transmute_at(vars(!!segmento_quo, !!var_quo, !!wt_quo),
                     sjmisc::to_label, add.non.labelled = TRUE) %>%
        group_by_at(vars(!!segmento_quo, !!var_quo)) %>%
        summarise(casos = sum(!!wt_quo %||% n())) %>%
        ungroup()

    # Agrega el porcentaje de respuesta.
    tab <- tabla_prop(tab,
                      .segmento = !!segmento_quo)

    # Agrega el porcentaje válido si es que se señalan categorias perdidas.
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

        tab <- tabla_var_segmento(.data,
                                  .var = {{.var}},
                                  .segmento = {{.seg}},
                                  total = total,
                                  .wt = {{.wt}},
                                  miss = miss) %>%
            mutate(segmento_var = !!rlang::as_label(enquo(.seg))) %>%
            rename(segmento_cat = !!rlang::as_label(enquo(.seg))) %>%
            mutate_at(vars(.data$segmento_var, .data$segmento_cat), as.character)
    }

    tab <- map(.segmentos, ~tabla_var_seg(.data, .seg = !!.))

    tab <- reduce(tab, bind_rows) %>%
        mutate_at(vars(.data$segmento_cat), forcats::as_factor)

    tab <- sjlabelled::copy_labels(df_new = tab,
                                   df_origin = .data)

    tab %>%
        select(.data$segmento_var,
               .data$segmento_cat, everything())
}

#' @title Tabla de porcentajes de variables según segmentos.
#'
#'  Obtiene porcentajes de respuestas de múltiples variables según multiples segmentos.
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
#' @importFrom sjlabelled get_label
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
    wt_quo <- enquo(.wt)

    tab <- purrr::map(variables, ~tabla_var_segmentos(.data,
                                                      .var = !!.,
                                                      .segmentos = .segmentos,
                                                      .wt = !!wt_quo,
                                                      total = total,
                                                      miss = miss))

    tabla_variables <- function(.data, .var) {

        var_label <- sjlabelled::get_label(.data, .var)

        .data %>%
            mutate(pregunta_var = .var,
                   pregunta_lab = var_label) %>%
            rename(pregunta_cat = .var) %>%
            mutate_at(vars(.data$pregunta_var),
                      as.character)
    }

    map2(tab, variables, ~tabla_variables(.x, .y)) %>%
        reduce(bind_rows) %>%
        select(starts_with("segmento"),
               .data$pregunta_var, .data$pregunta_lab, .data$pregunta_cat,
               everything()) %>%
        mutate_at(vars(.data$segmento_var, .data$pregunta_var, .data$pregunta_lab),
                  forcats::as_factor)
}
