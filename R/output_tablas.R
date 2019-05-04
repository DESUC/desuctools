#' Tablas de resultados ----------------------------------------------------
#'
#' @import dplyr
#'
#' @export
tabla_segmentos <- function(.data, ..., wt = NULL) {
    # Tabla con número de casos y proporción de respuestas por segmentos.

    segmentos <- tidyselect::vars_select(names(.data), ...)
    wt_quo <- enquo(wt)

    seg_labels <- sjlabelled::get_label(.data, segmentos)

    tabla <- .data %>%
        transmute_at(vars(segmentos, !!wt_quo), list(sjlabelled::as_label)) %>%
        group_by_at(vars(segmentos)) %>%
        summarise(n = sum(!!wt_quo %||% n())) %>%
        gather("variable", "categoria", -n) %>%
        mutate(variable = forcats::as_factor(variable))

    tabla <- tabla %>%
        count(variable, categoria = forcats::as_factor(categoria), wt = n) %>%
        group_by(variable) %>%
        mutate(prop = n/sum(n)) %>%
        rename(casos = n) %>%
        ungroup() %>%
        identity()

    tabla %>%
        mutate(var_label = forcats::as_factor(seg_labels[variable])) %>%
        select(starts_with('var'), everything())
}

#' @export
tabla_orden <- function(.data, .var, .segmento = NULL) {
    # Orden de variables y categorias para la presentación de tablas.

    var_quo <- enquo(.var)
    segmento_quo <- enquo(.segmento)

    var_seg_exprs <- rlang::exprs(!!segmento_quo, starts_with(!!rlang::as_label(var_quo)))

    .data %>%
        select_at(vars(!!!var_seg_exprs, everything())) %>%
        arrange_at(vars(!!!var_seg_exprs))
}

#' @export
tabla_prop <- function(.data, .segmento) {
    # Cálculo de porcetaje de respuestas en tabla con numero de casos.

    segmento_quo <- enquo(.segmento)

    .data %>%
        group_by_at(vars(!!segmento_quo)) %>%
        mutate(prop = casos/sum(casos)) %>%
        ungroup()
}

#' @export
tabla_prop_val <- function(.data, .var, .segmento, miss) {
    # Cálculo de porcetaje de respuestas válidas en tabla con numero de casos.

    var_quo <- enquo(.var)
    segmento_quo <- enquo(.segmento)

    .data %>%
        group_by_at(vars(!!segmento_quo)) %>%
        mutate(casos_val = if_else(!(!!var_quo %in% miss), as.double(casos), NA_real_), prop_val = casos_val/sum(casos_val,
                                                                                                                 na.rm = TRUE)) %>%
        select(-casos_val) %>%
        ungroup()
}

#' @export
tabla_total <- function(.data, .var, .segmento,
                        miss = NULL) {
    # Cálculo de porcetaje para el total de segmento

    var_quo <- enquo(.var)
    segmento_quo <- enquo(.segmento)

    tab_total <- .data %>%
        group_by_at(vars(!!var_quo)) %>%
        summarise_at(vars(casos), ~sum(.)) %>%
        mutate(`:=`(!!segmento_quo, "Total")) %>%
        ungroup()

    tab_total <- tabla_prop(tab_total, .segmento = NULL)

    # Agrega el porcentaje válido si es que se señalan categorias perdidas.

    tab <- bind_rows(.data %>%
                         mutate(`:=`(!!rlang::as_label(segmento_quo), as.character(!!segmento_quo))), tab_total)

    tab <- tab %>%
        mutate(`:=`(!!segmento_quo, forcats::as_factor(!!segmento_quo))) %>%
        var_labels(`:=`(!!var_quo, !!get_label(.data, !!var_quo)))

    if (!is.null(miss)) {
        tab <- tabla_prop_val(tab, .var = !!var_quo, .segmento = !!segmento_quo, miss = miss)
    }
    return(tab)
}

#' @export
tabla_var_segmento <- function(.data, .var,
                               .segmento = NULL, .wt = NULL, total = FALSE, miss = NULL) {
    # Tabla con número de casos y proporción de variable Se agrega una variable de de segmentación llamada 'segmento' con valor 'Total'.

    var_quo <- enquo(.var)
    segmento_quo <- enquo(.segmento)
    wt_quo <- enquo(.wt)

    tab <- .data %>%
        transmute_at(vars(!!segmento_quo, !!var_quo, !!wt_quo),
                     sjlabelled::as_label, add.non.labelled = TRUE) %>%
        group_by_at(vars(!!segmento_quo, !!var_quo)) %>%
        summarise(casos = sum(!!wt_quo %||%
                                  n())) %>%
        ungroup()

    # Agrega el porcentaje de respuesta.
    tab <- tabla_prop(tab, .segmento = !!segmento_quo)

    # Agrega el porcentaje válido si es que se señalan categorias perdidas.
    if (!is.null(miss)) {
        tab <- tabla_prop_val(tab, .var = !!var_quo, .segmento = !!segmento_quo, miss = miss)
    }

    # Agrega el porcentaje válido si es que se señalan categorias perdidas.
    if (total) {
        tab <- tabla_total(tab, .var = !!var_quo, .segmento = !!segmento_quo, miss = miss)
    }

    tabla_orden(tab, .var = !!var_quo, .segmento = !!segmento_quo)
}

#' @export
tabla_var_segmentos <- function(.data, .var, .segmentos,
                                .wt = NULL, total = FALSE, miss = NULL) {
    # Resultados de una pregunta `.var` para varios segmentos `.segmentos`

    var_quo <- enquo(.var)
    wt_quo <- enquo(.wt)

    tabla_var_seg <- function(.data, .seg) {
        segmento_quo <- enquo(.seg)

        tab <- tabla_var_segmento(.data,
                                  .var = !!var_quo,
                                  .seg = !!segmento_quo,
                                  total = total,
                                  .wt = !!wt_quo, miss = miss) %>%
            mutate(segmento_var = !!rlang::as_label(segmento_quo)) %>%
            rename(segmento_cat = !!rlang::as_label(segmento_quo)) %>%
            mutate_at(vars(segmento_var, segmento_cat), as.character)
    }

    tab <- map(.segmentos, ~tabla_var_seg(.data, .seg = !!.))

    reduce(tab, bind_rows) %>%
        mutate_at(vars(segmento_cat), forcats::as_factor) %>%
        var_labels(`:=`(!!var_quo, !!get_label(.data, !!var_quo))) %>%
        select(segmento_var,
               segmento_cat, everything())
}

#' @export
tabla_vars_segmentos <- function(.data, .vars, .segmentos,
                                 .wt = NULL, total = FALSE, miss = NULL) {
    # Resultados de varias preguntas `.var` para varios segmentos `.segmentos`

    wt_quo <- enquo(.wt)

    tab <- map(.vars, ~tabla_var_segmentos(.data,
                                           .var = !!.,
                                           .segmentos = .segmentos,
                                           .wt = !!wt_quo,
                                           total = total, miss = miss))

    tabla_variables <- function(.data, .var) {
        var_quo <- enquo(.var)

        .data %>%
            mutate(pregunta_var = !!rlang::as_label(var_quo), pregunta_lab = get_label(!!var_quo)) %>%
            rename(pregunta_cat = !!rlang::as_label(var_quo)) %>%
            mutate_at(vars(pregunta_var),
                      as.character)
    }

    map2(tab, .vars, ~tabla_variables(.x, !!.y)) %>%
        reduce(bind_rows) %>%
        select(starts_with("segmento"),
               pregunta_var, pregunta_lab, pregunta_cat, everything()) %>%
        mutate_at(vars(segmento_var, pregunta_var, pregunta_lab), forcats::as_factor)
}
