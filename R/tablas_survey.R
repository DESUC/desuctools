# Funciones para análisis de datos de encuestas
# 181228

#' @name svr_tabla_var_segmento
#'
#' Devuelve tabla de frecuencias con intervalos de confianza para un nivel
#' `level`de significancia entre las categorías de respuesta de la
#' variable `.var`.
#'
#'
#' @param .data data frame con diseño complejo
#'
#' @param .var Variable en la que interesa comparar categorías de respuesta.
#' @param .segmentos Segmentos de interés para ver diferencias en categorías
#'        de variable `.var`. Por defecto NULL
#' @param na.rm boolean. Considera o no missings, por defecto FALSE.
#' @param level double. Nivel de significancia para intervalos de confianza
#'
#' @export
svr_tabla_var_segmento <- function(.data,
                                   .var,
                                   .segmentos = NULL,
                                   na.rm = FALSE,
                                   level = 0.95){

  if(!any(class(.data) %in% 'tbl_svy')) stop('Se necesita un data.frame con diseño complejo')

  # Función para cálculo de proporción de respuestas de '.var'

  tab <- .data %>%
    transmute(segmento_var = rlang::as_label(enquo(.segmentos))      %||% 'Total',
              segmento_lab = sjlabelled::get_label({{ .segmentos }}) %||% FALSE,
              segmento_cat = sjmisc::to_label({{ .segmentos }}       %||% FALSE),
              {{ .var }} := sjmisc::to_label({{ .var }}, add.non.labelled = TRUE)) %>%
    group_by(segmento_var, segmento_lab, segmento_cat,
             pregunta_var = rlang::as_label(enquo(.var)),
             pregunta_lab = sjlabelled::get_label({{ .var }}) %||% 'Sin etiqueta',
             pregunta_cat = sjmisc::to_label({{ .var }})) %>%
    summarise(prop = survey_mean(na.rm = na.rm, vartype = c('ci', 'se'), level = level)) %>%
    group_by(segmento_cat) %>%
    mutate(diff_sig = min(prop_upp) < prop_low | max(prop_low) > prop_upp) %>%
    ungroup()

  if(rlang::quo_is_null(enquo(.segmentos))){
    tab %>%
      select(-starts_with('segmento'))
  } else {
    tab
  }
}

segmentos_frq <- function(base, variable, ...){

  variable_quo <- enquo(variable)
  segmentos_quo <- enquos(...)

  segmento_frq <- function(base, variable, segmento){

    variable_quo <- enquo(variable)
    segmento_quo <- enquo(segmento)

    seg_categorias <- get_labels(base[['variables']][[rlang::quo_text(segmento_quo)]], drop.unused = TRUE)

    out_seg_cat_frq <- function(base, variable, segmento, seg_cat){
      variable_quo <- enquo(variable)
      segmento_quo <- enquo(segmento)

      out <- base %>%
        filter(to_label(!!segmento_quo) == seg_cat) %>%
        pregunta_frq(!!variable_quo) %>%
        mutate(segmento = rlang::quo_text(segmento_quo),
               seg_label = get_label(base[['variables']][[rlang::quo_text(segmento_quo)]]) %||% NA_character_,
               seg_cat = seg_cat) %>%
        select(segmento, seg_label, seg_cat, everything())

      return(out)
    }
    map_df(seg_categorias, ~ out_seg_cat_frq(base, !!variable_quo, !!segmento_quo, .))

  }
  tabla <- map_df(exprs(!!!segmentos_quo), ~ segmento_frq(base, variable = !!variable_quo, segmento = !!.))

  tabla %>%
    mutate_at(vars(seg_label, seg_cat, var_labels), forcats::as_factor)

}
