# Funciones para análisis de datos de encuestas
# 181228

# Tablas de frecuencias

pregunta_frq <- function(base, pregunta, na.rm = FALSE){

	# Función para cálculo de proporción de respuestas de 'pregunta'

  pregunta_enq <- enquo(pregunta)

  base %>%
    mutate(!!quo_name(pregunta_enq) := to_label(!!pregunta_enq, add.non.labelled = TRUE)) %>%
    group_by(var_code = !!rlang::quo_text(pregunta_enq),
             var_label = get_label(!!pregunta_enq) %||% 'Sin etiqueta',
             var_labels = to_label(!!pregunta_enq)) %>%
    summarise(frq = survey_mean(na.rm = na.rm, vartype = c('ci', 'se'))) %>%
    ungroup()
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
