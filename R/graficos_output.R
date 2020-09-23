
#' @title Barras 3 niveles positivo neutro negativo
#'
#' Gr\\u00e1fico de barras dise\\u00f1ado para comparar categor\\u00eda positiva, negativa y neutra.
#'
#' @param .df `data.frame` Debe contener variables `pregunta_lab` y `pregunta_cat`.
#'   Funciona bien a partir de data.frame de resultado de funci\\u00f3n `tabla_vars_segmentos`.
#' @param x `quo` Nombre de variable a utilizar en eje X.
#' @param title `chr` T\\u00edtulo del gr\\u00e1fico.
#' @param subtitle `chr` Subt\\u00edtulo del gr\\u00e1fico.
#' @param caption `chr` Caption del gr\\u00e1fico.
#' @param missing `chr` vector con categor\\u00edas de respuesta consideradas 'missing'.
#' @param text_size `num` tama\\u00f1o de letra.
#' @param flip `logical` TRUE gira los ejes.
#' @param colour_neg_neu_pos Vector con tres colores para negativo, neutro y positivo.
#' @param y_prop `chr` Variable con valor de proporciones a graficar.
#' @param y_na `dbl` posici\\u00f3n de la etiqueta en y de valores missing.
#' @param x_na `dbl` posici\\u00f3n de la etiqueta en x de valores missing.
#' @param facet_col Variable de facet columna.
#' @param facet_row Variable de facet fila.
#' @param x_str_entre_ini `chr` caracter desde el cual se cortar\\u00e1 la etiqueta de x.
#'        El caracter no queda incluido. Si queda en blanco '', parte desde el inicio,
#' @param x_str_entre_fin `chr` caracter hasta donde se cortar\\u00e1 la etiqueta de x.
#'        El caracter no queda incluido. Si queda en blanco '', termina al final.
#' @param x_str_width `int` numero de caracteres para wrap las etiquetas de x.
#' @param colour_na color para los valores de dato missing, si se incluye.
#' @param font_family letra a utilizar en el gr\\u00e1fico. Por defecto se usa 'Calibre'.
#'
#' @import ggplot2
#' @importFrom stringr str_wrap
#' @importFrom tidyr replace_na
#' @importFrom rlang .data
#' @importFrom scales percent
#'
#' @return ggplot
#' @export
#'
#' @examples
#' df_chart <- data.frame(pregunta_lab = c(rep('a', 4), rep('b', 4)),
#'                        x_other = c(rep('x', 4), rep('y', 4)),
#'                        prop = c(-0.1, 0.3, 0.4, 0.1, -0.3, 0.1, 0.4, 0.05),
#'                        pregunta_cat = factor(rep(c('bajo', 'medio', 'alto', 'ns'), 2),
#'                                              levels = c('bajo', 'medio', 'alto', 'ns')))
#'
#' gg_bar_3_niveles_stack(df_chart,
#'                        missing = 'ns',
#'                        title = 'Prueba',
#'                        font_family = NULL)
#'
gg_bar_3_niveles_stack <- function(.df,
                                   x = pregunta_lab,
                                   title = NULL,
                                   subtitle = NULL,
                                   caption = NULL,
                                   missing = NULL,
                                   text_size = 3,
                                   flip = TRUE,
                                   colour_neg_neu_pos = c('#C00001', '#FFC000', '#20497D'),
                                   y_prop = prop,
                                   y_na = 1.1,
                                   x_na = 0.6,
                                   facet_col = NULL,
                                   facet_row = NULL,
                                   x_str_entre_ini = '',
                                   x_str_entre_fin = '',
                                   x_str_width = 50,
                                   colour_na = 'grey20',
                                   font_family = 'Calibri'
) {

  # Revisar que est\\u00e9n las variables necesarias en la tabla de datos
  var_check <- c('pregunta_lab', 'prop', 'pregunta_cat')

  if(!all(sapply(var_check, function(x) any(names(.df) %in% x)))) {
    stop(paste("No est\u00e1n presentes en ",
               deparse(substitute(.df))
               ,"alguna de las variables 'pregunta_lab', 'prop', 'pregunta_cat'"))
  }
  # Revisar que hayan 4 niveles de respuesta y missing
  if(length(levels(droplevels(.df[['pregunta_cat']]))) >= 4 & is.null(missing)) {
    stop("4 niveles o m\\u00e1s en pregunta_cat sin missing expl\\u00edcito")
  }

  gg_niv3 <- .df %>%
    filter(!.data$pregunta_cat %in% missing) %>%
    ggplot(aes(x = {{ x }},
               y = {{ y_prop }},
               fill = .data[['pregunta_cat']])) +
    geom_col(width = .5,
             position = position_stack(reverse = TRUE)) +
    geom_hline(yintercept = 0, colour = 'grey30') +
    geom_text(aes(label = abs(round({{ y_prop }} * 100))),
              position = position_stack(vjust = 0.5, reverse = TRUE),
              size = rel(text_size),
              family = font_family %||% '',
              fontface = 'bold',
              colour = 'white') +
    scale_x_discrete('',
                     labels = function(x) desuctools::str_entre(x,
                                                                ini = x_str_entre_ini,
                                                                fin = x_str_entre_fin) %>%
                       stringr::str_wrap(width = x_str_width)) +
    scale_y_continuous('% de respuestas',
                       labels = function(x) scales::percent(abs(x))) +
    scale_fill_manual('',
                      values = colour_neg_neu_pos) +
    theme_minimal() +
    theme(legend.position = 'top',
          legend.key.size = unit(1, 'char'),
          text = element_text(family = font_family))

  # Opciones para girar o no las coordenadas.
  # Se deja clip = 'off' para que se vea bien el dato missing si es que se tiene.
  if (flip) {
    gg_niv3 <- gg_niv3 + coord_flip(clip = 'off')
  } else {
    gg_niv3 <- gg_niv3 + coord_cartesian(clip = 'off')
  }

  if(!is.null(missing)){
    tab_ns <- .df %>%
      filter(.data$pregunta_cat %in% missing) %>%
      group_by(across(c({{ x }},
                        {{ facet_col }},
                        {{ facet_row }}))) %>%
      summarise(pregunta_cat = str_c(.data$pregunta_cat, collapse = '/'),
                prop = sum(.data$prop)) %>%
      tidyr::replace_na(list('prop' = 0))

    pos_x_annotate <- length(unique(.df[[rlang::as_name(enquo(x))]]))

    gg_niv3 <- gg_niv3 +
      geom_text(data = tab_ns,
                aes(label = round({{ y_prop }} * 100), fill = NULL),
                y = y_na,
                size = rel(text_size),
                hjust = if(flip) 1 else .5,
                family = font_family %||% '',
                fontface = 'plain',
                colour = colour_na) +
      annotate(geom = 'text',
               label = str_c(missing, collapse = ' '),
               x = pos_x_annotate + x_na,
               y = y_na,
               size = rel(text_size),
               hjust = 1,
               family = font_family %||% '',
               fontface = 'plain',
               colour = colour_na)
  }

  gg_niv3 +
    facet_grid(cols = vars({{ facet_col }}),
               rows = vars({{ facet_row }})) +
    labs(title = title,
         subtitle = subtitle,
         caption = caption)
}
