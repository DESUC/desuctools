
#' @title Barras 3 niveles positivo neutro negativo
#'
#' Gráfico de barras diseñado para comparar categoría positiva, negativa y neutra.
#'
#' @param .data `data.frame` Debe contener variables `pregunta_lab` y `pregunta_cat`.
#'   Funciona bien a partir de data.frame de resultado de función `tabla_vars_segmentos`.
#' @param x `quo` Nombre de variable a utilizar en eje X.
#' @param title `chr` Título del gráfico
#' @param subtitle `chr` Subtítulo del gráfico
#' @param caption `chr` Caption del gráfico
#' @param missing `chr` vector con categorías de respuesta consideradas 'missing'
#' @param text_size `num` tamaño de letra
#' @param flip `logical` TRUE gira los ejes.
#' @param colour_neg_neu_pos
#' @param y_na
#' @param x_str_entre_ini
#' @param x_str_entre_fin
#' @param x_str_width
#' @param colour_na
#' @param font_family
#'
#' @import ggplot2
#' @importFrom scales percent
#' @importFrom stringr str_wrap
#' @importFrom magrittr %>%
#'
#' @return ggplot
#' @export
#'
#' @examples
#'   df_chart <- data.frame(pregunta_lab = c(rep('a', 4), rep('b', 4)),
#'                          x_other = c(rep('x', 4), rep('y', 4)),
#'                          prop = c(-0.1, 0.3, 0.4, 0.1, -0.3, 0.1, 0.4, 0.05),
#'                          pregunta_cat = factor(rep(c('bajo', 'medio', 'alto', 'ns'), 2),
#'                                                levels = c('bajo', 'medio', 'alto', 'ns')))
#'
#' gg_bar_3_niveles_stack(df_chart,
#'                        missing = 'ns',
#'                        title = 'Gráfico de prueba')
#'
gg_bar_3_niveles_stack <- function(.data,
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
                                   x_str_entre_ini = '',
                                   x_str_entre_fin = '',
                                   x_str_width = 50,
                                   colour_na = 'grey20',
                                   font_family = 'Calibri'
) {

  # Revisar que estén las variables necesarias en la tabla de datos
  var_check <- c('pregunta_lab', 'prop', 'pregunta_cat')
  if(!all(sapply(var_check, function(x) any(names(.data) %in% x)))) {
    stop(paste("No están presentes en ",
               deparse(substitute(.data))
               ,"alguna de las variables 'pregunta_lab', 'prop', 'pregunta_cat'"))
  }
  # Revisar que hayan 4 niveles de respuesta y missing
  if(length(levels(droplevels(.data[['pregunta_cat']]))) >= 4 & is.null(missing)) {
    stop("4 niveles o más en pregunta_cat sin missing explícito")
  }

  gg_niv3 <- .data %>%
    filter(!pregunta_cat %in% missing) %>%
    ggplot(aes(x = {{ x }}, y = {{ y_prop }}, fill = pregunta_cat)) +
    geom_col(width = .5,
             position = position_stack(reverse = TRUE)) +
    geom_hline(yintercept = 0, colour = 'grey30') +
    geom_text(aes(label = abs(round(..y.. * 100))),
              position = position_stack(vjust = 0.5, reverse = TRUE),
              size = rel(text_size),
              family = font_family, fontface = 'bold',
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
    tab_ns <- .data %>%
      filter(pregunta_cat == missing)

    pos_x_annotate <- length(unique(.data[[rlang::as_name(enquo(x))]]))

    gg_niv3 <- gg_niv3 +
      geom_text(data = tab_ns,
                aes(label = round({{ y_prop }} * 100), fill = NULL),
                y = y_na,
                size = rel(text_size),
                hjust = if(flip) 1 else .5,
                family = font_family,
                fontface = 'plain',
                colour = colour_na) +
      annotate(geom = 'text',
               label = str_c(missing, collapse = ' '),
               x = pos_x_annotate + .6,
               y = y_na,
               size = rel(text_size),
               hjust = 1,
               family = font_family,
               fontface = 'plain',
               colour = colour_na)
  }

  gg_niv3 +
    labs(title = title,
         subtitle = subtitle,
         caption = caption)
}
