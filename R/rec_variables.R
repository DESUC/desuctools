# Recodificación

#' @title Recodificaión de variables
#'
#' Por defecto recodifica variables _likert_ de 5 niveles a 3 niveles.
#'
#' @param variable numeric, Valores de variable a recodificar.
#' @param rec string, por defecto recodifica 1:2 en 1, 3 en 2 y 4:5 en 3.
#'        Todo lo demás lo deja como 9
#' @param labels vector string, etiquetas para las variables recodificadas. Por defecto NULL.
#'
#' @return haven_labelled
#' @export
#'
#' @examples
rec_cat_5a3 <- function(variable,
                        rec = "1:2 = 1;
                               3 = 2;
                               4:5 = 3;
                               else = 9",
                        labels = NULL){

  sjmisc::rec(variable,
              rec = rec) %>%
    haven::labelled(labels = labels,
                    label = sjlabelled::get_label(variable))
}
