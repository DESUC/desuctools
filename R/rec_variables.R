# Recodificación

# Función para recodifircar variables y agregarle etiquetas a las
rec_cat <- function(variable,
                    rec,
                    labels = NULL){

  sjmisc::rec(variable,
              rec = rec) %>%
    haven::labelled(labels = labels,
                    label = attr(variable, 'label'))
}

#' @title Recodificaión de variables 5 a 3
#'
#' @description
#' Por defecto recodifica variables _likert_ de 5 niveles a 3 niveles.
#'
#' @param variable numeric, Valores de variable a recodificar.
#' @param rec string, por defecto recodifica 1:2 en 1, 3 en 2 y 4:5 en 3.
#'        Todo lo demás lo deja como 9
#' @param labels vector string, etiquetas para las variables recodificadas.
#'        Por defecto NULL.
#'
#' @importFrom haven labelled
#' @importFrom sjmisc rec
#'
#' @return haven_labelled
#' @export
#'
#' @examples
#'
#' vect <- c(1, 4, 6, 99, NA)
#' rec_cat_5a3(vect, labels = c('alto' = 1))
#'
rec_cat_5a3 <- function(variable,
                        rec = "1:2 = 1; 3 = 2; 4:5 = 3; else = 9",
                        labels = NULL){

  rec_cat(variable = variable,
          rec = rec,
          labels = labels)
}

#' @title Recodificaión de variables 7 a 3
#'
#' @description
#' Por defecto recodifica variables de _notas_ de 7 niveles a 3 niveles.
#'
#' @param variable numeric, Valores de variable a recodificar.
#' @param rec string, por defecto recodifica 1:4 en 1, 5 en 2 y 6:7 en 3.
#'        Todo lo demás lo deja como 9
#' @param labels vector string, etiquetas para las variables recodificadas.
#'        Por defecto NULL.
#'
#' @importFrom haven labelled
#' @importFrom sjmisc rec
#'
#' @return haven_labelled
#' @export
#'
#' @examples
#'
#' vect <- c(1, 4, 6, 99, NA)
#' rec_cat_7a3(vect, labels = c('alto' = 1))
#'
rec_cat_7a3 <- function(variable,
                        rec = "1:4 = 1; 4 = 2; 6:7 = 3; else = 9",
                        labels = NULL){

  rec_cat(variable = variable,
          rec = rec,
          labels = labels)
}
