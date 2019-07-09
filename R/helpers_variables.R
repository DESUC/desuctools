#' Funciones de trabajo con datos ------------------------------------------
#'
#' @title Corregir missings en preguntas múltiples
#'
#' Función para corregir problemas de no respuesta en preguntas múltiples
#'
#' @name shift_missing
#'
#' @param .data Una data frame
#' @param .x nombre de la variable la primera variable
#' @param .y nombre de la variable la primera variable
#' @param missing vector con valores considerados no válidos (por defecto \code{77, 88, 99})
#'
#' @return Una data frame con los valores de .x y .y corregidos
#'
#' @import dplyr
#' @importFrom rlang quo_is_null as_label
#' @importFrom haven labelled
#'
#' @export
shift_missing <- function(.data, .x, .y = NULL, missing = c(77L, 88L, 99L)) {
    # Revisa preguntas de respuesta múltiple para corregir respuestas inválidas en variables posteriores.
    missing <- c(missing, NA)

    var1 <- enquo(.x)
    var2 <- enquo(.y)

    add_labels <- function(x, var_labels = NULL) {
        haven::labelled(x, labels = attr(var_labels, "labels"))
    }

    x <- pull(.data, !!var1)

    if (rlang::quo_is_null(var2)) {

        x_lab <- if_else(x %in% missing, NA_integer_, as.integer(x)) %>% add_labels(x)

        .data %>% mutate(`:=`(!!rlang::as_label(var1), x_lab))

    } else {
        y <- pull(.data, !!var2)

        x_lab <- if_else(x %in% missing & !(y %in% missing), as.integer(y), as.integer(x)) %>% add_labels(x)
        y_lab <- if_else(y %in% missing, NA_integer_, as.integer(y)) %>% add_labels(y)

        .data %>% mutate(`:=`(!!rlang::as_label(var1), x_lab),
                         `:=`(!!rlang::as_label(var2), y_lab))
    }

}


#' @title Colapso de strings
#'
#' Variable útil para colapsar en una sola casilla _list variables_ en un
#' tibble que quiera ser mostrad en una tabla
#'
#' @param .data tibble
#'
#' @param .var variable name
#' @param collapse string, string con el que se unirán los textos. Por defecto
#'        ", ".
#'
#' @importFrom dplyr %>% pull enquo
#' @importFrom stringr str_c
#' @export
collapse_chr <- function(.data, .var, collapse = ", ") {
    # Colapsar valores de una variable en un string.

    var_quo <- enquo(.var)

    pull(.data, !!var_quo) %>%
        stringr::str_c(., collapse = collapse)
}


#' @title Extrae string entre dos textos
#'
#' @param text string. Puede ser un named string.
#' @param ini string, desde donde se extrae el texto
#' @param fin string, hasta donde se extrae el texto
#'
#' @return string
#'
#' @importFrom stringr str_extract str_glue str_squish
#'
#' @export
#'
str_entre <- function(text,
                      ini = '',
                      fin = ''){
    structure(stringr::str_extract(text,
                                   stringr::str_glue('(?<={ini}).*(?={fin})')) %>%
                  stringr::str_squish(),
              names = names(text))
}


#' @title Extrae string entre paréntesis
#'
#' Función para extraer texto presente entre paréntesis.
#' Creado para trabajo con etiquetas de variables y extraer así el concepto
#' preguntado.
#'
#' Si no encuentra texto entre paréntesis, devuelve el texto original.
#'
#' @name str_entre_parentesis
#'
#' @param text string
#'
#' @return string
#'
#' @importFrom stringr str_extract
#'
#' @export
str_entre_parentesis <- function(text){

    # Extrae texto entre paréntesis
    text_extract <- str_entre(text, ini = '\\(', fin = '\\)')

    if (is.na(nchar(text_extract))) {
        text_extract <- text
    }

    return(text_extract)
}

