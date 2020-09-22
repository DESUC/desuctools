#' Funciones de trabajo con datos ------------------------------------------
#'
#' @title Corregir missings en preguntas múltiples
#'
#' @description
#' Función para corregir problemas de no respuesta en preguntas múltiples y sucesivas.
#'
#' @name shift_missing
#'
#' @param .data Una data frame
#' @param .var1 nombre de la variable la primera variable
#' @param .var2 nombre de la variable la segunda variable
#' @param missing vector con valores considerados no válidos (por defecto \code{77, 88, 99}). `NA` es considerado siempre.
#'
#' @return Una data frame con los valores de .var1 y .var2 corregidos
#'
#' @import dplyr
#' @importFrom rlang quo_is_null as_label :=
#'
#' @export
#'
shift_missing <- function(.data,
                          .var1,
                          .var2 = NULL,
                          missing = c(77L, 88L, 99L)) {
    # Revisa preguntas de respuesta múltiple para corregir respuestas inválidas en variables posteriores.
    missing <- c(missing, NA)

    x <- pull(.data, {{ .var1 }})

    # Posición en donde el vector x es missing
    x_index <- which(x %in% missing)

    if (rlang::quo_is_null(enquo(.var2))) {

        # Para esos casos, reemplazo el valor de y por NA ya que quedó en x
        x[x_index] <- NA_integer_

        .data %>%
            mutate({{ .var1 }} := x)

    } else {
        y <- pull(.data, {{ .var2 }})

        # En esos casos, reemplazo el valor de x por el valor de y
        x[x_index] <- y[x_index]

        # Para esos casos, reemplazo el valor de y por NA ya que quedó en x
        y[x_index] <- NA_integer_

        .data %>%
            mutate({{ .var1 }} := x,
                   {{ .var2 }} := y)
    }

}

#' @title Colapso de strings
#'
#' @description
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
#'
#' @examples
#'
#' df <- data.frame(x = c('a', 'b', 'c'))
#'
#' str_collapse(df, x)
#'
#' @export
#'
str_collapse <- function(.data, .var, collapse = ", ") {
    # Colapsar valores de una variable en un string.

    pull(.data, {{ .var }}) %>%
        stringr::str_c(collapse = collapse)
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
#' @examples
#'
#' str_entre('a (between) z', ini = '\\(', fin = '\\)')
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
#' @description
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
#'
str_entre_parentesis <- function(text){

    # Extrae texto entre paréntesis
    text_extract <- str_entre(text, ini = '\\(', fin = '\\)')

    text_extract <- ifelse(is.na(nchar(text_extract)),
                           text,
                           text_extract)

    return(text_extract)
}


#' @title Número de dígitos enteros.
#'
#' @description
#' Número de dígitos enteros para valores con decimales. Diseñada para casos en que
#' longitud y latitud son capturados como digitos sin decimal y necesitan ser reescalados.
#'
#' @name digitos_entero
#'
#' @param x vector numerico.
#' @param digits cantidad de dígitos del número entero que se quiere obtener.
#'        Por defecto digits = 2.
#'
#' @return vector numerico.
#' @export
#'
#' @examples
#'
#' digitos_entero(c(0.1234, 12.34, 1234, 12345),
#'                digits = 3)
#'
digitos_entero <-  function(x, digits = 2) {
    div <- floor(log10(abs(x))) - digits + 1

    x / 10 ^ div
}


#' @title Validación de sintaxis de email
#'
#' @description
#' Validación de la sintaxis de correos electrónicos.
#' La expresión regular utilizada para ello viene del siguiente post en [SO][https://stackoverflow.com/questions/201323/how-to-validate-an-email-address-using-a-regular-expression]
#'
#' @name is_email
#'
#' @param email vector character.
#'
#' @return vector logico
#' @importFrom stringr str_detect
#'
#' @export
#'
#' @examples
#'
#' is_email(c('a@a.com', 'a@a'))
#'
is_email <- function(email){
    regex_mail <- '(?:[a-z0-9!#$%&\'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&\'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])'

    stringr::str_detect(email , regex_mail)
}


#' @title Orden de preguntas según valor de categoría
#'
#' @description función de ayuda para `forcats::fct_reorder2` con tal de ordenar distintas preguntas
#' según una catogoría de respuesta de interès `cat_orden`.
#'
#' @param .x chr o factor. Variable con categorías de respuesta.
#' @param .y numeric. Valor de respuesta para cada categoría de respuesta
#' @param cat_orden string. Nombre de la categoría de respuesta de interés.
#'
#' @return numeric vector
#'
pregunta_orden <- function(.x, .y, cat_orden){
    max(if_else(.x == cat_orden, .y, 0))
}

#' @title Orden de preguntas según valor de categoría
#'
#' @description Wrapper al rededor de `forcats::fct_reorder2` para ordenar factor `.f` según
#' según una catogoría de respuesta de `.x` de interès `cat_orden` a partir del valor en `.y`.
#'
#' @param .f factor. Factor que quiere ser ordenado.
#' @param .cat chr o factor. Variable con categorías de respuesta.
#' @param .val numeric. Valor de respuesta para cada categoría de respuesta
#' @param cat_orden string. Nombre de la categoría de respuesta de interés.
#' @param .desc logical. Controla si el orden es ascendente o descendente. Por defecto
#'    .desc = FALSE.
#'
#' @return factor
#' @importFrom forcats fct_reorder2
#'
#' @export
#'
fct_reorder_cat <- function(.f, .cat, .val, cat_orden, .desc = FALSE){
    forcats::fct_reorder2(.f, .cat, .val,
                          .fun = pregunta_orden, cat_orden = cat_orden,
                          .desc = .desc)
}


#' @title Orden de región
#'
#' @description Ordena un vector de números asociadas a las 16 regiones de Chile
#' según posición geográfica de norte a sur.
#'
#' @param reg `num`. numérico con valores de 1 a 16
#' @param as.factor `logical`. Por defecto TRUE para crear factor ordenado geográficamente.
#'        FALSE crea variable de clase `labelled`.
#'
#' @return ordered factor
#'
#' @importFrom haven labelled
#' @export
#'
#' @examples
#' region_orden(c(1, 13, 5, 15))
#'
region_orden <- function(reg, as.factor = TRUE){
    # Ordena número de regiones en un factor de norte a sur.
    reg_num <- c(15, 1:5, 13, 6, 7, 16, 8, 9, 14, 10:12)
    reg_nom <- c("Arica y Parinacota",
                 "Tarapaca\u0301",
                 "Antofagasta",
                 "Atacama",
                 "Coquimbo",
                 "Valparai\u0301so",
                 "Metropolitana",
                 "O\u2019Higgins",
                 "Maule",
                 "\u00d1uble",
                 "Biobi\u0301o",
                 "La Araucani\u0301a",
                 "Los Ri\u0301os",
                 "Los Lagos",
                 "Ayse\u0301n",
                 "Magallanes")

    names(reg_num) <- reg_nom

    if(as.factor){
        factor(reg,
               levels = reg_num,
               labels = names(reg_num))
    } else {
        haven::labelled(reg,
                        labels = reg_num,
                        label = 'Regi\u00f3n')
    }
}
