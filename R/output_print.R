#' Frecuencia de variable con etiqueta truncada
#'
#' Se trunca el texto de la etiqueta de la variable a un largo preestablecido para
#' una mejor visión de la tabla resultante.
#'
#' @name frq_trunc
#'
#' @param .data Una data frame o vector
#' @param .var nombre de la variable de la que se quiere saber su frecuencia
#' @param width = 50 Largo del texto de la etiqueta de la variable.
#'
#' @return Una kable con el formato DESUC
#'
#' @import dplyr
#' @importFrom sjmisc frq
#' @importFrom haven labelled
#' @importFrom stringr str_trunc
#'
#' @export
frq_trunc <- function(.data,
                      .var = NULL,
                      width = 50) {
    # frecuencia de variable truncando las etiquetas para mejorar visualización.

    var <- enquo(.var)

    if (is.numeric(.data)) {
        vect <- .data
    } else {
        vect <- .data %>% pull(!!var)
    }

    labels_trunc <- attr(vect, "labels")
    names(labels_trunc) <- stringr::str_trunc(names(labels_trunc), width = width)

    haven::labelled(vect, labels = labels_trunc) %>%
        sjmisc::frq()
}


#' Función para ajustar estilo a tablas al momento de ser mostradas en un informe
#'
#' @name kable_desuc
#'
#' @param .data Una data frame
#' @param caption `string` Leyenda asociada a la tabla (por defecto sin leyenda)
#' @param digits `int` Número de decimales en la tabla (por defecto digits = 1)
#' @param col.names `strings` Vector de texto para los nombres de las columnas.
#'    (por defecto igual al colname de la tabla)
#' @param escape `bolean` Si se normalizan los caracterres especiales en html o latex.
#'    (por defecto TRUE)
#' @param ... Atributos pasados a la función `kable_styling`
#'
#' @return Una kable con el formato DESUC
#'
#' @importFrom knitr kable
#'
#' @export
kable_desuc <- function(.data,
                        caption = NULL,
                        digits = 1,
                        col.names = NA,
                        escape = TRUE,
                        ...){
    # Ajustes de formatos para tablas según estilo DESUC.
    .data %>%
        knitr::kable(caption = caption,
                     booktabs = TRUE,
                     digits = digits,
                     col.names = col.names,
                     escape = escape,
                     linesep = "",
                     format.args = list(decimal.mark = ',', big.mark = ".")) %>%
        kableExtra::kable_styling(latex_options = "hold_position",
                                  position = "center",
                                  font_size = 8,
                                  ...)
}


#' @title Divide un data.frame en columnas
#'
#' Toma una tabla y la divide y pega en sucesicas columnas para imprimir en menor espacio.
#'
#' @name tabla_columnas
#'
#' @param data tibble
#' @param ncols integer. Número de columnas en las que se quiere dividir la data original.
#'    Por defecto ncols = 2.
#'
#' @import dplyr
#' @importFrom purrr reduce
#'
#' @return tibble
#' @export
#'
tabla_columnas <- function(data, ncols = 2){

    tab <- data %>%
        janitor::adorn_totals('row') %>%
        mutate(col = ceiling((1:n())/n() * ncols)) %>%
        group_nest(col)

    tab <- purrr::reduce(tab$data, bind_cols)

    colnames(tab) <- rep(names(data), ncols)

    return(tab)
}


#' Ajuste del tamaño del texto en chunks
#'
#' @name chunk_size
#'
#' @export
chunk_size <- function(...) {
    # Cambio de tamaño del chunk.
    # Obtenido desde
    # https://stackoverflow.com/questions/25646333/code-chunk-font-size-in-rmarkdown-with-knitr-and-latex

    fmt <- rmarkdown::pdf_document(...)

    fmt$knitr$knit_hooks$size = function(before, options, envir) {
        if (before) return(paste0("\n \\", options$size, "\n\n"))
        else return("\n\n \\normalsize \n")
    }

    return(fmt)
}
