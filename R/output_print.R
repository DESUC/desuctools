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
                      ...,
                      width = 50,
                      ellipsis = '...',
                      weights = NULL) {
    # frecuencia de variable truncando las etiquetas para mejorar visualización.

    if (is.data.frame(.data)) {
        var_sel <- tidyselect::vars_select(colnames(.data), ...)
        data <- .data[var_sel]
    } else {
        data <- .data
    }

    labels_trunc <- purrr::map(sjlabelled::get_labels(data),
                               ~ str_trunc(., width = width, ellipsis = ellipsis))

    if (!is.data.frame(data)) {
        (labels_trunc <- unlist(labels_trunc))
    }

    if (is.null({{weights}})) {
        vct_weights <- NULL
    } else {
        if (is.vector({{weights}})) {
            print('vect')
            vct_weights <- weights
        } else {
            print('nom')
            vct_weights <- .data[!!rlang::as_string(weights)]
        }
    }
    print(vct_weights)

    data <- sjlabelled::set_labels(data, labels = labels_trunc)

    sjmisc::frq(data, weights = vct_weights)
}


#' Función para ajustar estilo a tablas al momento de ser mostradas en un informe
#'
#' @name kable_desuc
#'
#' @param .data Una data frame
#' @param digits `int` Número de decimales en la tabla (por defecto digits = 1)
#' @param row.names `Logical` Se incluye o no los nombres de las filas.
#' @param col.names `strings` Vector de texto para los nombres de las columnas.
#'    (por defecto igual al colname de la tabla)
#' @param align `vector` Indicación para la alineación de las columnas. c('rll')
#' @param caption `string` Leyenda asociada a la tabla (por defecto sin leyenda)
#' @param escape `bolean` Si se normalizan los caracterres especiales en html o latex.
#'    (por defecto TRUE)
#' @param ... Atributos pasados a la función `kable_styling`
#' @param booktabs `bolean` si usa estilo booktabs para la tabla (por defecto TRUE)
#' @param longtable `bolean` si usa estilo para tablas que cruzan dos páginas (por defecto FALSE)
#' @param font_size `ìnt` tamaño de letra en la tabla (por defecto 8)
#'
#' @return Una kable con el formato DESUC
#'
#' @importFrom knitr kable
#'
#' @export
kable_desuc <- function(.data,
                        digits = 1,
                        row.names = NA,
                        col.names = NA,
                        align = NULL,
                        caption = NULL,
                        booktabs = TRUE,
                        longtable = FALSE,
                        escape = TRUE,
                        font_size = 8,
                        ...){
    # Ajustes de formatos para tablas según estilo DESUC.
    .data %>%
        knitr::kable(digits = digits,
                     row.names = row.names,
                     col.names = col.names,
                     align = align,
                     caption = caption,
                     booktabs = booktabs,
                     longtable = longtable,
                     escape = escape,
                     linesep = "",
                     format.args = list(decimal.mark = ',', big.mark = ".")) %>%
        kableExtra::kable_styling(latex_options = "hold_position",
                                  repeat_header_text = '(continuaci\u00f3n)',
                                  position = "center",
                                  font_size = font_size,
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
#' @param ... path al documento .Rmd
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
