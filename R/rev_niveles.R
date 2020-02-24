#' @title Invertir niveles
#'
#' @description Función para revertir niveles. Especialmente útil si se quiere revertir
#' sólo un rango de valores de una pregunta.
#'
#' @param x `vector`: Vector de datos que se quiere invertir
#' @param niveles_inv `chr`: Niveles de un factor que serán invertidos.
#'    Por defecto, invierte todos los niveles.
#' @param rango_inv `num`: Vector de dos niveles en el que se señala el valor
#'    del primero y último valor que será invertido. Se espera que esos dos valores
#'    definan un rango.
#'    Por defecto, invierte todos los niveles.
#' @param ...
#'
#' @export
rev_niveles <- function(x, ...) {
  UseMethod("rev_niveles")
}


#' @rdname rev_niveles
#' @export
rev_niveles.default <- function(x, ...) {
  rev(x)
}


#' @rdname rev_niveles
#' @export
rev_niveles.factor <- function(x, niveles_inv = NULL, ...) {

  # Niveles del vector
  lev <- levels(x)

  if(is.null(niveles_inv)){
    lev_new <- rev(lev)
  } else {
    # Posiciones de interés de los niveles que se quieren invertir.
    pos <- which(lev %in% niveles_inv)

    # Nuevo orden de niveles de acuerdo a niveles a invertir y otros niveles presentes.
    lev_new <- lev[c(rev(pos), setdiff(seq_along(lev), pos))]
  }

  # Factor con nuevo orden.
  x_new <- factor(x, levels = lev_new)

  # Agregar etiqueta label si es que estaba presente en vector original.
  structure(x_new,
            label = attr(x, 'label', exact = TRUE))
}


#' @rdname rev_niveles
#' @export
rev_niveles.haven_labelled <- function(x, rango_inv = NULL, ...){

  # Función para invertir valores según rango_inv
  valores_inv <- function(val, index_inf, index_sup){
    val_names <- names(val)

    val <- ifelse(val >= val[index_inf] & val <= val[index_sup],
                  val[index_sup] + 1 - val,
                  val)

    structure(val,
              names = val_names)
  }

  # Por defecto (NULL), se invierte todo el rango de números.
  # Para eso tengo que obtener el valor minimo y máximo del vector.
  if (is.null(rango_inv)) {
    rango_inv <- range(x)
  }

  value_pos <- which(x %in% rango_inv)

  x_new <- valores_inv(x,
                       index_inf = value_pos[1],
                       index_sup = value_pos[2])

  # Indice de etiquetas.
  # Solo entre los valores buscados si es que se especifican.
  # Todo el rango si es que no está disponible.

  labels_x <- attr(x, 'labels', exact = TRUE)

  if(!is.null(labels_x)) {
    # Posiciones de las etiquetas de los valores a invertir
    labels_pos <- which(labels_x %in% rango_inv)

    # Valores de etiquetas invertidos
    labels_new <- valores_inv(labels_x, labels_pos[1], labels_pos[2])

    # Agregar las etiquetas nuevas al nuevo vector, también invertido.
    x_new <- structure(x_new,
                       labels = labels_new)
  }

  # Devuelve nuevo vector invertido con etiqueta de variable y
  # etiqueta de niveles si las hubo.
  structure(x_new,
            label = attr(x, 'label', exact = TRUE),
            class = 'haven_labelled')
}


#' @rdname rev_niveles
#' @export
rev_niveles.numeric <- function(x, rango_inv = NULL, ...){

  x_new <- rev_niveles.haven_labelled(x, rango_inv = rango_inv, ...)

  # Cambio clase de haven_labelled a numeric por consistencia.
  unclass(x_new)
}
