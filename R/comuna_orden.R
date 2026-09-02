#' @title Orden de comuna
#'
#' @description Convierte un vector de CUT comunal (numérico o character, con o sin
#' ceros a la izquierda) al nombre de la comuna, ordenado geográficamente de norte a
#' sur según el orden de filas de [regiones_y_comunas].
#'
#' @param com `num` o `chr`. CUT comunal, en formato numérico (p. ej. `1101`) o
#'        character (p. ej. `"01101"`).
#' @param as.factor `logical`. Por defecto TRUE para crear factor ordenado geográficamente.
#'        FALSE crea variable de clase `labelled`.
#'
#' @return Si `as.factor = TRUE`, un factor con los niveles de comuna ordenados de
#' norte a sur (según [regiones_y_comunas]). Si `as.factor = FALSE`, un vector de
#' clase `labelled` con el CUT comunal y las etiquetas de nombre de comuna. Si algún
#' valor de `com` (no `NA`) no corresponde a ninguna comuna conocida, se emite un
#' `warning` y ese valor queda como `NA` en el resultado.
#'
#' @importFrom haven labelled
#' @export
#'
#' @examples
#' comuna_orden(c(1101, 5101, 13101))
#' comuna_orden(c("01101", "05101", "13101"))
#'
comuna_orden <- function(com, as.factor = TRUE) {
  # Acepta CUT comunal en formato character (con ceros a la izquierda) o numérico.
  com <- as.numeric(as.character(com))

  # `regiones_y_comunas` ya está ordenada de norte a sur; se usa ese orden de fila
  # para construir los niveles/labels sin reordenar manualmente.
  com_num <- desuctools::regiones_y_comunas$comuna18
  com_nom <- desuctools::regiones_y_comunas$comuna_nom

  # Avisa si algún CUT no coincide con ninguna comuna conocida (y por tanto
  # se convertirá en NA), para no ocultar errores de tipeo o CUT inválidos.
  no_reconocidos <- !is.na(com) & !(com %in% com_num)
  if (any(no_reconocidos)) {
    warning(
      "CUT comunal no reconocido, se convertir\u00e1(n) a NA: ",
      paste(unique(com[no_reconocidos]), collapse = ", "),
      call. = FALSE
    )
  }

  if (as.factor) {
    factor(com, levels = com_num, labels = com_nom)
  } else {
    haven::labelled(
      com,
      labels = stats::setNames(com_num, com_nom),
      label = "Comuna"
    )
  }
}
