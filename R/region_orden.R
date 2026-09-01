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
region_orden <- function(reg, as.factor = TRUE) {
  # Ordena número de regiones en un factor de norte a sur.
  reg_num <- c(15, 1:5, 13, 6, 7, 16, 8, 9, 14, 10:12)
  reg_nom <- c(
    "Arica y Parinacota",
    "Tarapac\u00e1",
    "Antofagasta",
    "Atacama",
    "Coquimbo",
    "Valpara\u00edso",
    "Metropolitana",
    "O'Higgins",
    "Maule",
    "\u00d1uble",
    "Biob\u00edo",
    "La Araucan\u00eda",
    "Los R\u00edos",
    "Los Lagos",
    "Ays\u00e9n",
    "Magallanes"
  )

  names(reg_num) <- reg_nom

  if (as.factor) {
    factor(reg, levels = reg_num, labels = names(reg_num))
  } else {
    haven::labelled(reg, labels = reg_num, label = "Regi\u00f3n")
  }
}
