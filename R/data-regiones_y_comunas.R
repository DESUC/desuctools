#' @title Datos de regiones y comunas de Chile
#'
#' @description
#' Tabla que contiene nombres y códigos de regiones y comunas de Chile,
#' además de algunas segmentación es utilizadas habitualmente
#'
#' @format Un data frame con 346 filas (comunas) y 11 variables
#'
#' \describe{
#'   \item{id}{orden de comunas siguiendo geografía y código comunal}
#'   \item{region_orden}{numeración de regiones de norte a sur}
#'   \item{comuna}{código de comuna}
#'   \item{region}{número de región}
#'   \item{region_nom}{nombre oficial de la región}
#'   \item{region_nom_small}{nombre breve de la región}
#'   \item{comuna_nom}{nombre de la comuna}
#'   \item{region_capital}{TRUE si comuna es capital regional}
#'   \item{comuna_extrema}{1 si comuna es extrema para INE, 2 si es extrema para DESUC}
#'   \item{gran_santiago}{TRUE si comuna pertenece al Gran Santiago}
#'   \item{gran_santiago_zonas}{nombre de zonas del Gran Santiago}
#' }
#'

"regiones_y_comunas"
