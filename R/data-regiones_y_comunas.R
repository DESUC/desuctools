#' @title Datos de regiones y comunas de Chile
#'
#' @description
#' Tabla que contiene nombres y códigos de regiones y comunas de Chile,
#' además de algunas segmentación es utilizadas habitualmente
#'
#' @format Un data frame con 346 filas `comunas` y 19 variables
#'
#' \describe{
#'   \item{id}{orden de comunas siguiendo geografía y código comunal}
#'   \item{region_orden}{numeración de regiones de norte a sur}
#'   \item{comuna}{código de comuna}
#'   \item{comuna18}{código de comuna luego de la creación de la región de Ñuble en 2018}
#'   \item{region}{número de región}
#'   \item{provincia}{código de provincia luego de la creación de la región de Ñuble}
#'   \item{region_nom}{nombre oficial de la región}
#'   \item{region_nom_small}{nombre breve de la región}
#'   \item{comuna_nom}{nombre de la comuna}
#'   \item{region_capital}{TRUE si comuna es capital regional}
#'   \item{comuna_extrema}{1 si comuna es extrema para INE, 2 si es extrema para DESUC}
#'   \item{gran_santiago}{TRUE si comuna pertenece al Gran Santiago}
#'   \item{gran_santiago_zonas}{nombre de zonas del Gran Santiago}
#'   \item{educ_sup_com}{% hogares con algún miembro con educación superior}
#'   \item{hacinamiento}{% hogares con hacinamiento}
#'   \item{allegamiento_int}{% hogares con allegamiento interno}
#'   \item{carente_ser_bas}{% hogares carentes de servicios básicos}
#'   \item{carente_entorno}{% hogares carentes de características beneficiosas en el entorno}
#'   \item{decil}{promedio de decir de ingresos de los hogares}
#' }
#'

"regiones_y_comunas"
