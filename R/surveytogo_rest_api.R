#' @title SurveyToGo rest API
#'
#' @description Función para acceder a datos del sistema SurveyToGo utilizando
#'        su API REST.
#'
#' @param api_operation `chr`
#' @param query `list` listado de variables que serán pasadas a la api_operation
#'        que se elija.
#' @param api_key `chr` REST API Key entregada por dooblo.
#' @param user `chr` nombre de usuario.
#' @param pass `chr` password de usuario.
#' @param type `chr` Formato en el que se obtendrán datos.
#'        Por defecto 'application/json'. Puede ser 'text/xml' si se quiere xml.
#'
#' @references https://support.dooblo.net/hc/en-us/articles/208294645-How-To-Use-The-SurveyToGo-REST-API
#'
#' @return json
#'
#' @importFrom httr authenticate GET content add_headers
#'
#' @export
#'
sg_get <- function(api_operation,
                   query,
                   api_key, user, pass,
                   type = 'application/json'){
  # Referencia
  # https://support.dooblo.net/hc/en-us/articles/208294645-How-To-Use-The-SurveyToGo-REST-API

  # SurveyToGo REST API
  url_sg_api <- 'http://api.dooblo.net/'


  sg_auth <- httr::authenticate(user = paste0(api_key, '/', user),
                                password = pass,
                                type = "basic")

  db <- httr::GET(url = url_sg_api,
                  sg_auth,
                  httr::add_headers("Accept" = type,
                                    "Accept-Charset" = "utf-8"),
                  path = paste0('newapi/', api_operation),
                  query = query)

  # url usada en GET.
  print(db$url)

  httr::content(db,
                type = type)
}
