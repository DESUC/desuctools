#' @title SurveyToGo rest API
#'
#' @description Función para acceder a datos del sistema SurveyToGo utilizando
#'        su API REST.
#'
#' @param api_operation `chr` Nombre de alguno de las operaciones GET disponibles
#'        en la API de SurveyToGo. Ver link en referencias.
#' @param query `list` listado de variables que serán pasadas a la api_operation
#'        que se elija.
#' @param api_key `chr` REST API Key entregada por dooblo.
#' @param user `chr` nombre de usuario.
#' @param pass `chr` password de usuario.
#' @param type `chr` Formato en el que se obtendrán datos.
#'        Por defecto 'application/json'. Puede ser 'text/xml' si se quiere xml o
#'        'text/csv' si se desea un csv.
#'
#' @references https://support.dooblo.net/hc/en-us/articles/208294645-How-To-Use-The-SurveyToGo-REST-API
#'
#' @return json
#'
#' @import httr2
#' @importFrom utils read.csv
#'
#' @export
#'
sg_get <- function(api_operation,
                   query,
                   api_key, user, pass,
                   type = c('application/json',
                            'text/xml',
                            'text/csv')){

  if (missing(type) &&
      !missing(api_operation)) {
    type <- "application/json"
  }
  if (sum(type %in% c("application/json", "text/xml", "text/csv")) != length(type)) {
    stop("\"type\" must be one of: \"application/json\" or \"text/xml\" or \"text/csv\"")
  }

  # Referencia
  # https://support.dooblo.net/hc/en-us/articles/208294645-How-To-Use-The-SurveyToGo-REST-API

  # SurveyToGo REST API
  url_sg_api <- 'https://api.dooblo.net/'

  req_stg <- httr2::request(url_sg_api) |>
    httr2::req_headers("Accept-Charset" = "utf-8",
                       "Connection" = "keep-alive",
                       "Accept" = type) |>
    httr2::req_url_path(path = paste0('newapi/',
                                      api_operation)) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_auth_basic(
      username = paste0(api_key, '/', user),
      password = pass) |>
    httr2::req_throttle(rate = 2) |>  # max 2 requests por segundo
    httr2::req_retry(max_tries = 5)   # max 5 intentos

  print(req_stg)

  # Perform request:
  response <- try( # to catch errors
    {
      req_stg |>
        httr2::req_perform()
    }, silent = FALSE)

  if(is.null(response)){
    # Si es error, devuelve la respuesta con el mensaje de error.
    return(response)
  }

  switch(httr2::resp_content_type(response),
         'application/json'         = response |>
           httr2::resp_body_json(),
         'application/octet-stream' = response |>
           httr2::resp_body_json(check_type = FALSE),
         'text/xml'                 = response |>
           httr2::resp_body_xml(),
         'text/csv'                 = response |>
           httr2::resp_body_string() |>
           textConnection() |> utils::read.csv()
  )
}
