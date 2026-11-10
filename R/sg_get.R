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
#' @return `list` (json), `xml_document` (xml) o `data.frame` (csv), según el
#'        tipo de contenido de la respuesta. Si la consulta falla se lanza un
#'        error que indica la operación. Si la respuesta tiene un tipo de
#'        contenido no soportado se devuelve `NULL` con un `warning`.
#'
#' @import httr2
#' @importFrom utils read.csv
#'
#' @export
#'
sg_get <- function(
  api_operation,
  query,
  api_key,
  user,
  pass,
  type = c("application/json", "text/xml", "text/csv")
) {
  if (
    missing(type) &&
      !missing(api_operation)
  ) {
    type <- "application/json"
  }
  if (
    sum(type %in% c("application/json", "text/xml", "text/csv")) != length(type)
  ) {
    stop(
      "\"type\" must be one of: \"application/json\" or \"text/xml\" or \"text/csv\""
    )
  }

  # Referencia
  # https://support.dooblo.net/hc/en-us/articles/208294645-How-To-Use-The-SurveyToGo-REST-API

  # SurveyToGo REST API
  url_sg_api <- "https://api.dooblo.net/"

  req_stg <- httr2::request(url_sg_api) |>
    httr2::req_headers(
      "Accept-Charset" = "utf-8",
      "Connection" = "keep-alive",
      "Accept" = type
    ) |>
    httr2::req_url_path(path = paste0("newapi/", api_operation)) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_auth_basic(
      username = paste0(api_key, "/", user),
      password = pass
    ) |>
    httr2::req_throttle(rate = 2) |> # max 2 requests por segundo
    httr2::req_retry(max_tries = 5) # max 5 intentos

  # print(req_stg)

  # Estados de la consulta: error_http | respuesta con tipo conocido |
  # respuesta con tipo no soportado.

  # Estado: error_http. Se informa la operación que falló y el error original.
  response <- tryCatch(
    httr2::req_perform(req_stg),
    error = function(e) {
      stop(
        "Error: fall\u00f3 la consulta a SurveyToGo '",
        api_operation,
        "': ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  content_type <- httr2::resp_content_type(response)

  switch(
    content_type,
    "application/json" = response |>
      httr2::resp_body_json(),
    "application/octet-stream" = response |>
      httr2::resp_body_json(check_type = FALSE),
    "text/xml" = response |>
      httr2::resp_body_xml(),
    "text/csv" = response |>
      httr2::resp_body_string() |>
      textConnection() |>
      utils::read.csv(),
    # Estado: tipo no soportado. Se mantiene el retorno NULL, pero se avisa.
    {
      warning(
        "SurveyToGo '",
        api_operation,
        "' devolvi\u00f3 un tipo de contenido no soportado ('",
        content_type,
        "'); se devuelve NULL.",
        call. = FALSE
      )
      invisible(NULL)
    }
  )
}
