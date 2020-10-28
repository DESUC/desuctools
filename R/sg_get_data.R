#' @title SurveyToGo rest API: Get individual data
#'
#' @description Función para bajar datos y respuestas considerando el límite de
#'       99 casos por envío. Es un envoltorio de la función `sg_get()`.
#'
#' @param api_operation `chr` Bajar datos según operaciones "SimpleExport"
#'        o "OperationData".
#' @param interview_ids `int` Vector con códigos de interviews de los que
#'        se quiera bajar información.
#' @param ... `list` pasa a query en sg_get
#' @inheritParams sg_get
#'
#' @seealso sg_get
#'
#' @return data.frame
#' @export
#'
#' @examples
sg_get_data <- function(api_operation = c('SimpleExport',
                                          'OperationData'),
                        interview_ids,
                        ...,
                        api_key, user, pass,
                        type = NULL) {

  if (missing(api_operation)) {
    api_operation <- "SimpleExport"
  }
  if (sum(api_operation %in% c("SimpleExport", "OperationData")) != length(api_operation)) {
    stop("\"api_operation\" must be one of: \"SimpleExport\" or \"OperationData\"")
  }

  query_additional <- list(...)

  # Dividir interviews. en grupos de 99 entrevistas.
  df_interviews <- data.frame(group = ceiling(seq_along(interview_ids)/99),
                              ids = as.integer(unlist(interview_ids)))

  # Colapsar el listado de ids a strings de un máximo de 99.
  df_interviews <- df_interviews %>%
    group_by(.data$group) %>%
    summarize(string = stringr::str_c(.data$ids, collapse = ','),
              .groups = 'drop')

  # Obtener la información de respuesta de cada grupo de interviews.
  df_data <- purrr::map(df_interviews$string,
                        ~desuctools::sg_get(api_operation = api_operation,
                                            query = append(list(subjectIDs = .),
                                                           query_additional),
                                            user = user,
                                            pass = pass,
                                            api_key = api_key,
                                            type = type))

  # Integrar todas las respuestas en una sola base
  if(api_operation == 'SimpleExport'){
    df_data <- df_data %>%
      purrr::modify_depth(.depth = 1, purrr::pluck, 'Subjects') %>%
      purrr::flatten()
  }

  return(df_data)
}
