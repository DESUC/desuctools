#' Factor case_when
#'
#' Wrapper around \code{dplyr::case_when} that converts the output to a factor and
#' preserves the order in which value labels were passed into the function.
#'
#' @param ... A sequence of two-sided formulas consistent with \code{dplyr::case_when}.
#' @param label A character. It's the label of the created value.
#' @return The output of \code{dplyr::case_when}, as class \code{"factor"} and ordered
#' however you wanted it.
#' @details Unlike case_when, fct_case_when allows factors to be passed in as
#' right-hand-side arguments - they are treated internally as characters, but the
#' resulting vector will preserve the order of the original factor levels.
#' @author pewmethods
#'
#' @examples
#' library(dplyr)
#' macrozona <- with(regiones_y_comunas,
#'                   fct_case_when(region %in% c(15, 1:4) ~ "Norte",
#'                                 region %in% c(5:7, 16) ~ "Centro",
#'                                 region %in% c(13) ~ "RM",
#'                                 region %in% c(8:12, 14) ~ "Sur")
#' )
#'
#' # Compare to normal case_when() and then factor(), which will arrange the levels in
#' # unwanted alphabetical order
#'
#' macrozona <- with(regiones_y_comunas,
#'                   factor(case_when(region %in% c(15, 1:4) ~ "Norte",
#'                                    region %in% c(5:7, 16) ~ "Centro",
#'                                    region %in% c(13) ~ "RM",
#'                                    region %in% c(8:12, 14) ~ "Sur"))
#' )
#'
#' @import rlang
#' @export
fct_case_when <- function(..., label = NULL)
{
  default_env <- caller_env()
  arguments <- list2(...)
  arguments <- Filter(function(elt) !is.null(elt), arguments)
  arg_len <- length(arguments)
  output_levels <- purrr::map(arguments,
                              function(a) {
                                out <- f_rhs(a) %>% eval_tidy(env = default_env)
                                return(levels(out) %||% out)
                              }) %>%
    purrr::list_c()
  for (i in 1:arg_len) {
    f_rhs(arguments[[i]]) <- as.character(f_rhs(arguments[[i]]) %>%
                                            eval_tidy(env = default_env))
  }
  cw <- do.call(case_when, arguments)
  cw <- factor(cw, levels = unique(output_levels))

  attr(cw, 'label') <- label

  return(cw)
}
