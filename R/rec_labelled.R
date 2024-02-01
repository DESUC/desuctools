#' @title Recodificación de variables labelled
#'
#' @description
#' Recodificación de un vector labelled_haven utilizando una lista de etiquetas y niveles.
#'
#' @param .vec `labelled_haven`: vector/variable a recodificar
#' @param ls_niveles `list`: lista con recodificaciones.
#'                   Debe cumplir un formato específico.
#'
#' @return vector labelled_haven
#'
#' @importFrom labelled labelled var_label `var_label<-`
#' @importFrom purrr map_chr
#' @importFrom forcats fct_recode
#' @importFrom stringr str_match
#'
#' @examples
#' # vector etiquetado
#' v_labelled <- labelled::labelled(
#'   c(1, 2, 2, 2, 3, 4, 5, 5, 1, NA),
#'   labels = c("Muy de acuerdo" = 1,
#'              "De acuerdo" = 2,
#'              "Ni de acuerdo ni en desacuerdo" = 3,
#'              "En desacuerdo" = 4,
#'              "Muy en desacuerdo" = 5)
#' )
#'
#' # Definir la lista de recodificaciones
#' # IMPORTANTE: el formato es "val_final-Etiqueta = val_inicial"
#' # Donde:
#' #    val_final: serán los valores finales de las etiquetas.
#' #    Etiqueta: etiquetas de los valores anteriormente definidos.
#' #    val_inicial: categorías que se consideran para la recodificación (numéricos)
#'
#' ls_rec <- list(
#'   "1-Muy de acuerdo + De acuerdo" = 1:2,
#'   "2-Ni de acuerdo ni en desacuerdo" = 3,
#'   "3-En desacuerdo + Muy en desacuerdo" = 4:5,
#'   "99-NA" = NA
#' )
#'
#' # Recodificación de v_labelled según niveles de ls_rec
#' rec_labelled(.vec = v_labelled,
#'              ls_niveles = ls_rec)
#'
#' @export
#'
rec_labelled <- function(.vec, ls_niveles){

  # Función base para codificación
  recode_list <- function(.vec,
                          ls_niveles) {

    vec_chr <- purrr::map_chr(.vec,
                              \(val){
                                for (category in names(ls_niveles)) {
                                  range <- ls_niveles[[category]]
                                  if (val %in% range) {
                                    return(category)
                                  }
                                }
                              }
    )
    vec_fct <- base::factor(vec_chr,
                            levels = c(names(ls_niveles)))

    vec_fct |>
      labelled::`var_label<-`(labelled::var_label(.vec))
  }

  # Aplicación de función base
  fct_rec <- recode_list(.vec, ls_niveles)

  # Codificación de etiquetas
  nivel_chr <- stringr::str_match(levels(fct_rec), '(\\d*)-(.*)')
  nivel_num <- nivel_chr[, 2]
  nivel_cod <- nivel_chr[, 3]

  fct_rec |>
    forcats::fct_recode(!!!setNames(nivel_chr[,1],
                           nivel_num)) |>
    as.character() |>
    as.numeric() |>
    labelled::labelled(labels = setNames(as.numeric(nivel_num),
                                         nivel_cod),
                       label = var_label(.vec))

}
