#' Title Función para recodificar variables etiquetadas (labelled)
#'
#' @param .vec: vector/variable a recodificar
#' @param l_niveles: lista con recodificaciones.
#'                   Debe cumplir un formato específico.
#'
#' @return vector labelled recodificado
#' @export rec_labelled
#'
#'
#' @examples
#' ls_rec <- list()
rec_labelled <- function(.vec, ls_niveles){

  # Funcion base para recodificacion
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
      `var_label<-`(var_label(.vec))
  }


  # Aplicacion de funcion base
  fct_rec <- recode_list(.vec, ls_niveles)


  # Recodificacion de etiquetas
  nivel_chr <- stringr::str_match(levels(fct_rec), '(\\d*)-(.*)')
  nivel_num <- nivel_chr[, 2]
  nivel_cod <- nivel_chr[, 3]

  fct_rec %>%
    forcats::fct_recode(!!!setNames(nivel_chr[,1],
                           nivel_num)) %>%
    as.character() %>%
    as.numeric() %>%
    labelled::labelled(labels = setNames(nivel_num %>% as.numeric(),
                                         nivel_cod),
                       label = var_label(.vec))

}


x <- labelled(
  c(1, 2, 2, 2, 3, 4, 5, 5, 1, NA),
  c("Muy de acuerdo" = 1,
    "De acuerdo" = 2,
    "Ni de acuerdo ni en desacuerdo" = 3,
    "En desacuerdo" = 4,
    "Muy en desacuerdo" = 5)
)

ls_rec <- list("1-Muy de acuerdo + De acuerdo" = 1:2,
               "2-Ni de acuerdo ni en desacuerdo" = 3,
               "3-En desacuerdo + Muy en desacuerdo" = 4:5,
               "99-NA" = NA)

rec_labelled(.vec = x,
             ls_niveles = ls_rec)
