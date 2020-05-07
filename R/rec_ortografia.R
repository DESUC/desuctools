#' Title Corrector ortográfico
#'
#' @description
#' Código idéntico al de Rasmus Bååth
#'
#' @param word chr Palabra a corregir
#' @param sorted_words Lista de palabras de referencia ordenadas por frecuencia.
#'    Por defecto usa lista de CRAE
#'
#' @return character
#'
#' @source \url{https://nubededatos.blogspot.com/2015/01/corrector-ortografico-en-espanol-para-r.html}
#'
#' @importFrom purrr map_chr
#' @export
#'
#' @examples
#'
#' rec_ortografia(c('pais', 'arbol'))
#'
rec_ortografia <- function(word,
                           sorted_words = desuctools::corpus_rae$Orden[1:5000]) {

  correct <- function(w){
    # Calcula la distancia entre la palabra y el resto de palabras ordenadas (sorted words).
    edit_dist <- adist(w, sorted_words)

    # Genera un vector con todas las palabras con el mínimo de distancia.
    # Como sorted_words está ordenada de más a menos común, el vector
    # resultante tendrá la primera coincidencia más común/probable.
    proposals_by_prob <- c(sorted_words[ edit_dist <= min(edit_dist, 2) ])

    # En caso de que proposals_by_prob esté vacío asignamos la palabra evaluada
    proposals_by_prob <- c(proposals_by_prob, w)

    # ... y devuelve la palabra primera/más probable en el vector.
    proposals_by_prob[1]
  }

  purrr::map_chr(word, correct)
}
