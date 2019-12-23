
#' @title Codigos de disposición final según AAPOR
#'
#' @description
#' Recodificación de códigos de disposición final de casos según AAPOR.
#'
#' Tabla de cógidos a partir de Casen 2015
#'
#' val                                                      label
#' 110                                        Entrevista completa
#' 120                                         Entrevista parcial
#' 211                                   Se rechazó la entrevista
#' 212                               Se interrumpió la entrevista
#' 223                            Se impidió acceso a la vivienda
#' 224                   Vivienda ocupada sin moradores presentes
#' 225                  Informante no ubicable o no puede atender
#' 231                                      Muerte del informante
#' 232           Informante impedido físico/mental para contestar
#' 233                                         Problema de idioma
#' 236                                        Otra razón elegible
#' 311                                      No se envió a terreno
#' 317                         Area peligrosa o de difícil acceso
#' 318                      No fue posible localizar la dirección
#' 390                     Otra razón de elegibilidad desconocida
#' 410                                           Fuera de muestra
#' 451            Empresa, oficina de gobierno u otra institución
#' 452     Institución (Hospital, cárcel, asilo de ancianos, etc)
#' 453      Dormitorio colectivo (Militar, de trabajo, internado)
#' 454 Vivienda en demolición, incendiada, destruida o erradicada
#' 461                             Vivienda particular desocupada
#' 462                      Vivienda de veraneo o de uso temporal
#' 463                                     Otra razón no elegible
#'
#' @param codigo integer. Código de disposición final de casos
#'
#' @return factor con cófigos agrupados.
#' @export
#'
#' @examples rec_cdf(c(110, 213))
rec_cdf <- function(codigo){
  cdf <- dplyr::case_when(codigo %/% 10  == 11 ~ 11L,
                          codigo %/% 10  == 12 ~ 12L,
                          codigo %/% 10  == 21 ~ 21L,
                          codigo %/% 10  == 22 ~ 22L,
                          codigo %/% 10  == 23 ~ 23L,
                          codigo %/% 10  == 31 ~ 31L,
                          codigo %/% 10  == 39 ~ 39L,
                          codigo %/% 100 ==  4 ~ 40L)


  haven::labelled(cdf,
                  labels = c("I - Entrevistas Completas"            = 11L,
                             "P - Entrevistas Parciales"            = 12L,
                             "R - Rechazos"                         = 21L,
                             "NC - No Contacto"                     = 22L,
                             "O - Otros, No Responde"               = 23L,
                             "UH - Elegibilidad Desconocida"        = 31L,
                             "OH - Elegibilidad Desconocida, Otros" = 39L,
                             "NE - No Elegible"                     = 40L))
}
