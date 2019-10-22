#' @title Calculo NSE
#'
#' Funciones de trabajo con datos ------------------------------------------
#'
#' Cálculo de Nivel socioeconómico según formula DESUC utilizando
#' ocupación, educación y bienes.
#'
#' Nivel de educación más alto que alcanzó
#' val                                                          label
#' 1                                                     No estudió
#' 2                     Educación básica o preparatoria incompleta
#' 3                       Educación básica o preparatoria completa
#' 4                       Educación media o humanidades incompleta
#' 5                         Educación media o humanidades completa
#' 6 Instituto Profesional o Centro de formación Técnica Incompleta
#' 7   Instituto Profesional o Centro de Formación Técnica Completa
#' 8                                       Universitaria incompleta
#' 9                                         Universitaria completa
#' 10                   Post grado (máster, doctorado o equivalente)
#' 88                                                        No sabe
#' 99                                                    No responde
#'
#'
#' ¿Cuál de las siguientes ocupaciones corresponde al trabajo del principal sostenedor del hogar?
#' Si el principal sostenedor del hogar esta cesante o es jubilado, preguntar por la ultima ocupacion
#' remunerada que tuvo. Si el principal sostenedor tiene más de 1 trabajo, debe registrarse el de mayor ingreso.
#'
#' val Categoría
#' 1. Trabajadores no calificados en ventas y servicios, peones agropecuarios, forestales, construcción, etc.
#' 2. Obreros, operarios y artesanos de artes mecánicas y de otros oficios.
#' 3. Trabajadores de los servicios y vendedores de comercio y mercados.
#' 4. Agricultores y trabajadores calificados agropecuarios y pesqueros.
#' 5. Operadores de instalaciones y máquinas y montadores / conductores de vehículos.
#' 6. Empleados de oficina públicos y privados.
#' 7. Técnicos y profesionales de nivel medio (incluye hasta suboficiales FFAA y Carabineros).
#' 8. Profesionales, científicos e intelectuales.
#' 9. Alto ejecutivo (gerente general o gerente de área o sector) de empresa privadas o pública.
#'    Director o dueño de grandes empresas.
#'    Alto directivo del poder ejecutivo, de los cuerpos legislativos y la administración pública
#'    (incluye oficiales de FFAA y Carabineros).
#' 10. Otros grupos no identificados (incluye rentistas, incapacitados, etc.)
#'
#'
#' @param .data data.frame Base de datos
#' @param .ocupacion_jh name nombre de variable para educación de jefe de hogar
#' @param .educacion_jh name nombre de variable para educación de jefe de hogar
#' @param .bienes_var tidyselect selector de variables para bienes
#' @param append logic TRUE para agregar la variable a la base original
#'
#' @name calculo_nse
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#' @export
calculo_nse <- function(.data,
                        .ocupacion_jh,
                        .educacion_jh,
                        .bienes_var = NULL,
                        append = TRUE) {
    # Cálculo de nivel socioeconómico según fórmula DESUC

    ocup_jh <- enquo(.ocupacion_jh)
    educ_jh <- enquo(.educacion_jh)

    ocup_jh <- .data[[rlang::as_label(ocup_jh)]] %>% as.integer()
    educ_jh <- .data[[rlang::as_label(educ_jh)]] %>% as.integer()

    stopifnot(all(unique(ocup_jh) %in% c(1:16, 99)))

    # Recodificación de ocupacion
    ocup_jh <- dplyr::case_when(ocup_jh %in% 1:2 ~ 1L,
                                ocup_jh %in% 3:4 ~ 2L,
                                ocup_jh %in% 5:6 ~ 3L,
                                ocup_jh %in% 7:8 ~ ocup_jh,
                                TRUE ~ NA_integer_)

    # Tabla con relación educación y ocupación y nivel socioeconómico
    ocu_y_edu_a_nse <- tibble::tibble(
        edu = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8,
                8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10),
        ocu = c(1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 1,
                2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6),
        nse = c(1, 1, 2, 3, 3, 4, 1, 1, 2, 3, 3, 4, 1, 2, 2, 3, 3, 4, 2, 2, 2,
                3, 4, 4, 2, 2, 3, 3, 4, 5, 2, 2, 3, 3, 4, 5, 3, 3, 4, 4, 4, 5, 3, 3, 4, 4, 4, 5, 3, 3, 4, 4, 5, 5, 3, 3, 4, 5, 5, 5))

    tab_ocu_y_edu_a_nse <- ocu_y_edu_a_nse %>%
        transmute(index = .data$edu * 10 + .data$ocu,
                  nse = as.integer(.data$nse)) %>%
        tibble::deframe()

    # NSE educación y ocupación
    nse_edu_ocu <- tab_ocu_y_edu_a_nse[as.character(educ_jh * 10 + ocup_jh)]

    # NSE Bienes
    if (!is.null(.bienes_var)) {
        rec_bienes <- function(.data, val_valido = 1, ns_nr = c(8, 9)) {
            dplyr::case_when(.data %in% val_valido ~ TRUE,
                             .data %in% ns_nr      ~ NA,
                             TRUE ~ FALSE)
        }

        bienes_sum <- .data %>%
            transmute_at(.vars = .bienes_var,
                         .funs = rec_bienes, val_valido = 1) %>%
            sjmisc::row_sums(n = 0.9, append = FALSE) %>% pull()

        nse_bienes <- dplyr::case_when(bienes_sum == 0 ~ 1L,
                                       bienes_sum <= 3 ~ 2L,
                                       bienes_sum <= 6 ~ 3L,
                                       bienes_sum <= 8 ~ 4L,
                                       bienes_sum <= 10 ~ 5L)
    } else {
        nse_bienes = NA_integer_
    }

    # NSE sólo educación
    nse_educacion <- dplyr::case_when(educ_jh <= 3 ~ 1L,
                                      educ_jh <= 6 ~ 2L,
                                      educ_jh <= 8 ~ 3L,
                                      educ_jh == 9 ~ 4L,
                                      educ_jh == 10 ~ 5L)

    # Construcción de NSE
    df_nse <- tibble::tibble(nse_edu_ocu   = nse_edu_ocu,
                             nse_bienes    = nse_bienes,
                             nse_educacion = nse_educacion) %>%
        mutate(NSE = dplyr::case_when(is.na(nse_edu_ocu) & is.na(nse_bienes) ~ nse_educacion,
                                      is.na(nse_edu_ocu) ~ nse_bienes,
                                      TRUE ~ nse_edu_ocu))

    # Etiquetas y recodificaciones finales
    df_nse <- df_nse %>%
        mutate(NSE = haven::labelled(.data$NSE,
                                     labels = c(E = 1, D = 2, C3 = 3, C2 = 4, ABC1 = 5),
                                     label = "Grupo socioecon\u00f3mico"),
               tnse = sjmisc::rec(.data$NSE, rec = "1:2 = 1 [Bajo]; 3 = 2 [Medio]; 4:5 = 3 [Alto]"),
               nse4 = sjmisc::rec(.data$NSE, rec = "1:2 = 1 [D];    3 = 2 [C3];      4 = 3 [C2]; 5 = 4 [Alto]")) %>%
        sjlabelled::var_labels(tnse = "Nivel socioecon\u00f3mico",
                               nse4 = "NSE con cuatro niveles") %>%
        mutate_all(sjlabelled::as_labelled)

    if (append) {
        bind_cols(.data, df_nse)
    } else {
        df_nse
    }
}
