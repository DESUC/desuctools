#' Funciones de trabajo con datos ------------------------------------------
#'
#' Cálculo de Nivel socioeconómico según formula DESUC utilizando
#' ocupación, educación y bienes.
#'
#' @name calculo_nse
#'
#' @import dplyr
#'
#' @export
calculo_nse <- function(.data, .ocupacion_jh, .educacion_jh, .bienes_var = NULL, append = TRUE) {
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
        transmute(index = edu * 10 + ocu, nse = as.integer(nse)) %>%
        tibble::deframe()

    # NSE educación y ocupación
    nse_edu_ocu <- tab_ocu_y_edu_a_nse[as.character(educ_jh * 10 + ocup_jh)]

    # NSE Bienes
    if (!is.null(.bienes_var)) {
        rec_bienes <- function(.data, val_valido = 1, ns_nr = c(8, 9)) {
            dplyr::case_when(.data %in% val_valido ~ TRUE,
                      .data %in% ns_nr ~ NA, TRUE ~ FALSE)
        }

        bienes_sum <- .data %>% transmute_at(.vars = .bienes_var, .funs = rec_bienes, val_valido = 1) %>% sjmisc::row_sums(n = 0.9, append = FALSE) %>% pull()

        nse_bienes <- dplyr::case_when(bienes_sum == 0 ~ 1L,
                                       bienes_sum <= 3 ~ 2L,
                                       bienes_sum <= 6 ~ 3L,
                                       bienes_sum <= 8 ~ 4L,
                                       bienes_sum <= 10 ~ 5L)
    } else {
        nse_bienes = NA_integer_
    }

    # NSE sólo educación
    nse_educacion <- dplyr::case_when(educ_jh <= 3 ~ 1L, educ_jh <= 6 ~ 2L, educ_jh <= 8 ~ 3L, educ_jh == 9 ~ 4L, educ_jh == 10 ~ 5L)

    # Construcción de NSE
    df_nse <- tibble::tibble(nse_edu_ocu   = nse_edu_ocu,
                             nse_bienes    = nse_bienes,
                             nse_educacion = nse_educacion) %>%
        mutate(NSE = dplyr::case_when(is.na(nse_edu_ocu) & is.na(nse_bienes) ~ nse_educacion,
                               is.na(nse_edu_ocu) ~ nse_bienes,
                               TRUE ~ nse_edu_ocu))

    # Etiquetas y recodificaciones finales
    df_nse <- df_nse %>% mutate(NSE = haven::labelled(NSE,
                                                      labels = c(E = 1, D = 2, C3 = 3, C2 = 4, ABC1 = 5),
                                                      label = "Grupo socioecon\u00f3mico"),
                                tnse = sjmisc::rec(NSE, rec = "1:2 = 1 [Bajo];
                                                               3   = 2 [Medio];
                                                               4:5 = 3 [Alto]"),
                                nse4 = sjmisc::rec(NSE, rec = "1:2 = 1 [D];
                                                               3   = 2 [C3];
                                                               4   = 3 [C2];
                                                               5   = 4 [Alto]")) %>%
        sjlabelled::var_labels(tnse = "Nivel socioecon\u00f3mico", nse4 = "NSE con cuatro niveles") %>%
        mutate_all(sjlabelled::as_labelled)

    if (append) {
        bind_cols(.data, df_nse)
    } else {
        df_nse
    }
}
