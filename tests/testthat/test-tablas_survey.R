library(dplyr)
library(srvyr)
library(desuctools)

d <- data.frame(x      = factor(c(rep(letters[1:3], 6), letters[1:2]), levels = letters[1:4]),
                fct    = factor(         c(rep(1, 12), rep(0, 8)), labels = c('No', 'Si')),
                lab    = haven::labelled(c(rep(1, 12), rep(0, 8)), labels = c('Si' = 1, 'No' = 0)),
                lab_na = haven::labelled(c(rep(1, 12), rep(0, 7), NA), labels = c('Sí' = 1, 'No' = 0, 'NS/NR' = 9)),
                chr    = c(rep('Si', 12), rep('No', 7), NA_character_),
                num    = c(rep(1, 12), rep(0, 8)),
                esc    = 1:20,
                esc_na = c(NA_integer_, 2:20),
                wgt    = c(rep(.5, 10), rep(1.5, 10))
)

s <- srvyr::as_survey_desig(d, weights = wgt)

# svy_tabla_var_segmento ---------------------------------------------------------

# Resultados sin segmentos

test_that("svy_tabla_var_segmento proporcion de categoría factor", {
  expect_identical(svy_tabla_var_segmento(s, .var = fct)$pregunta_cat, factor(c('No', 'Si')))
})

test_that("svy_tabla_var_segmento proporcion de categoría labelled", {
  expect_identical(svy_tabla_var_segmento(s, .var = lab)$pregunta_cat, factor(c('No', 'Si')))
})

test_that("svy_tabla_var_segmento mismos resultados entre factor y labelled", {
  expect_identical(svy_tabla_var_segmento(s, .var = fct)$prop,
                   svy_tabla_var_segmento(s, .var = lab)$prop)
})

test_that("svy_tabla_var_segmento media de escalar labelled", {
  expect_s3_class(svy_tabla_var_segmento(s, .var = esc), 'tbl_df')
})

test_that("svy_tabla_var_segmento proporcion de categoría labelled", {
  expect_s3_class(svy_tabla_var_segmento(s, .var = lab_na), 'tbl_df')
})

# Resultados con segmentos

test_that("svy_tabla_var_segmento proporcion de categoría factor con segmento", {
  expect_s3_class(svy_tabla_var_segmento(s, .var = fct, .segmento = x), 'tbl_df')
})

test_that("svy_tabla_var_segmento proporcion de categoría labelled con segmento", {
  expect_s3_class(svy_tabla_var_segmento(s, .var = lab, .segmento = x), 'tbl_df')
})

test_that("svy_tabla_var_segmento mismos resultados entre factor y labelled", {
  expect_identical(svy_tabla_var_segmento(s, .var = fct, .segmento = x)$prop,
                   svy_tabla_var_segmento(s, .var = lab, .segmento = x)$prop)
})

test_that("svy_tabla_var_segmento proporcion labelled con segmento", {
  expect_s3_class(svy_tabla_var_segmento(s, .var = esc, .segmento = x), 'tbl_df')
})

test_that("svy_tabla_var_segmento media escalar con segmento", {
  expect_s3_class(svy_tabla_var_segmento(s, .var = esc, .segmento = lab_na), 'tbl_df')
})

test_that("svy_tabla_var_segmento proporcion de categoría labelled", {
  expect_s3_class(svy_tabla_var_segmento(s, .var = lab_na, .segmento = x), 'tbl_df')
})

# svy_tabla_vars_segmentos ---------------------------------------------------------

# Resultados sin segmentos

test_that("svy_tabla_vars_segmento proporcion de categoría factor", {
  expect_identical(svy_tabla_vars_segmentos(s, .vars = fct)$pregunta_cat,
                   factor(c('No', 'Si')))
})

test_that("svy_tabla_vars_segmentos proporcion labelled con segmento", {
  expect_s3_class(svy_tabla_vars_segmentos(s, .vars = esc, .segmentos = c(x)),
                  'tbl_df')
})

test_that("svy_tabla_vars_segmentos proporcion labelled con segmento", {
  expect_s3_class(svy_tabla_vars_segmentos(s, .vars = esc, .segmentos = c(x, fct))$segmento_cat,
                  'factor')
})
