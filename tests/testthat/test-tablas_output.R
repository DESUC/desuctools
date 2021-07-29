df_test <- tibble::tibble(sexo = haven::labelled(c(1, 2, 2, 2),
                                                 labels = c('M' = 1, 'W' = 2),
                                                 label = 'Sex'),
                          edad = haven::labelled(c(1, 1, 2, 2),
                                                 labels = c('young' = 1, 'old' = 2),
                                                 label = 'Age'),
                          c_int  = c(1, 1, 1, 1),
                          cat    = c(1, 1, 2, 2),
                          cat_na = c(1, 1, 2, NA),
                          wt     = c(2, 1, 1, 0))

# tabla_categoria ---------------------------------------------------------

test_that("tabla_categoria proporcion de categoría labelled", {
  expect_identical(tabla_categorias(df_test, sexo)[['prop']],
                   c(0.25, 0.75))
})

test_that("tabla_categoria proporcion de dos variables labelled", {
  expect_identical(tabla_categorias(df_test, sexo, edad)[['prop']],
                   c(0.25, 0.75, 0.50, 0.50))
})

test_that("tabla_categoria agrega en pregunta_lab la etiqueta de categoria", {
  expect_equal(tabla_categorias(df_test, sexo)[['pregunta_lab']] %>%
                 as.character(),
               c('Sex', 'Sex'))
})

test_that("tabla_categoria proporcion de categoría numerica", {
  expect_identical(tabla_categorias(df_test, cat)[['prop']],
                   c(0.5, 0.5))
})

test_that("tabla_categoria proporcion de categoría numerica con missing", {
  expect_identical(tabla_categorias(df_test, cat_na)[['prop']],
                   c(0.5, 0.25, 0.25))
})

test_that("tabla_categoria proporcion de categoría labelled con peso", {
  expect_equal(tabla_categorias(df_test, sexo, .wt = wt)[['prop']],
               c(0.5, 0.5))
})

test_that("tabla_categoria proporcion de categoría numerica con missing con peso", {
  expect_equal(tabla_categorias(df_test, cat_na, .wt = wt)[['prop']],
               c(0.75, 0.25, 0))
})


# tabla_vars_segmentos ----------------------------------------------------

test_that("tabla_vars_segmentos proporcion de categoría y total", {
  expect_equal(tabla_vars_segmentos(df_test,
                                    .vars = vars(sexo),
                                    .segmentos = vars(cat),
                                    total = TRUE)[['prop']],
               c(0.5, 0.5, 1, 0.25, 0.75))
})

test_that("tabla_categoria proporcion de categoría y total y missing factor", {
  expect_equal(tabla_vars_segmentos(df_test,
                                    .vars = vars(sexo),
                                    .segmentos = vars(cat),
                                    miss = 'M',
                                    total = TRUE)[['prop_val']],
               c(NA, 1, 1, NA, 1))
})

test_that("tabla_categoria proporcion de categoría y total y missing numérico", {
  expect_equal(tabla_vars_segmentos(df_test,
                                    .vars = vars(cat_na),
                                    .segmentos = vars(sexo),
                                    miss = c(2, NA),
                                    total = FALSE)[['prop_val']],
               c(1, 1, NA, NA))
})

test_that("tabla_categoria proporcion de categoría con segmento constante", {
  expect_equal(tabla_vars_segmentos(df_test,
                                    .vars = vars(cat_na),
                                    .segmentos = vars(NULL),
                                    miss = c(2, NA),
                                    total = FALSE)[['prop_val']],
               c(1, 1, NA, NA))
})

#  rec_cat_5a3 ------------------------------------------------------------

test_that("rec_cat_5a3 mantiene la etiqueta de la variable", {
  expect_equal(attr(rec_cat_5a3(df_test$sexo), 'label', exact = TRUE),
               'Sex')
})
