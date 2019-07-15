library(tidyverse)

df_test <- tibble::tibble(sexo = haven::labelled(c(1, 2, 2, 2),
                                                 labels = c('M' = 1, 'W' = 2),
                                                 label = 'Sex'),
                          cat    = c(1, 1, 2, 2),
                          cat_na = c(1, 1, 2, NA),
                          wt     = c(2, 1, 1, 0))



# tabla_categoria ---------------------------------------------------------

test_that("tabla_categoria proporcion de categoría labelled", {
  expect_identical(tabla_categorias(df_test, sexo) %>%
                      pull(prop),
                    c(0.25, 0.75))
})

test_that("tabla_categoria agrega en pregunta_lab la etiqueta de categoria", {
  expect_equal(tabla_categorias(df_test, sexo) %>%
                 pull(pregunta_lab) %>%
                 as.character(),
               c('Sex', 'Sex'))
})

test_that("tabla_categoria proporcion de categoría numerica", {
  expect_identical(tabla_categorias(df_test, cat) %>%
                     pull(prop),
                   c(0.5, 0.5))
})

test_that("tabla_categoria proporcion de categoría numerica con missing", {
  expect_identical(tabla_categorias(df_test, cat_na) %>%
                     pull(prop),
                   c(0.5, 0.25, 0.25))
})

test_that("tabla_categoria proporcion de categoría labelled con peso", {
  expect_equivalent(tabla_categorias(df_test, sexo, .wt = wt) %>%
                      pull(prop),
                    c(0.5, 0.5))
})

test_that("tabla_categoria proporcion de categoría numerica con missing con peso", {
  expect_equivalent(tabla_categorias(df_test, cat_na, .wt = wt) %>%
                      pull(prop),
                    c(0.75, 0.25, 0))
})


# tabla_vars_segmentos ----------------------------------------------------

test_that("tabla_categoria proporcion de categoría con peso y total", {
  expect_equivalent(tabla_vars_segmentos(df_test,
                                         .vars = vars(sexo),
                                         .segmentos = vars(cat),
                                         total = TRUE) %>%
                      pull(prop),
                    c(0.5, 0.5, 1, 0.25, 0.75))
})


#  rec_cat_5a3 ------------------------------------------------------------

test_that("rec_cat_5a3 mantiene la etiqueta de la variable", {
  expect_equivalent(attr(rec_cat_5a3(df_test$sexo), 'label'),
                    'Sex')
})
