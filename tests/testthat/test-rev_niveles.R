var_labelled <- structure(c(1, 2, 2, 3, 9),
                          labels = c('a' = 1, 'b' = 2, 'c' = 3, 'nr' = 9),
                          label = 'this is labelled',
                          class = 'haven_labelled')

test_that("rev_niveles haven_labelled", {

  # Valores sin rango
  expect_equal(as.numeric(rev_niveles(var_labelled, rango_inv = NULL)), c(9, 8, 8, 7, 1))
  # Etiquetas sin rango
  expect_equal(attr(rev_niveles(var_labelled, rango_inv = NULL), 'labels'), c('a' = 9, 'b' = 8, 'c' = 7, 'nr' = 1))

  # Valores con rango
  expect_equal(as.numeric(rev_niveles(var_labelled, rango_inv = c(1, 2))), c(2, 1, 1, 3, 9))
  # Etiquetas con rango
  expect_equal(attr(rev_niveles(var_labelled, rango_inv = c(1, 2)), 'labels'), c('a' = 2, 'b' = 1, 'c' = 3, 'nr' = 9))

  # Igualdad entre rango == NULL y todo el rango
  expect_equal(rev_niveles(var_labelled, rango_inv = NULL),
               rev_niveles(var_labelled, rango_inv = c(1, 9)))
})

test_that("rev_niveles factor", {
  var_factor <- factor(c('a', 'b', 'b', 'c', 'nr'))

  expect_equal(levels(rev_niveles(var_factor, niveles_inv = c('a', 'b'))), c('b', 'a', 'c', 'nr'))

  expect_equal(levels(rev_niveles(var_factor)), c('nr', 'c', 'b', 'a'))

  # Igualdad entre rango == NULL y todo el rango
  expect_equal(rev_niveles(var_factor, niveles_inv = NULL),
               rev_niveles(var_factor, niveles_inv = levels(var_factor)))
})

test_that("rev_niveles numeric", {
  var_numeric <- c(1, 2, 2, 9)

  class(var_labelled) <- NULL

  expect_equal(rev_niveles(var_numeric, rango_inv = NULL), c(9, 8, 8, 1))

  expect_equal(rev_niveles(var_numeric, rango_inv = c(1, 2)), c(2, 1, 1, 9))

  expect_equal(as.numeric(rev_niveles(var_labelled, rango_inv = c(1, 2))), c(2, 1, 1, 3, 9))

  # Igualdad entre rango == NULL y todo el rango
  expect_equal(rev_niveles(var_numeric, rango_inv = NULL),
               rev_niveles(var_numeric, rango_inv = c(1, 9)))
})
