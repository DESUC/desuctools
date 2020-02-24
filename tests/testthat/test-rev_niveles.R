var_labelled <- structure(c(1, 2, 2, 9),
                          labels = c('a' = 1, 'b' = 2, 'nr' = 9),
                          label = 'this is labelled',
                          class = 'haven_labelled')

test_that("rev_niveles haven_labelled", {

  # Valores
  expect_equal(as.numeric(rev_niveles(var_labelled, rango_inv = NULL)), c(9, 8, 8, 1))
  # Etiquetas
  expect_equal(attr(rev_niveles(var_labelled, rango_inv = NULL), 'labels'), c('a' = 9, 'b' = 8, 'nr' = 1))

  # Valores
  expect_equal(as.numeric(rev_niveles(var_labelled, rango_inv = c(1, 2))), c(2, 1, 1, 9))
  # Etiquetas
  expect_equal(attr(rev_niveles(var_labelled, rango_inv = c(1, 2)), 'labels'), c('a' = 2, 'b' = 1, 'nr' = 9))
})

test_that("rev_niveles factor", {
  var_factor <- factor(c('a', 'b', 'b', 'nr'))

  expect_equal(levels(rev_niveles(var_factor, niveles_inv = c('a', 'b'))), c('b', 'a', 'nr'))

  expect_equal(levels(rev_niveles(var_factor)), c('nr', 'b', 'a'))
})

test_that("rev_niveles numeric", {
  var_numeric <- c(1, 2, 2, 9)

  class(var_labelled) <- NULL

  expect_equal(rev_niveles(var_numeric, rango_inv = NULL), c(9, 8, 8, 1))

  expect_equal(rev_niveles(var_numeric, rango_inv = c(1, 2)), c(2, 1, 1, 9))

  expect_equal(as.numeric(rev_niveles(var_labelled, rango_inv = c(1, 2))), c(2, 1, 1, 9))
})
