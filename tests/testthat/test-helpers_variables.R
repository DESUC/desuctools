test_that("str_collapse funciona", {

  df_vect <- data.frame(x = c('a', 'b', 'c'))

  expect_equal(str_collapse(df_vect, x), 'a, b, c')
})


test_that("str_entre funciona", {

  text <- 'x (entre) y'

  expect_equal(str_entre(text, ini = '\\(', fin = '\\)'), 'entre')
  expect_equal(str_entre(text, ini = ' ', fin = ' '), '(entre)')
  expect_equal(str_entre(text, ini = ' '), '(entre) y')
})


test_that('digitos_entero funciona', {
  num_vec <- c(1, 123, -123)

  expect_equal(digitos_entero(num_vec), c(10.0, 12.3, -12.3))
})


test_that("is_email funciona", {

  mails <- c('a@a.com', 'a@a', '--@---.___-')

  expect_equal(is_email(mails), c(TRUE, FALSE, FALSE))
})


# Datos para probar `shift_missing`
df <- data.frame(v1 = c(1, 2,  3, 9, NA),
                 v2 = c(1, 9, NA, 4,  5))

df$v1 <- haven::labelled(df$v1,
                         labels = c('red' = 1, 'blue' = 2),
                         label = 'colours')

v1_result <- haven::labelled(c(1, 2, 3, NA, NA),
                             labels = c('red' = 1, 'blue' = 2),
                             label = 'colours')

test_that("shift_missing para var1", {
  # Remplazar valores anotados como missings en el primer vector
  expect_equal(shift_missing(df, v1, missing = 9)[['v1']], v1_result)
})

test_that("shift_missing mantiene etiquetas", {
  # Remplazar valores anotados como missings en el primer vector
  label <- attr(shift_missing(df, v1, missing = 9)[['v1']], 'label')

  expect_equal(label, 'colours')
})

test_that("shift_missing para var2", {
  # Remplazar valores anotados como missings en el primer vector
  expect_equal(shift_missing(df, v1, v2, missing = 9)[['v2']], c(1, 9, NA, NA, NA))
})
