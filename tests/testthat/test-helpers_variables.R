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
