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
