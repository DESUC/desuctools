test_that("Order factor", {
  vct <- c(1, 2)

  expect_equal(fct_case_when(vct == 1 ~ 'a',
                             vct == 2 ~ 'b'),
               factor(c('a', 'b')))

  expect_equal(fct_case_when(vct == 1 ~ 'b',
                             vct == 2 ~ 'a'),
               factor(c('b', 'a'), levels = c('b', 'a')))
})


test_that("Label order factor", {
  vct <- c(1, 2)

  x <- fct_case_when(vct == 1 ~ 'a',
                     vct == 2 ~ 'b',
                     label = 'test')

  expect_equal(attr(x, 'label'), 'test')
  expect_equal(sjlabelled::get_label(x), 'test')
})
