test_that("Recodificación a 3 niveles", {
  vect <- c(1, 4, 6, 99, NA)

  expect_equal(rec_cat_5a3(vect,
                           labels = c('alto' = 1)),
               labelled(c(1, 3, 9, 9, NA),
                        labels = c('alto' = 1)))

  expect_equal(rec_cat_7a3(vect,
                           labels = c('alto' = 1)),
               labelled(c(1, 2, 3, 9, NA),
                        labels = c('alto' = 1)))
})
