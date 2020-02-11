test_that("frq_trunc works", {

  vect <- structure(c(1, 2),
                    labels = c('val 1' = 1, 'val 2' = 2))

  df <- data.frame(x = structure(c(1, 2),
                                 labels = c('val 1' = 1, 'val 2' = 2)),
                   y = structure(c(3, 4),
                                 labels = c('val 3' = 3, 'val 4' = 4)))

  expect_equal(as.data.frame(frq_trunc(vect, width = 4))$label, c('v...', 'v...', NA_character_))
})
