test_that("Formato CLP y $", {
  val <- 10000.07

  expect_equal(format_clp(val, digits = 1),
               'CLP 10.000,1')

  expect_equal(format_dinero(val, digits = 1),
                 '$ 10.000,1')
})

