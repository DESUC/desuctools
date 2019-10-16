test_that("frq_trunc works", {

  vect <- structure(c(1, 2),
                    labels = 'casa grande')
  attr(vect, "labels")

  frq_trunc(vect)


  # expect_equal(frq_trunc(vect, width = 4), 4)
})
