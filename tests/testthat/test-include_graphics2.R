test_that("latex_img_estado distingue los tres estados", {
  expect_equal(latex_img_estado("fig.png"), "valida")
  expect_equal(latex_img_estado("fig.png", alt_path = "otra.png"), "valida")
  expect_equal(latex_img_estado("fig.gif"), "sin_alternativa")
  expect_equal(latex_img_estado("fig.svg", alt_path = "fig.png"), "alternativa")
  expect_error(latex_img_estado("fig.svg", alt_path = "fig.gif"))
})

test_that("include_graphics_latex usa handler, alternativa o figura original", {
  png <- tempfile(fileext = ".png")
  file.create(png)

  expect_equal(
    include_graphics_latex("fig.gif", handler = function(path) "handler"),
    "handler"
  )
  expect_equal(
    as.character(include_graphics_latex("fig.gif", alt_path = png)),
    png
  )
  expect_equal(as.character(include_graphics_latex(png)), png)
})
