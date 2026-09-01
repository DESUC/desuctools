test_that("comuna_orden convierte CUT numérico a nombre correcto", {
  expect_equal(as.character(comuna_orden(1101)), "Iquique")
  expect_equal(as.character(comuna_orden(13101)), "Santiago")
  expect_equal(as.character(comuna_orden(15101)), "Arica")
})

test_that("comuna_orden convierte CUT character (con ceros a la izquierda) a nombre correcto", {
  expect_equal(as.character(comuna_orden("01101")), "Iquique")
  expect_equal(as.character(comuna_orden("13101")), "Santiago")
  expect_equal(as.character(comuna_orden("05101")), "Valparaíso")
})

test_that("comuna_orden numérico y character producen el mismo resultado", {
  cuts_num <- regiones_y_comunas$comuna18
  cuts_chr <- sprintf("%05d", cuts_num)

  expect_equal(
    as.character(comuna_orden(cuts_num)),
    as.character(comuna_orden(cuts_chr))
  )
})

test_that("comuna_orden respeta el orden geográfico (norte a sur) de regiones_y_comunas", {
  niveles <- levels(comuna_orden(regiones_y_comunas$comuna18))
  expect_equal(niveles, regiones_y_comunas$comuna_nom)
})

test_that("comuna_orden devuelve NA y emite warning para CUT inexistente", {
  expect_warning(out <- comuna_orden(99999), "no reconocido")
  expect_true(is.na(out))
})

test_that("comuna_orden no emite warning cuando la entrada ya es NA", {
  expect_no_warning(out <- comuna_orden(NA))
  expect_true(is.na(out))
})

test_that("comuna_orden no emite warning cuando todos los CUT son válidos", {
  expect_no_warning(comuna_orden(c(1101, 13101)))
})

test_that("comuna_orden con as.factor = FALSE devuelve vector labelled", {
  out <- comuna_orden(c(1101, 13101), as.factor = FALSE)
  expect_s3_class(out, "haven_labelled")
  expect_equal(as.numeric(out), c(1101, 13101))
  expect_equal(attr(out, "label"), "Comuna")
})
