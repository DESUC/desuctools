# ---------------------------------------------------------------------------- #
#   Test de funcion edad_rut
# ---------------------------------------------------------------------------- #


# Rut de ejemplo
x <- 20117419
fecha <- "2019-01-31"

# Test de clase
test_that("Class", {

  # Ejecutar la función
  edad <- edad_rut(.rut = x,
                   fecha_referencia = fecha)

  # Usar expect_* para verificar los resultados
  expect_equal(class(edad),
               c("integer"))
})


# Test de valor
# Test de clase
test_that("Valor", {

  # Ejecutar la función
  edad <- edad_rut(.rut = x,
                   fecha_referencia = fecha)

  # Usar expect_* para verificar los resultados
  expect_equal(edad,
               c(24))
})
