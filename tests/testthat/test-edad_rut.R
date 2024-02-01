# Test de función edad_rut ----

# Rut de ejemplo
x <- 20117419
fecha <- as.Date("2024-01-31")

# Ejecutar la función
edad_rut <- edad_rut(.rut = x,
                     fecha_referencia = fecha)

# Test de clase
test_that("Class", {

  expect_type(edad_rut, "integer")
})


# Test de valor
# Test de clase
test_that("Valor", {

  expect_equal(edad_rut,
               24L)
})
