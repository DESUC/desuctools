library(testthat)
library(lubridate)

test_that("sg_date function works correctly", {

  # La zona horaria puede cambiar según cuando se haga el test.
  timezone_format <- format(Sys.time(), '%z') |>
    (\(tz) paste0(substr(tz, 1, 3), ":", substr(tz, 4, 5)))()

  # Test 1: Fecha como Date con hora 'mañana'
  expect_equal(
    sg_date(as.Date('2024-08-26'), hora = 'mañana'),
    paste0('2024-08-26T00:00:00.0000000', timezone_format)
  )

  # Test 2: Fecha como Date con hora 'tarde'
  expect_equal(
    sg_date(as.Date('2024-08-26'), hora = 'tarde'),
    paste0('2024-08-26T23:59:59.9990000', timezone_format)
  )

  # Test 3: Fecha como POSIXct, debe mantener la hora proporcionada
  expect_equal(
    as.POSIXct('2024-08-26 14:35:00', tz = "America/Santiago") |>
      sg_date(),
    '2024-08-26T14:35:00.0000000-04:00'
  )

  # Test 4: Verificar que lance un error con un formato de fecha incorrecto
  expect_error(
    sg_date('2024-08-26'),
    "Error: 'date' debe ser un objeto de tipo Date o POSIXct."
  )

  # Test 5: Verificar que lance un error con una hora incorrecta
  expect_error(
    sg_date(as.Date('2024-08-26'), hora = 'medianoche'),
    "Error: 'hora' debe ser 'mañana' o 'tarde'."
  )

  # Test 6: Verificar que el valor por defecto de 'hora' es 'mañana'
  expect_equal(
    sg_date(as.Date('2024-08-26')),
    paste0('2024-08-26T00:00:00.0000000', timezone_format)
  )
})
