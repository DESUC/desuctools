# ---------------------------------------------------------------------------- #
#   Test de funcion rec_labelled
# ---------------------------------------------------------------------------- #

# Vector
x <- labelled(
  c(1, 2, 2, 2, 3, 4, 5, 5, 1, NA),
  c("Muy de acuerdo" = 1,
    "De acuerdo" = 2,
    "Ni de acuerdo ni en desacuerdo" = 3,
    "En desacuerdo" = 4,
    "Muy en desacuerdo" = 5)
)

# Lista para recodificacion
ls_rec <- list("1-Muy de acuerdo + De acuerdo" = 1:2,
               "2-Ni de acuerdo ni en desacuerdo" = 3,
               "3-En desacuerdo + Muy en desacuerdo" = 4:5,
               "99-NA" = NA)



test_that("Recodificación de etiquetas", {
  # Configuración del test (si es necesario)

  # Ejecutar la función
  vec_test <- rec_labelled(.vec = x,
                           ls_niveles = ls_rec)

  # Usar expect_* para verificar los resultados
  expect_equal(names(attr(vec_test, "labels")),
               c("Muy de acuerdo + De acuerdo",
                 "Ni de acuerdo ni en desacuerdo",
                 "En desacuerdo + Muy en desacuerdo",
                 "NA"))
})


test_that("Recodificación de valores", {
  # Configuración del test (si es necesario)

  # Ejecutar la función
  vec_test <- rec_labelled(.vec = x,
                           ls_niveles = ls_rec)

  # Usar expect_* para verificar los resultados
  expect_equal(class(vec_test),
               c("haven_labelled", "vctrs_vctr", "double"))
})

