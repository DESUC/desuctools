# Test de funcion rec_labelled ----

# Vector
v_labelled <- labelled(
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

# Ejecutar la función
v_labelled_rec <- rec_labelled(.vec = v_labelled,
                               ls_niveles = ls_rec)

test_that("Recodificación de etiquetas", {

  expect_equal(names(attr(v_labelled_rec, "labels")),
               c("Muy de acuerdo + De acuerdo",
                 "Ni de acuerdo ni en desacuerdo",
                 "En desacuerdo + Muy en desacuerdo",
                 "NA"))
})

test_that("Recodificación de valores", {

  # Usar expect_* para verificar los resultados
  expect_equal(as.numeric(v_labelled_rec),
               c(1, 1, 1, 1, 2, 3, 3, 3, 1, 99))
})

test_that("Clase", {

  # Usar expect_* para verificar los resultados
  expect_s3_class(v_labelled_rec, "haven_labelled")
})

