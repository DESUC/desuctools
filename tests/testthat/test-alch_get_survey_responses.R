testthat::skip_on_cran()
testthat::skip_on_ci()
testthat::skip_if_offline()

test_that("alch_get_survey_responses devuelve lista con metadatos y data", {
  resp <- tryCatch(
    desuctools::alch_get_survey_responses(survey_id = 8453877, page = 1),
    error = function(e) {
      testthat::skip(paste(
        "API de Alchemer no disponible:",
        conditionMessage(e)
      ))
    }
  )
  expect_type(resp, "list")
  expect_true("data" %in% names(resp))
  expect_true("total_count" %in% names(resp))
})


test_that("alch_get_survey_responses devuelve todas las páginas cuando page = 'all'", {
  resp <- tryCatch(
    desuctools::alch_get_survey_responses(
      survey_id = 8453877,
      page = "all"
    ),
    error = function(e) {
      testthat::skip(paste(
        "API de Alchemer no disponible:",
        conditionMessage(e)
      ))
    }
  )
  expect_type(resp, "list")
  expect_true(resp$page == "all")
  expect_equal(resp$results_per_page, resp$total_count)
  expect_true(length(resp$data) == resp$total_count)
})


test_that("alch_get_survey_responses lanza error con survey_id inválido", {
  expect_error(
    desuctools::alch_get_survey_responses(survey_id = "no_num"),
    "survey_id"
  )
})
