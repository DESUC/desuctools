# Estados de la paginación de Alchemer, con respuestas simuladas (sin red).

alch_mock <- function(total_count, total_pages, por_pagina = 2) {
  function(req) {
    page <- as.integer(httr2::url_parse(req$url)$query$page)
    n <- if (total_count == 0) 0 else min(por_pagina, total_count - (page - 1) * por_pagina)
    httr2::response_json(
      body = list(
        result_ok = TRUE,
        total_count = total_count,
        page = page,
        total_pages = total_pages,
        results_per_page = por_pagina,
        data = lapply(seq_len(n), function(i) list(id = (page - 1) * por_pagina + i))
      )
    )
  }
}

alch_paginado_test <- function(page) {
  alch_get_paginado(
    path = list("survey", 1, "surveyresponse"),
    api_token = "token",
    api_token_secret = "secret",
    results_per_page = 2,
    page = page
  )
}

test_that("pagina_unica devuelve la respuesta de la API sin modificar", {
  httr2::local_mocked_responses(alch_mock(total_count = 5, total_pages = 3))

  resp <- alch_paginado_test(page = 2)

  expect_equal(resp$page, 2)
  expect_equal(vapply(resp$data, \(x) x$id, numeric(1)), c(3, 4))
})

test_that("sin_datos devuelve la primera página tal cual", {
  httr2::local_mocked_responses(alch_mock(total_count = 0, total_pages = 0))

  resp <- alch_paginado_test(page = "all")

  expect_equal(resp$page, 1)
  expect_length(resp$data, 0)
})

test_that("una_pagina marca los metadatos como combinados", {
  httr2::local_mocked_responses(alch_mock(total_count = 2, total_pages = 1))

  resp <- alch_paginado_test(page = "all")

  expect_equal(resp$page, "all")
  expect_equal(resp$results_per_page, 2)
  expect_length(resp$data, 2)
})

test_that("paginas_restantes concatena todas las páginas en orden", {
  httr2::local_mocked_responses(alch_mock(total_count = 5, total_pages = 3))

  resp <- alch_paginado_test(page = "all")

  expect_equal(resp$page, "all")
  expect_equal(resp$results_per_page, 5)
  expect_equal(vapply(resp$data, \(x) x$id, numeric(1)), 1:5)
})

test_that("error_api informa el mensaje entregado por Alchemer", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(
      body = list(result_ok = FALSE, message = "Invalid survey")
    )
  })

  expect_error(alch_paginado_test(page = "all"), "Invalid survey")
})

test_that("las funciones exportadas usan el endpoint correcto", {
  urls <- character()
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    httr2::response_json(body = list(result_ok = TRUE, total_count = 0))
  })

  alch_get_survey_responses("token", "secret", survey_id = 10)
  alch_get_survey_contacts("token", "secret", survey_id = 10, campaign_id = 20)

  expect_match(urls[1], "/v5/survey/10/surveyresponse\\?", fixed = FALSE)
  expect_match(urls[2], "/v5/survey/10/surveycampaign/20/surveycontact\\?")
})
