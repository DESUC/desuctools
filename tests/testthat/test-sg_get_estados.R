# Estados de la respuesta de sg_get(), con respuestas simuladas (sin red).

sg_get_mock <- function(...) {
  sg_get(
    api_operation = "Admin/GetStudioUsers",
    query = NULL,
    api_key = "key",
    user = "user",
    pass = "pass",
    ...
  )
}

test_that("sg_get interpreta json, octet-stream y csv", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(a = 1))
  })
  expect_equal(sg_get_mock(), list(a = 1))

  httr2::local_mocked_responses(function(req) {
    httr2::response(
      headers = list("Content-Type" = "application/octet-stream"),
      body = charToRaw('{"a":1}')
    )
  })
  expect_equal(sg_get_mock(), list(a = 1))

  httr2::local_mocked_responses(function(req) {
    httr2::response(
      headers = list("Content-Type" = "text/csv"),
      body = charToRaw("x,y\n1,2\n")
    )
  })
  expect_equal(sg_get_mock(type = "text/csv"), data.frame(x = 1L, y = 2L))
})

test_that("sg_get informa la operación cuando la consulta falla", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 404)
  })

  expect_error(sg_get_mock(), "Admin/GetStudioUsers.*404")
})

test_that("sg_get avisa ante un tipo de contenido no soportado", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      headers = list("Content-Type" = "text/html"),
      body = charToRaw("<html></html>")
    )
  })

  expect_warning(out <- sg_get_mock(), "text/html")
  expect_null(out)
})

test_that("sg_get valida type", {
  expect_error(sg_get_mock(type = "text/plain"), "type")
})

test_that("sg_get_data funciona con el type por defecto", {
  ids_pedidos <- character()
  httr2::local_mocked_responses(function(req) {
    ids_pedidos <<- c(ids_pedidos, httr2::url_parse(req$url)$query$subjectIDs)
    httr2::response_json(body = list(Subjects = list(list(SubjectID = "1"))))
  })

  out <- sg_get_data(
    api_operation = "SimpleExport",
    interview_ids = 1:100,
    api_key = "key",
    user = "user",
    pass = "pass"
  )

  # 100 ids se dividen en grupos de 99 y 1.
  expect_length(ids_pedidos, 2)
  expect_length(out, 2)
  expect_equal(out[[1]]$SubjectID, "1")
})
