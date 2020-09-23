sg_var <- Sys.getenv(c("SG_REST_API", "SG_USER", "SG_PASS"))

output <- desuctools::sg_get(api_operation = 'Admin/GetStudioUsers',
                             query = NULL,
                             api_key = sg_var['SG_REST_API'],
                             user = sg_var['SG_USER'],
                             pass = sg_var['SG_PASS'])

test_that("Lista de Studio Users mayor a 0", {
  expect_equal(length(output) > 0, TRUE)
})
