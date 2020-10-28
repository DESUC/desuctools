sg_var <- Sys.getenv(c("SG_REST_API", "SG_USER", "SG_PASS"))

sg_get_fun <- function(api_operation,
                       query = NULL,
                       type = NULL) {
  desuctools::sg_get(api_operation = api_operation,
                     query = query,
                     api_key = sg_var['SG_REST_API'],
                     user = sg_var['SG_USER'],
                     pass = sg_var['SG_PASS'],
                     type = type)
}

test_that("OperationData works en sg_get", {
  output_json <- sg_get_fun(api_operation = 'OperationData',
                            query = list(subjectIDs = 67808534))

  expect_equal(output_json[[1]][['SubjectID']], 67808534)
})


test_that("Lista de Studio Users mayor a 0. output json", {
  output_json <- sg_get_fun(api_operation = 'Admin/GetStudioUsers')

  expect_equal(length(output_json) > 0, TRUE)
})

test_that("Lista de Studio Users mayor a 0. output csv", {
  output_csv <- sg_get_fun(api_operation = 'Admin/GetStudioUsers',
                           type = 'text/csv')

  expect_equal(is.data.frame(output_csv), TRUE)
})
