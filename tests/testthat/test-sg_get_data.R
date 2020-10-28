sg_var <- Sys.getenv(c("SG_REST_API", "SG_USER", "SG_PASS"))

sg_get_data_test <- function(...) {
  desuctools::sg_get_data(...,
                          api_key = sg_var['SG_REST_API'],
                          user = sg_var['SG_USER'],
                          pass = sg_var['SG_PASS'])
}

test_that("OperationData works", {
  output_json <- sg_get_data_test(api_operation = 'OperationData',
                                  interview_ids = 67808534,
                                  type = 'application/json')

  expect_equal(output_json[[1]][[1]][['SubjectID']], 67808534)
})

test_that("SimpleExport works", {
  output_json <- sg_get_data_test(api_operation = 'SimpleExport',
                                  interview_ids = 67808534,
                                  surveyID = '9dd9b1c9-6381-4a34-87f4-da26368b25db',
                                  type = 'application/json')

  expect_equal(output_json[[1]]$SubjectID, "67808534")
})
