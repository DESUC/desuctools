df <- data.frame(`first variable 1` = 1,
                 `second variable 2` = 2,
                 var_1 = 3L,
                 check.names = FALSE)

test_that("varnames to label pattern and prefix", {

  df_label <- varnames_to_label(df, pattern_detect = ' \\d$',
                                var_prefix = 'p')

  expect_equal(names(df_label),
               c('p_1', 'p_2', 'var_1'))
  expect_equal(as.character(sapply(df_label, class)),
               c('haven_labelled', 'haven_labelled', 'integer'))
  expect_equal(as.character(sapply(df_label, function(x) attr(x, 'label'))),
               c('first variable', 'second variable', 'NULL'))
})

test_that("varnames to label pattern and prefix", {

  df_label <- varnames_to_label(df, pattern_detect = ' \\d$',
                                pattern_extract = '\\d$',
                                var_prefix = 'p')

  expect_equal(names(df_label),
               c('p1', 'p2', 'var_1'))
})
