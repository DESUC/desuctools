test_that("frq_trunc works", {

  vect <- structure(c(1, 2),
                    labels = c('val 1' = 1, 'val 2' = 2))

  df <- data.frame(x = structure(c(1, 2),
                                 labels = c('val 1' = 1, 'val 2' = 2)),
                   y = structure(c(3, 4),
                                 labels = c('val 3' = 3, 'val 4' = 4)))

  names(attr(vect, "labels")) %>% str_trunc(width = 3, ellipsis = '')

  frq_trunc(vect)


  # expect_equal(frq_trunc(vect, width = 4), 4)
})


frq_trunc(df, x, width = 3, weights = c(1, 5))

frq_trunc(df, x, width = 3, weights = y)


frq_trunc(vect, width = 3)

sjmisc::frq(df_x, x, y)

map(sjlabelled::get_labels(vect), ~ str_trunc(., width = 3, ellipsis = ''))


lab <- purrr::map(sjlabelled::get_labels(vect),
                  ~ str_trunc(., width = 3, ellipsis = ''))

unlist(lab)

vect_x <- sjlabelled::set_labels(vect, labels = lab)

sjmisc::frq(vect_x, weights = )


lab <- map(sjlabelled::get_labels(df),
           ~ str_trunc(., width = 3, ellipsis = ''))

df_x <- sjlabelled::set_labels(df, labels = lab)

sjmisc::frq(df_x)
