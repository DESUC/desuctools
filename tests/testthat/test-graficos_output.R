test_that("gg_bar_3_niveles_stack", {
  df_chart <- data.frame(pregunta_lab = c(rep('a', 4), rep('b', 4)),
                         x_other = c(rep('x', 4), rep('y', 4)),
                         prop = c(-0.1, 0.3, 0.4, 0.1, -0.3, 0.1, 0.4, 0.05),
                         pregunta_cat = factor(rep(c('bajo', 'medio', 'alto', 'ns'), 2),
                                               levels = c('bajo', 'medio', 'alto', 'ns')))

  gg_bar_3_niveles_stack(df_chart,
                         missing = 'ns',
                         title = 'Gráfico de prueba')

  expect_s3_class(gg_bar_3_niveles_stack(df_chart, missing = 'ns'), 'ggplot')
})
