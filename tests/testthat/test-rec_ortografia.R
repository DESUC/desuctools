test_that("multiplication works", {
  expect_equal(rec_ortografia(c('pais', 'arbol')),
               c('país', 'árbol'))

  expect_equal(rec_ortografia(c('cristián', 'desuc'),
                              sorted_words = c('cristián', 'desuc', desuctools::corpus_rae$Orden[1:100])),
               c('cristián', 'desuc'))
})
