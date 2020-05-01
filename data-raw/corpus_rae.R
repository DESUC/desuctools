## code to prepare `corpus_rae.rds` dataset goes here
#
# Obtener Corpus de Referencia del Español Actual (CREA) - Listado de frecuencias
#
# El Corpus de Referencia del Español Actual (CREA) es un conjunto de textos de diversa procedencia,
# almacenados en soporte informático, del que es posible extraer información
# para estudiar las palabras, sus significados y sus contextos.
#
# http://corpus.rae.es/lfrecuencias.html
#
# 10.000 palabras
# http://corpus.rae.es/frec/10000_formas.TXT
#
# Full lista
# http://corpus.rae.es/frec/CREA_total.zip

full <- FALSE

if(full){
  file <- tempfile(fileext = '.zip')
  download.file(url = 'http://corpus.rae.es/frec/CREA_total.zip',
                destfile = file)

  file <- unzip(file,
                exdir = tempdir())
} else {
  file <- tempfile(fileext = '.txt')
  download.file(url = 'http://corpus.rae.es/frec/10000_formas.TXT',
                destfile = file)
}

corpus_rae <- read.table(file,
                         header = TRUE,
                         strip.white = TRUE,
                         dec = '.',
                         fileEncoding = 'latin1')

# Transformar con Frec.absoluta en números.
corpus_rae$Frec.absoluta <- gsub(corpus_rae$Frec.absoluta,
                                 pattern = ',',
                                 replacement = '')
corpus_rae$Frec.absoluta <- as.integer(corpus_rae$Frec.absoluta)

# Llevar palabras a UTF8
corpus_rae$Orden <- enc2utf8(corpus_rae$Orden)

usethis::use_data(corpus_rae, overwrite = TRUE)
