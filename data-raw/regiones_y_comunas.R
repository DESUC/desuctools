## code to prepare `regiones_y_comunas` dataset goes here

regiones_y_comunas <- read.csv2('data-raw/regiones_y_comunas.csv')

usethis::use_data(regiones_y_comunas, overwrite = TRUE)
