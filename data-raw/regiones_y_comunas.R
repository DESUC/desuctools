## code to prepare `regiones_y_comunas` dataset goes here

regiones_y_comunas <- read.csv2('data-raw/regiones_y_comunas.csv', encoding = 'UTF-8')

# Llevar palabras a UTF8
regiones_y_comunas$comuna_nom <- enc2utf8(regiones_y_comunas$comuna_nom)
regiones_y_comunas$region_nom <- enc2utf8(regiones_y_comunas$region_nom)

usethis::use_data(regiones_y_comunas, overwrite = TRUE)
