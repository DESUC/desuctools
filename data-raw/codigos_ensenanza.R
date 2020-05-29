## code to prepare `codigos_ensenanza` dataset goes here

codigos_ensenanza <- read.csv2('data-raw/codigos_ensenanza.csv')

# Llevar palabras a UTF8
codigos_ensenanza$descripcion   <- enc2utf8(codigos_ensenanza$descripcion)
codigos_ensenanza$cod_ense2_lab <- enc2utf8(codigos_ensenanza$cod_ense2_lab)
codigos_ensenanza$cod_ense3_lab <- enc2utf8(codigos_ensenanza$cod_ense3_lab)

usethis::use_data(codigos_ensenanza, overwrite = TRUE)
