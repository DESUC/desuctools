## code to prepare `codigos_ensenanza` dataset goes here

codigos_ensenanza <- read.csv2('data-raw/codigos_ensenanza.csv')

usethis::use_data(codigos_ensenanza, overwrite = TRUE)
