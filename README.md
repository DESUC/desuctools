
<!-- README.md is generated from README.Rmd. Please edit that file -->

# desuctools <img src="man/figures/desuctools.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/caayala/desuctools/branch/master/graph/badge.svg)](https://codecov.io/gh/caayala/desuctools?branch=master)
<!-- badges: end -->

Funciones y datos auxiliares para análisis de encuestas sociales
utilizados por la [Dirección de Estudios Sociales
UC](http://sociologia.uc.cl/desuc) (DESUC).

## Instalación

Para instalar la versión en desarrollo de desuctools desde
[GitHub](https://github.com/desuc/desuctools) puedes utilizar el
siguiente código:

``` r
# install.packages("devtools")
devtools::install_github("desuc/desuctools")
```

## Ejemplos

Base de datos con información sobre regiones y comunas.

``` r
library(dplyr, warn.conflicts = FALSE)
library(desuctools)

# Tabla con comunas capitales regionales.
desuctools::regiones_y_comunas %>% 
  filter(region_capital) %>% 
  arrange(region_orden) %>% 
  select(region, comuna, comuna_nom) %>% 
  desuctools::kable_desuc(align = 'rrl')
#> Warning in kableExtra::kable_styling(., latex_options = latex_options,
#> repeat_header_text = "(continuación)", : Please specify format in kable.
#> kableExtra can customize either HTML or LaTeX outputs. See https://
#> haozhu233.github.io/kableExtra/ for details.
```

| region | comuna | comuna\_nom  |
| -----: | -----: | :----------- |
|     15 | 15.101 | Arica        |
|      1 |  1.101 | Iquique      |
|      2 |  2.101 | Antofagasta  |
|      3 |  3.101 | Copiapó      |
|      4 |  4.101 | La Serena    |
|      5 |  5.101 | Valparaíso   |
|      6 |  6.101 | Rancagua     |
|      7 |  7.101 | Talca        |
|     16 |  8.401 | Chillán      |
|      8 |  8.101 | Concepción   |
|      9 |  9.101 | Temuco       |
|     14 | 14.101 | Valdivia     |
|     10 | 10.101 | Puerto Montt |
|     11 | 11.101 | Coyhaique    |
|     12 | 12.101 | Punta Arenas |
