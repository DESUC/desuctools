# Tablas de resultados —————————————————-

Produce una tabla de distribución por categorías para una variable.

## Usage

``` r
tabla_categoria(.df, .var, .wt = NULL, miss = NULL)
```

## Arguments

- .df:

  tibble. datos tidy.

- .var:

  chr. Variable de la que se sabrá su distribución. Solo una variable.

- .wt:

  chr. Variable de ponderación. Solo una variable.

- miss:

  chr vector. Nombre de niveles de categorías de `.var` que deben ser
  considerados missing.

## Details

Generación de una data.frame con el número de casos y proporción de las
distintas variables de segmentos que se agregen en `...`.
