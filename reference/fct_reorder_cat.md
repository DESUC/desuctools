# Orden de preguntas según valor de categoría

Wrapper al rededor de
[`forcats::fct_reorder2`](https://forcats.tidyverse.org/reference/fct_reorder.html)
para ordenar factor `.f` según según una catogoría de respuesta de `.x`
de interès `cat_orden` a partir del valor en `.y`.

## Usage

``` r
fct_reorder_cat(.f, .cat, .val, cat_orden, .desc = FALSE)
```

## Arguments

- .f:

  factor. Factor que quiere ser ordenado.

- .cat:

  chr o factor. Variable con categorías de respuesta.

- .val:

  numeric. Valor de respuesta para cada categoría de respuesta

- cat_orden:

  string. Nombre de la categoría de respuesta de interés.

- .desc:

  logical. Controla si el orden es ascendente o descendente. Por defecto
  .desc = FALSE.

## Value

factor
