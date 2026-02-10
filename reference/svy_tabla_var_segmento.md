# Tabla con intervalos de confianza

Devuelve tabla de frecuencias con intervalos de confianza para un nivel
`level`de significancia entre las categorías de respuesta de la variable
`.var`.

## Usage

``` r
svy_tabla_var_segmento(
  .df,
  .var,
  .segmento = NULL,
  miss = NULL,
  vartype = c("ci", "se"),
  level = 0.95
)
```

## Arguments

- .df:

  data frame con diseño complejo

- .var:

  Variable en la que interesa comparar categorías de respuesta.

- .segmento:

  Segmentos de interés para ver diferencias en categorías de variable
  `.var`. Por defecto NULL

- miss:

  chr vector. Categorías a excluir en el denominador de `prop_val` y
  `mean`.

- vartype:

  chr vector. Tipo de error para estimaciones (`ci`, `se`, etc.).

- level:

  double. Nivel de significancia para intervalos de confianza

## Value

tibble
