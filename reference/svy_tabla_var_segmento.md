# Tabla con intervalos de confianza

Devuelve tabla de frecuencias con intervalos de confianza para un nivel
\`level\`de significancia entre las categorías de respuesta de la
variable \`.var\`.

## Usage

``` r
svy_tabla_var_segmento(
  .data,
  .var,
  .segmento = NULL,
  na.rm = TRUE,
  level = 0.95
)
```

## Arguments

- .data:

  data frame con diseño complejo

- .var:

  Variable en la que interesa comparar categorías de respuesta.

- .segmento:

  Segmentos de interés para ver diferencias en categorías de variable
  \`.var\`. Por defecto NULL

- na.rm:

  boolean. Considera o no missings, por defecto FALSE.

- level:

  double. Nivel de significancia para intervalos de confianza

## Value

data.frame
