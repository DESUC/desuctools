# Tabla con intervalos de confianza

Devuelve tabla de frecuencias con intervalos de confianza para un nivel
\`level\`de significancia entre las categorías de respuesta de la
variable \`.var\`.

## Usage

``` r
svy_tabla_var_segmentos(.data, .var, .segmentos = NULL, ...)
```

## Arguments

- .data:

  \`tbl_svy\` data.frame con diseño de encuesta.

- .var:

  Variable de interés respecto.

- .segmentos:

  vars(). Lista de variables por las que se quiere segmentar \`.var\`.

- ...:

  atributos que se pasan a funcion \`svy_tabla_var_segmento\`.

## Value

data.frame
