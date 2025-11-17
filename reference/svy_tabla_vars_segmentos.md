# Tabla con intervalos de confianza

Devuelve tabla de frecuencias con intervalos de confianza para un nivel
\`level\`de significancia entre las categorías de respuesta de la
variable \`.vars\`.

A diferencia de svy_tabla_var_segmentos, esta funcion puede procesar
varias variables y segmentos a la vez.

## Usage

``` r
svy_tabla_vars_segmentos(.data, .vars, .segmentos = NULL, ...)
```

## Arguments

- .data:

  \`tbl_svy\` data.frame con diseño de encuesta.

- .vars:

  c(). Variables de interés respecto.

- .segmentos:

  c(). Lista de variables por las que se quiere segmentar \`.vars\`.

- ...:

  atributos que se pasan a funcion \`svy_tabla_var_segmento\`.

## Value

data.frame
