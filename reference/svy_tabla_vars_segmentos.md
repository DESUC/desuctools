# Tabla con intervalos de confianza

Devuelve tabla de frecuencias con intervalos de confianza para un nivel
\`level\`de significancia entre las categorías de respuesta de la
variable \`.vars\`.

A diferencia de svy_tabla_var_segmentos, esta funcion puede procesar
varias variables y segmentos a la vez.

## Usage

``` r
svy_tabla_vars_segmentos(
  .df,
  .vars,
  .segmentos = NULL,
  miss = NULL,
  vartype = c("ci", "se"),
  level = 0.95
)
```

## Arguments

- .df:

  \`tbl_svy\` data.frame con diseño de encuesta.

- .vars:

  c(). Variables de interés respecto.

- .segmentos:

  c(). Lista de variables por las que se quiere segmentar \`.vars\`.

- miss:

  chr vector. Categorías a excluir en el denominador de \`prop_val\` y
  \`mean\`.

- vartype:

  chr vector. Tipo de error para estimaciones (\`ci\`, \`se\`, etc.).

- level:

  double. Nivel de confianza para intervalos.

## Value

tibble
