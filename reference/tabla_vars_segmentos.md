# Tabla de porcentajes de variables según segmentos.

Obtiene porcentajes de respuestas de múltiples variables según múltiples
segmentos. Función genérica que funciona con data.frames regulares y
objetos tbl_svy.

## Usage

``` r
tabla_vars_segmentos(.df, .vars, .segmentos = NULL, ...)

# S3 method for class 'data.frame'
tabla_vars_segmentos(
  .df,
  .vars,
  .segmentos = NULL,
  .wt = NULL,
  miss = NULL,
  total = FALSE,
  ...
)

# S3 method for class 'tbl_svy'
tabla_vars_segmentos(
  .df,
  .vars,
  .segmentos = NULL,
  miss = NULL,
  vartype = c("ci", "se"),
  level = 0.95,
  ...
)

# S3 method for class 'survey.design2'
tabla_vars_segmentos(
  .df,
  .vars,
  .segmentos = NULL,
  miss = NULL,
  vartype = c("ci", "se"),
  level = 0.95,
  ...
)
```

## Arguments

- .df:

  Un data.frame, tibble o tbl_svy

- .vars:

  tidyselect, lista de nombres de variables de las que se quiere saber
  su proporción de respuestas

- .segmentos:

  tidyselect, lista de nombres de variables de segmentación de las
  preguntas de `.vars`

- ...:

  Parámetros adicionales que serán pasados a los métodos específicos

- .wt:

  name, nombre de la variable de ponderación

- miss:

  integers, Vector de valores que deben coniderarse como missings.

- total:

  logical, Si total = TRUE, se agrega el total para cada segmento.

- vartype:

  chr vector. Tipo de error para estimaciones de encuesta.

- level:

  double. Nivel de confianza para intervalos.

## Value

tibble
