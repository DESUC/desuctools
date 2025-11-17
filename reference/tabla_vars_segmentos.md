# Tabla de porcentajes de variables según segmentos.

Obtiene porcentajes de respuestas de múltiples variables según multiples
segmentos.

## Usage

``` r
tabla_vars_segmentos(
  .data,
  .vars,
  .segmentos,
  .wt = NULL,
  total = FALSE,
  miss = NULL
)
```

## Arguments

- .data:

  tibble

- .vars:

  vars(), lista de nombres de variables de las que se quiere saber su
  proporción de respuestas

- .segmentos:

  vars(), lista de nombres de variables de segmentación de las preguntas
  de \`.vars\`

- .wt:

  name, nombre de la variable de ponderación

- total:

  logical, Si total = TRUE, se agrega el total para cada segmento.

- miss:

  integers, Vector de valores que deben coniderarse como missings.

## Value

tibble
