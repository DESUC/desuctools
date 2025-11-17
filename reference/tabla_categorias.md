# Tabla de categorías

Porcentaje de respuesta de categorías de varias variables.
Principalmente para mostrar la distribución de casos de variables de
segmentación posteriores.

## Usage

``` r
tabla_categorias(.data, ..., .wt = NULL)
```

## Arguments

- .data:

  data frame. Base de datos.

- ...:

  Preguntas de las que se quiere saber su proporcion. Se puede utilizar
  \`tidyselect\` para facilitar la selección de varias columnas.

- .wt:

  Ponderador o expansor de los datos. Por defecto es NULL.

## Value

tibble

## Details

Tablas de resultados —————————————————-

Generación de una data.frame con el número de casos y proporción de las
distintas variables de segmentos que se agregen en \`...\`.
