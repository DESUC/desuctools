# Tabla de categorías

Porcentaje de respuesta de categorías de varias variables.
Principalmente para mostrar la distribución de casos de variables de
segmentación posteriores.

## Usage

``` r
tabla_categorias(.df, ..., .wt = NULL, miss = NULL)
```

## Arguments

- .df:

  data.frame. Base de datos.

- ...:

  Preguntas de las que se quiere saber su proporcion. Se puede utilizar
  \`tidyselect\` para facilitar la selección de varias columnas.

- .wt:

  name. Nombre de columna con ponderador o expansor de los datos. Por
  defecto es NULL, sin expansor.

- miss:

  chr vector. Categorías de respuesta que deben excluirse del cálculo de
  \`prop_val\`.

## Value

tibble
