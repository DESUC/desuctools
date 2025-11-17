# Frecuencia de variable con etiqueta truncada

Se trunca el texto de la etiqueta de la variable a un largo
preestablecido para una mejor visión de la tabla resultante.

## Usage

``` r
frq_trunc(..., width = 50L, ellipsis = "...")
```

## Arguments

- ...:

  Una data frame o vector según lo requerido por \`sjmisc::frq\`.

- width:

  numeric. Por defecto = 50. Largo del texto de la etiqueta de la
  variable.

- ellipsis:

  string. Por defecto = '...'.

## Value

Una kable con el formato DESUC
