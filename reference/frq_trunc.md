# Frecuencia de variable con etiqueta truncada

Se trunca el texto de la etiqueta de la variable a un largo
preestablecido para una mejor visión de la tabla resultante.

## Usage

``` r
frq_trunc(x, width = 50L, ellipsis = "...")
```

## Arguments

- x:

  Un vector, posiblemente etiquetado (`haven_labelled`).

- width:

  numeric. Por defecto = 50. Largo del texto de la etiqueta de la
  variable.

- ellipsis:

  string. Por defecto = '...'.

## Value

Una data.frame con columnas val, label, frq, raw.prc y cum.prc.
