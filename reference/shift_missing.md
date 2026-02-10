# Corregir missings en preguntas múltiples

Función para corregir problemas de no respuesta en preguntas múltiples y
sucesivas.

## Usage

``` r
shift_missing(.data, .var1, .var2 = NULL, missing = c(77L, 88L, 99L))
```

## Arguments

- .data:

  Una data frame

- .var1:

  nombre de la variable la primera variable

- .var2:

  nombre de la variable la segunda variable

- missing:

  vector con valores considerados no válidos (por defecto `77, 88, 99`).
  `NA` es considerado siempre.

## Value

Una data frame con los valores de .var1 y .var2 corregidos

## Details

Funciones de trabajo con datos ——————————————
