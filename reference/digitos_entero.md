# Número de dígitos enteros.

Número de dígitos enteros para valores con decimales. Diseñada para
casos en que longitud y latitud son capturados como digitos sin decimal
y necesitan ser reescalados.

## Usage

``` r
digitos_entero(x, digits = 2)
```

## Arguments

- x:

  vector numerico.

- digits:

  cantidad de dígitos del número entero que se quiere obtener. Por
  defecto digits = 2.

## Value

vector numerico.

## Examples

``` r
digitos_entero(c(0.1234, 12.34, 1234, 12345),
               digits = 3)
#> [1] 123.40 123.40 123.40 123.45
```
