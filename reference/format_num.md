# Número en formato para texto

Transforma número a texto con separación de miles con \`.\` y decimales
con \`,\`.

## Usage

``` r
format_num(x, digits = 0)
```

## Arguments

- x:

  numeric

- digits:

  cantidad de dígitos del número entero que se quiere obtener. Por
  defecto digits = 0.

## Value

character

## Examples

``` r
format_num(1000000)
#> [1] "1.000.000"
```
