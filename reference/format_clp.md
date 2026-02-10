# Dinero en formato CLP

Transforma número a texto como `CLP` con separación de miles con `.` y
decimales con `,`.

## Usage

``` r
format_clp(x, digits = 0)
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
format_clp(1000000)
#> [1] "CLP 1.000.000"
```
