# Dinero en formato \$

Transforma número a texto como \`\$\` con separación de miles con \`.\`
y decimales con \`,\`.

## Usage

``` r
format_dinero(x, digits = 0)
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
format_dinero(1000000)
#> [1] "$ 1.000.000"
```
