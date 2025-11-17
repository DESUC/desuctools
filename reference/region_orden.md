# Orden de región

Ordena un vector de números asociadas a las 16 regiones de Chile según
posición geográfica de norte a sur.

## Usage

``` r
region_orden(reg, as.factor = TRUE)
```

## Arguments

- reg:

  \`num\`. numérico con valores de 1 a 16

- as.factor:

  \`logical\`. Por defecto TRUE para crear factor ordenado
  geográficamente. FALSE crea variable de clase \`labelled\`.

## Value

ordered factor

## Examples

``` r
region_orden(c(1, 13, 5, 15))
#> [1] Tarapacá           Metropolitana      Valparaíso         Arica y Parinacota
#> 16 Levels: Arica y Parinacota Tarapacá Antofagasta Atacama ... Magallanes
```
