# Orden de comuna

Convierte un vector de CUT comunal (numérico o character, con o sin
ceros a la izquierda) al nombre de la comuna, ordenado geográficamente
de norte a sur según el orden de filas de
[regiones_y_comunas](regiones_y_comunas.md).

## Usage

``` r
comuna_orden(com, as.factor = TRUE)
```

## Arguments

- com:

  `num` o `chr`. CUT comunal, en formato numérico (p. ej. `1101`) o
  character (p. ej. `"01101"`).

- as.factor:

  `logical`. Por defecto TRUE para crear factor ordenado
  geográficamente. FALSE crea variable de clase `labelled`.

## Value

Si `as.factor = TRUE`, un factor con los niveles de comuna ordenados de
norte a sur (según [regiones_y_comunas](regiones_y_comunas.md)). Si
`as.factor = FALSE`, un vector de clase `labelled` con el CUT comunal y
las etiquetas de nombre de comuna. Si algún valor de `com` (no `NA`) no
corresponde a ninguna comuna conocida, se emite un `warning` y ese valor
queda como `NA` en el resultado.

## Examples

``` r
comuna_orden(c(1101, 5101, 13101))
#> [1] Iquique    Valparaíso Santiago  
#> 346 Levels: Arica Camarones Putre General Lagos Iquique ... Torres del Paine
comuna_orden(c("01101", "05101", "13101"))
#> [1] Iquique    Valparaíso Santiago  
#> 346 Levels: Arica Camarones Putre General Lagos Iquique ... Torres del Paine
```
