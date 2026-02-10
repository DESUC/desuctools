# edad_rut

Función para calcular una edad estimada según el rut de la persona. Es
solo una aproximación. Puede ser inexacta para personas migrantes.

## Usage

``` r
edad_rut(.rut, fecha_referencia)
```

## Source

https://rutificador-chile.com/wp-content/uploads/2022/06/rut-a-edad.html

## Arguments

- .rut:

  `ìnt`: Vector numérico con el rut (sin dígito verificador).

- fecha_referencia:

  `date`: Vector que contiene la fecha de referencia que determina la
  edad. Pueden ser una fecha pasada, actual o futura.

## Value

integer

## Examples

``` r
# Importante: el rut no debe contar con el dígito verificador
x <- 20117419
fecha <- as.Date("2024-01-31")

edad_rut(.rut = x,
         fecha_referencia = fecha)
#> [1] 24
```
