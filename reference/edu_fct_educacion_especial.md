# Factor para distingir Educación Especial

Recodificación para bases de datos disponibles en el Centro de Estudios
MINEDUC, Chile Diferencia Educación Especial en tres niveles. es igual
al dígito `6`.

Los cod_educ relacioandos son:

- `211`: Educación Especial Discapacidad Auditiva

- `212`: Educación Especial Discapacidad Intelectual

- `213`: Educación Especial Discapacidad Visual

- `214`: Educación Especial Trastornos Específicos del Lenguaje

- `215`: Educación Especial Trastornos Motores

- `216`: Educación Especial Autismo

- `217`: Educación Especial Discapacidad Graves Alteraciones en la
  Capacidad de Relación y Comunicación

- `218`: Educación Especial Discapacidad Múltiple

- `219`: Educación Especial Sordoceguera

- `299`: Opción 4 Programa Integración Escolar

## Usage

``` r
edu_fct_educacion_especial(x)
```

## Arguments

- x:

  A numeric vector.

## Value

A factor with 3 levels, with an attached `"label"` attribute.

## See also

<https://centroestudios.mineduc.cl/datos-abiertos/>

## Examples

``` r
edu_fct_educacion_especial(c(211, 211, 216, 214, 110)) |>
table()
#> 
#>                         Educación especial 
#>                                          3 
#> Educación especial Trastornos del Lenguaje 
#>                                          1 
#>                                         No 
#>                                          1 
```
