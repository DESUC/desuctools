# Codigos de disposición final según AAPOR

Recodificación de códigos de disposición final de casos según AAPOR.

Tabla de cógidos a partir de Casen 2015

val label 110 Entrevista completa 120 Entrevista parcial 211 Se rechazó
la entrevista 2111 Se rechazó la entrevista Hogar 2112 Se rechazó la
entrevista Seleccionado 212 Se interrumpió la entrevista 223 Se impidió
acceso a la vivienda 224 Vivienda ocupada sin moradores presentes 225
Informante no ubicable o no puede atender 231 Muerte del informante 232
Informante impedido físico/mental para contestar 233 Problema de idioma
236 Otra razón elegible 311 No se envió a terreno 317 Area peligrosa o
de difícil acceso 318 No fue posible localizar la dirección 390 Otra
razón de elegibilidad desconocida 410 Fuera de muestra 451 Empresa,
oficina de gobierno u otra institución 452 Institución (Hospital,
cárcel, asilo de ancianos, etc) 453 Dormitorio colectivo (Militar, de
trabajo, internado) 454 Vivienda en demolición, incendiada, destruida o
erradicada 461 Vivienda particular desocupada 462 Vivienda de veraneo o
de uso temporal 463 Otra razón no elegible

## Usage

``` r
rec_cdf(codigo)
```

## Arguments

- codigo:

  integer. Código de disposición final de casos

## Value

factor con cófigos agrupados.

## Examples

``` r
rec_cdf(c(110, 213))
#> <labelled<integer>[2]>
#> [1] 11 21
#> 
#> Labels:
#>  value                                label
#>     11            I - Entrevistas Completas
#>     12            P - Entrevistas Parciales
#>     21                         R - Rechazos
#>     22                     NC - No Contacto
#>     23               O - Otros, No Responde
#>     31        UH - Elegibilidad Desconocida
#>     39 OH - Elegibilidad Desconocida, Otros
#>     40                     NE - No Elegible
```
