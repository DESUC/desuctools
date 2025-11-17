# Convierte una fecha para su uso en la API de SurveyToGo

Esta función toma una fecha y la convierte al formato requerido por la
API de SurveyToGo. Si no se especifica una hora, se asume 'mañana'
(00:00:00). Si se especifica 'tarde', se asigna el último segundo del
día (23:59:59).

## Usage

``` r
sg_date(date, hora = "mañana")
```

## Arguments

- date:

  Fecha en formato Date o POSIXct. Si se proporciona una fecha en
  formato POSIXct, se mantendrá la hora proporcionada en el objeto.

- hora:

  Cadena de texto que indica si se debe tomar la 'mañana' (00:00:00) o
  la 'tarde' (23:59:59) como hora de referencia para la fecha. Valor por
  defecto es 'mañana'.

## Value

Un string con la fecha formateada en el estándar ISO 8601 extendido,
incluyendo milisegundos y la zona horaria correspondiente.

## Examples

``` r
# Convertir una fecha en formato Date para la mañana
sg_date(as.Date('2024-08-26'))
#> [1] "2024-08-26T00:00:00.0000000+00:00"

# Convertir una fecha en formato Date para la tarde
sg_date(as.Date('2024-08-26'), hora = 'tarde')
#> [1] "2024-08-26T23:59:59.9990000+00:00"

# Convertir una fecha en formato POSIXct manteniendo la hora
sg_date(as.POSIXct('2024-08-26 14:35:00'))
#> [1] "2024-08-26T14:35:00.0000000+00:00"
```
