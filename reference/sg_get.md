# SurveyToGo rest API

Función para acceder a datos del sistema SurveyToGo utilizando su API
REST.

## Usage

``` r
sg_get(
  api_operation,
  query,
  api_key,
  user,
  pass,
  type = c("application/json", "text/xml", "text/csv")
)
```

## Arguments

- api_operation:

  \`chr\` Nombre de alguno de las operaciones GET disponibles en la API
  de SurveyToGo. Ver link en referencias.

- query:

  \`list\` listado de variables que serán pasadas a la api_operation que
  se elija.

- api_key:

  \`chr\` REST API Key entregada por dooblo.

- user:

  \`chr\` nombre de usuario.

- pass:

  \`chr\` password de usuario.

- type:

  \`chr\` Formato en el que se obtendrán datos. Por defecto
  'application/json'. Puede ser 'text/xml' si se quiere xml o 'text/csv'
  si se desea un csv.

## Value

json

## References

https://support.dooblo.net/hc/en-us/articles/208294645-How-To-Use-The-SurveyToGo-REST-API
