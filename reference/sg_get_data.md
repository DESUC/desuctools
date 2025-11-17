# SurveyToGo rest API: Get individual data

Función para bajar datos y respuestas considerando el límite de 99 casos
por envío. Es un envoltorio de la función \`sg_get()\`.

## Usage

``` r
sg_get_data(
  api_operation = c("SimpleExport", "OperationData"),
  interview_ids,
  ...,
  api_key,
  user,
  pass,
  type = NA
)
```

## Arguments

- api_operation:

  \`chr\` Bajar datos según operaciones "SimpleExport" o
  "OperationData".

- interview_ids:

  \`int\` Vector con códigos de interviews de los que se quiera bajar
  información.

- ...:

  \`list\` pasa a query en sg_get

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

data.frame

## See also

sg_get
