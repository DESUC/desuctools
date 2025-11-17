# Construye tibble de respuestas desde la lista de Alchemer

Construye una tabla (tibble) a partir de la salida de la API de Alchemer
(objeto devuelto por `alch_get_survey_responses`). Extrae para cada
pregunta los campos `id`, `question` y `answer`, pivotea las respuestas
a formato ancho (una fila por respuesta) creando columnas `var<id>` y
asigna la etiqueta de variable (atributo `"label"`) con el texto de la
pregunta (etiquetas HTML son limpiadas).

## Usage

``` r
alch_create_df(ls_alchemer)
```

## Arguments

- ls_alchemer:

  List. Objeto parseado de la API de Alchemer; debe contener el elemento
  `data` con las respuestas (lista de respuestas).

## Value

Tibble. Una fila por respuesta de encuesta con las columnas de metadatos
originales y columnas de respuesta llamadas `var<id>`. Cada columna de
respuesta recibe el atributo `"label"` con la pregunta correspondiente
(texto limpio).

## Details

\- Si una pregunta no tiene `answer`, el valor resultante será `NA`. -
Respuestas múltiples por misma pregunta se colapsan en una cadena
separada por `"; "`. - La función asume que `ls_alchemer[["data"]]` es
una lista con la estructura estándar de la API (cada elemento contiene
sublistas con elementos `id`, `question`, `answer` entre otros).

## Examples

``` r
if (FALSE) { # \dontrun{
resp <- alch_get_survey_responses(survey_id = 8487186)
df <- alch_create_df(resp)
head(df)
# ver label de la columna var3
attr(df$var3, "label")
} # }
```
