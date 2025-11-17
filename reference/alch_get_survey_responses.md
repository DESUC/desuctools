# Obtener respuestas de una encuesta de Alchemer

Recupera las respuestas de una encuesta en Alchemer. Permite descargar
una página concreta de resultados o todas las páginas (combinadas)
cuando `page = "all"`. La función valida parámetros y devuelve el objeto
parseado por la API (lista con metadatos y elemento `data` con las
respuestas).

## Usage

``` r
alch_get_survey_responses(
  api_token = Sys.getenv("ALCHEMER_API_KEY"),
  api_token_secret = Sys.getenv("ALCHEMER_API_SECRET"),
  survey_id,
  results_per_page = 500,
  page = "all"
)
```

## Arguments

- api_token:

  \`chr\` Clave pública de API de Alchemer. Por defecto toma
  `Sys.getenv("ALCHEMER_API_KEY")`.

- api_token_secret:

  \`chr\` Clave secreta de API. Por defecto toma
  `Sys.getenv("ALCHEMER_API_SECRET")`.

- survey_id:

  \`int\` ID de la encuesta de la que se desean obtener las respuestas.

- results_per_page:

  \`int\` Número de respuestas por página (1..500). Valor por defecto:
  500.

- page:

  \`int\` o `"all"`. Número de página a descargar, o `"all"` para
  recuperar y combinar todas las páginas.

## Value

\`list\`. Objeto devuelto por la API (parseado a lista). Si
`page = "all"` el elemento `data` contendrá las respuestas de todas las
páginas combinadas.

## Details

\- Valida que las credenciales y parámetros sean correctos antes de
llamar a la API. - Cuando `page = "all"` hace múltiples llamadas (si
procede) y concatena todos los elementos `data` en la respuesta
retornada.

## Examples

``` r
if (FALSE) { # \dontrun{
# definir credenciales (mejor usar variables de entorno en tu sistema)
Sys.setenv(ALCHEMER_API_KEY = "tu_api_token")
Sys.setenv(ALCHEMER_API_SECRET = "tu_api_secret")

# obtener la primera página
resp_page1 <- alch_get_survey_responses(survey_id = 123456, page = 1)
str(resp_page1)

# obtener todas las páginas combinadas
resp_all <- alch_get_survey_responses(survey_id = 123456, page = "all")
length(resp_all$data)    # número total de respuestas descargadas
} # }
```
