# Obtener contactos de una campaña de encuesta en Alchemer

Recupera los contactos asociados a una campaña de encuesta en Alchemer.
Permite descargar una página concreta de contactos o todas las páginas
(combinadas) cuando `page = "all"`. La función valida parámetros y
devuelve el objeto parseado por la API (lista con metadatos y elemento
`data` con los contactos).

## Usage

``` r
alch_get_survey_contacts(
  api_token = Sys.getenv("ALCHEMER_API_KEY"),
  api_token_secret = Sys.getenv("ALCHEMER_API_SECRET"),
  survey_id,
  campaign_id,
  results_per_page = 500,
  page = "all"
)
```

## Arguments

- api_token:

  `chr` Clave pública de API de Alchemer. Por defecto toma
  `Sys.getenv("ALCHEMER_API_KEY")`.

- api_token_secret:

  `chr` Clave secreta de API. Por defecto toma
  `Sys.getenv("ALCHEMER_API_SECRET")`.

- survey_id:

  `int` ID de la encuesta a la que pertenece la campaña.

- campaign_id:

  `int` ID de la campaña de la que se desean obtener los contactos.

- results_per_page:

  `int` Número de contactos por página (1..500). Valor por defecto: 500.

- page:

  `int` o `"all"`. Número de página a descargar, o `"all"` para
  recuperar y combinar todas las páginas.

## Value

`list`. Objeto devuelto por la API (parseado a lista). Si `page = "all"`
el elemento `data` contendrá los contactos de todas las páginas
combinadas.

## Details

- Valida que las credenciales y parámetros sean correctos antes de
  llamar a la API.

- Cuando `page = "all"` hace múltiples llamadas (si procede) y concatena
  todos los elementos `data` en la respuesta retornada.

- Los contactos pueden incluir campos como `id`, `email_address`,
  `first_name`, `last_name`, etc., según la configuración de la campaña.

## Examples

``` r
if (FALSE) { # \dontrun{
# definir credenciales (mejor usar variables de entorno en tu sistema)
Sys.setenv(ALCHEMER_API_KEY = "tu_api_token")
Sys.setenv(ALCHEMER_API_SECRET = "tu_api_secret")

# obtener la primera página de contactos
contactos_page1 <- alch_get_survey_contacts(
  survey_id = 8529571,
  campaign_id = 24631558,
  page = 1
)
str(contactos_page1)

# obtener todos los contactos combinados
contactos_all <- alch_get_survey_contacts(
  survey_id = 8529571,
  campaign_id = 24631558,
  page = "all"
)
length(contactos_all$data)    # número total de contactos descargados
} # }
```
