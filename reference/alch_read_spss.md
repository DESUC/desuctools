# read Alchemer SPSS export

Lee el archivo .sav a partir del *distribution link* de un reporte de
exportación de una encuesta programada en Alchemer.
<https://help.alchemer.com/help/spss>

Compatibility

- Comments are not available in SPSS exports.

- The Conjoint question is not available in SPSS exports.

- "Other, Write-In" rows in grid questions as not available in SPSS
  exports.

- The Time Started field is not includes as part of the SPSS export.

## Usage

``` r
alch_read_spss(url)
```

## Arguments

- url:

  `chr` Distribution link del reporte de base de datos en SPSS.

## Value

tibble

## Details

Funciones para el trabajo con Alchemer Servicio de encuestas web
<https://app.alchemer.com/>
