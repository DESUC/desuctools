# varnames_to_label

Toma nombre de variables y los separa en un nombre breve y el resto de
la etiqueta la integra en el atributo \`label\` de la variable. Además
cambia la clase de las variables modificadas a \`haven_labelled\`

## Usage

``` r
varnames_to_label(
  .df,
  pattern_detect,
  pattern_extract = NULL,
  var_prefix = NULL
)
```

## Arguments

- .df:

  \`data.frame\` Un data.frame

- pattern_detect:

  \`chr\` patrón de regex con el que se extrae el nombre breve de la
  variable. Este es el que será utilizado posteriormente como nombre
  final de la variable en la base

- pattern_extract:

  \`chr\` patrón de regex opcional. Por defecto es NULL e implica que se
  usará el mismo patrón que el dispuesto en \`pattern_detect\`.

- var_prefix:

  \`chr\` opcional. Prefijo para el nombre de las nuevas variables que
  se extraigan.

## Value

data.frame

## Examples

``` r
df <- data.frame(`first variable 1` = 1,
                 `second variable 2` = 2,
                 var_1 = 3L,
                 check.names = FALSE)

varnames_to_label(df, pattern_detect = ' \\d$',
                  var_prefix = 'p')
#>   p_1 p_2 var_1
#> 1   1   2     3

varnames_to_label(df,
                  pattern_detect = ' \\d$',
                  pattern_extract = '\\d$',
                  var_prefix = 'p')
#>   p1 p2 var_1
#> 1  1  2     3
```
