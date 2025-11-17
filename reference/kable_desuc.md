# Función para ajustar estilo a tablas al momento de ser mostradas en un informe

Función para ajustar estilo a tablas al momento de ser mostradas en un
informe

## Usage

``` r
kable_desuc(
  .data,
  digits = 1,
  row.names = NA,
  col.names = NA,
  align = NULL,
  caption = NULL,
  booktabs = TRUE,
  longtable = FALSE,
  escape = TRUE,
  font_size = 8,
  latex_options = c("hold_position"),
  ...
)
```

## Arguments

- .data:

  Una data frame

- digits:

  \`int\` Número de decimales en la tabla (por defecto digits = 1)

- row.names:

  \`Logical\` Se incluye o no los nombres de las filas.

- col.names:

  \`strings\` Vector de texto para los nombres de las columnas. (por
  defecto igual al colname de la tabla)

- align:

  \`vector\` Indicación para la alineación de las columnas. c('rll')

- caption:

  \`string\` Leyenda asociada a la tabla (por defecto sin leyenda)

- booktabs:

  \`bolean\` si usa estilo booktabs para la tabla (por defecto TRUE)

- longtable:

  \`bolean\` si usa estilo para tablas que cruzan dos páginas (por
  defecto FALSE)

- escape:

  \`bolean\` Si se normalizan los caracterres especiales en html o
  latex. (por defecto TRUE)

- font_size:

  \`ìnt\` tamaño de letra en la tabla (por defecto 8)

- latex_options:

  \`vector de caracteres\` donde se controla aspectos de latex.

- ...:

  Atributos pasados a la función \`kable_styling\`

## Value

Una kable con el formato DESUC
