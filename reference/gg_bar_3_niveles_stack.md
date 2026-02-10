# Barras 3 niveles positivo neutro negativo

Gráfico de barras diseñado para comparar categoría positiva, negativa y
neutra.

## Usage

``` r
gg_bar_3_niveles_stack(
  .df,
  x = pregunta_lab,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  missing = NULL,
  text_size = 3,
  flip = TRUE,
  colour_neg_neu_pos = c("#ec363e", "#dba008", "#0b5ed6"),
  y_prop = prop,
  y_na = 1.1,
  x_na = 0.6,
  facet_col = NULL,
  facet_row = NULL,
  x_str_entre_ini = "",
  x_str_entre_fin = "",
  x_str_width = 50,
  colour_na = "grey20",
  font_family = "Roboto"
)
```

## Arguments

- .df:

  `data.frame` Debe contener variables `pregunta_lab` y `pregunta_cat`.
  Funciona bien a partir de data.frame de resultado de función
  `tabla_vars_segmentos`.

- x:

  `quo` Nombre de variable a utilizar en eje X.

- title:

  `chr` Título del gráfico.

- subtitle:

  `chr` Subtítulo del gráfico.

- caption:

  `chr` Caption del gráfico.

- missing:

  `chr` vector con categorías de respuesta consideradas 'missing'.

- text_size:

  `num` tamaño de letra.

- flip:

  `logical` TRUE gira los ejes.

- colour_neg_neu_pos:

  Vector con tres colores para negativo, neutro y positivo.

- y_prop:

  `chr` Variable con valor de proporciones a graficar.

- y_na:

  `dbl` posición de la etiqueta en y de valores missing.

- x_na:

  `dbl` posición de la etiqueta en x de valores missing.

- facet_col:

  Variable de facet columna.

- facet_row:

  Variable de facet fila.

- x_str_entre_ini:

  `chr` caracter desde el cual se cortará la etiqueta de x. El caracter
  no queda incluido. Si queda en blanco ”, parte desde el inicio,

- x_str_entre_fin:

  `chr` caracter hasta donde se cortará la etiqueta de x. El caracter no
  queda incluido. Si queda en blanco ”, termina al final.

- x_str_width:

  `int` numero de caracteres para wrap las etiquetas de x.

- colour_na:

  color para los valores de dato missing, si se incluye.

- font_family:

  letra a utilizar en el gráfico. Por defecto se usa 'Calibre'.

## Value

ggplot

## Examples

``` r
df_chart <- data.frame(pregunta_lab = c(rep('a', 4), rep('b', 4)),
                       x_other = c(rep('x', 4), rep('y', 4)),
                       prop = c(-0.1, 0.3, 0.4, 0.1, -0.3, 0.1, 0.4, 0.05),
                       pregunta_cat = factor(rep(c('bajo', 'medio', 'alto', 'ns'), 2),
                                             levels = c('bajo', 'medio', 'alto', 'ns')))

gg_bar_3_niveles_stack(df_chart,
                       missing = 'ns',
                       title = 'Prueba',
                       font_family = NULL)

```
