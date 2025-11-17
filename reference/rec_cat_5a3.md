# Recodificaión de variables 5 a 3

Por defecto recodifica variables \_likert\_ de 5 niveles a 3 niveles.

## Usage

``` r
rec_cat_5a3(variable, rec = "1:2 = 1; 3 = 2; 4:5 = 3; else = 9", labels = NULL)
```

## Arguments

- variable:

  numeric, Valores de variable a recodificar.

- rec:

  string, por defecto recodifica 1:2 en 1, 3 en 2 y 4:5 en 3. Todo lo
  demás lo deja como 9

- labels:

  vector string, etiquetas para las variables recodificadas. Por defecto
  NULL.

## Value

haven_labelled

## Examples

``` r
vect <- c(1, 4, 6, 99, NA)
rec_cat_5a3(vect, labels = c('alto' = 1))
#> <labelled<double>[5]>
#> [1]  1  3  9  9 NA
#> 
#> Labels:
#>  value label
#>      1  alto
```
