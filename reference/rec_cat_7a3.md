# Recodificaión de variables 7 a 3

Por defecto recodifica variables de \_notas\_ de 7 niveles a 3 niveles.

## Usage

``` r
rec_cat_7a3(variable, rec = "1:4 = 1; 5 = 2; 6:7 = 3; else = 9", labels = NULL)
```

## Arguments

- variable:

  numeric, Valores de variable a recodificar.

- rec:

  string, por defecto recodifica 1:4 en 1, 5 en 2 y 6:7 en 3. Todo lo
  demás lo deja como 9

- labels:

  vector string, etiquetas para las variables recodificadas. Por defecto
  NULL.

## Value

haven_labelled

## Examples

``` r
vect <- c(1, 4, 6, 99, NA)
rec_cat_7a3(vect, labels = c("alto" = 1))
#> <labelled<double>[5]>
#> [1]  1  1  3  9 NA
#> 
#> Labels:
#>  value label
#>      1  alto
```
