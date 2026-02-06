# Calculo de proporción para categorías válidas.

Calculo de proporción para categorías válidas.

## Usage

``` r
tabla_prop_val(.df, by = NULL, miss)
```

## Arguments

- .df:

  tibble

- by:

  chr vector. variables respecto de la que se calculará el porcentaje.

- miss:

  chr vector. Categorías que se deben considerar como missing o
  inválidas.

## Value

tibble

## Examples

``` r
df <- tibble::tibble(
pregunta_cat = c("a", "b", "x"),
casos = c(30, 30, 40)
)

desuctools:::tabla_prop_val(df, miss = "x")
#> # A tibble: 3 × 3
#>   pregunta_cat casos prop_val
#>   <chr>        <dbl>    <dbl>
#> 1 a               30      0.5
#> 2 b               30      0.5
#> 3 x               40     NA  

desuctools:::tabla_prop_val(df, by = "pregunta_cat", miss = "x")
#> # A tibble: 3 × 3
#>   pregunta_cat casos prop_val
#>   <chr>        <dbl>    <dbl>
#> 1 a               30        1
#> 2 b               30        1
#> 3 x               40       NA
```
