# Cálculo de proporciones

Cálculo de proporciones

## Usage

``` r
tabla_prop(.df, .segmento)
```

## Arguments

- .df:

  tibble.

- .segmento:

  chr.

## Value

tibble

## Examples

``` r
df <- tibble::tibble(var = c("a", "b"),
            casos = c(30, 70))

desuctools:::tabla_prop(df, .segmento = "var")
#> # A tibble: 2 × 3
#>   var   casos  prop
#>   <chr> <dbl> <dbl>
#> 1 a        30     1
#> 2 b        70     1
```
