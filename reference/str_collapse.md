# Colapso de strings

Variable útil para colapsar en una sola casilla \_list variables\_ en un
tibble que quiera ser mostrad en una tabla

## Usage

``` r
str_collapse(.data, .var, collapse = ", ")
```

## Arguments

- .data:

  tibble

- .var:

  variable name

- collapse:

  string, string con el que se unirán los textos. Por defecto ", ".

## Examples

``` r
df <- data.frame(x = c("a", "b", "c"))

str_collapse(df, x)
#> [1] "a, b, c"
```
