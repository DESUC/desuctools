# Extrae string entre dos textos

Extrae string entre dos textos

## Usage

``` r
str_entre(text, ini = "", fin = "")
```

## Arguments

- text:

  string. Puede ser un named string.

- ini:

  string, desde donde se extrae el texto

- fin:

  string, hasta donde se extrae el texto

## Value

string

## Examples

``` r
str_entre('a (between) z', ini = '\\(', fin = '\\)')
#> [1] "between"
```
