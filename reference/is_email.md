# Validación de sintaxis de email

Validación de la sintaxis de correos electrónicos. La expresión regular
utilizada para ello viene del siguiente post en
[SO](https://stackoverflow.com/questions/201323/how-to-validate-an-email-address-using-a-regular-expression)

## Usage

``` r
is_email(email)
```

## Arguments

- email:

  vector character.

## Value

vector logico

## Examples

``` r

is_email(c("a@a.com", "a@a"))
#> [1]  TRUE FALSE
```
