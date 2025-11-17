# Corrector ortográfico

Código idéntico al de Rasmus Bååth

## Usage

``` r
rec_ortografia(word, sorted_words = desuctools::corpus_rae$Orden[1:5000])
```

## Source

<https://nubededatos.blogspot.com/2015/01/corrector-ortografico-en-espanol-para-r.html>

## Arguments

- word:

  chr Palabra a corregir

- sorted_words:

  Lista de palabras de referencia ordenadas por frecuencia. Por defecto
  usa lista de CRAE

## Value

character

## Examples

``` r
rec_ortografia(c('pais', 'arbol'))
#> [1] "país"  "árbol"
```
