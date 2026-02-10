# Invertir niveles

Función para revertir niveles. Especialmente útil si se quiere revertir
sólo un rango de valores de una pregunta.

## Usage

``` r
rev_niveles(x, ...)

# Default S3 method
rev_niveles(x, ...)

# S3 method for class 'factor'
rev_niveles(x, niveles_inv = NULL, ...)

# S3 method for class 'haven_labelled'
rev_niveles(x, rango_inv = NULL, ...)

# S3 method for class 'numeric'
rev_niveles(x, rango_inv = NULL, ...)
```

## Arguments

- x:

  `vector`: Vector de datos que se quiere invertir

- ...:

  placeholder.

- niveles_inv:

  `chr`: Niveles de un factor que serán invertidos. Por defecto,
  invierte todos los niveles.

- rango_inv:

  `num`: Vector de dos niveles en el que se señala el valor del primero
  y último valor que será invertido. Se espera que esos dos valores
  definan un rango. Por defecto, invierte todos los niveles.
