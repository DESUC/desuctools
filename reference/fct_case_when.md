# Factor case_when

Wrapper around
[`dplyr::case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
that converts the output to a factor and preserves the order in which
value labels were passed into the function.

## Usage

``` r
fct_case_when(..., label = NULL)
```

## Arguments

- ...:

  A sequence of two-sided formulas consistent with
  [`dplyr::case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html).

- label:

  A character. It's the label of the created value.

## Value

The output of
[`dplyr::case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html),
as class `"factor"` and ordered however you wanted it.

## Details

Unlike case_when, fct_case_when allows factors to be passed in as
right-hand-side arguments - they are treated internally as characters,
but the resulting vector will preserve the order of the original factor
levels.

## Author

pewmethods

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
macrozona <- with(regiones_y_comunas,
                  fct_case_when(region %in% c(15, 1:4) ~ "Norte",
                                region %in% c(5:7, 16) ~ "Centro",
                                region %in% c(13) ~ "RM",
                                region %in% c(8:12, 14) ~ "Sur")
)

# Compare to normal case_when() and then factor(), which will arrange the levels in
# unwanted alphabetical order

macrozona <- with(regiones_y_comunas,
                  factor(case_when(region %in% c(15, 1:4) ~ "Norte",
                                   region %in% c(5:7, 16) ~ "Centro",
                                   region %in% c(13) ~ "RM",
                                   region %in% c(8:12, 14) ~ "Sur"))
)
```
