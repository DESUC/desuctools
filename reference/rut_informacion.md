# Obtiene información para un RUT

Obtiene información personal a partir de un RUT de Chile.

## Usage

``` r
rut_informacion(.rut)
```

## Arguments

- .rut:

  string. rut sin puntos, con guión y dígito verificador.

## Value

Una lista con datos asociados al RUT.

## Examples

``` r
rut_list = c(
"8714763-0",
"4606477-1",
"20283632-1"
) # example list of RUT values

l_ruts_info <- lapply(rut_list, rut_informacion)
#> Información sobre:  8714763-0 
#> Información sobre:  4606477-1 
#> Información sobre:  20283632-1 

l_ruts_info |> str(2)
#> List of 3
#>  $ :List of 5
#>   ..$ timestamp: chr "2026-02-06T01:17:33.577+0000"
#>   ..$ status   : int 401
#>   ..$ error    : chr "Unauthorized"
#>   ..$ message  : chr "No message available"
#>   ..$ path     : chr "/v1/registro-civil/informacionPersona"
#>  $ :List of 5
#>   ..$ timestamp: chr "2026-02-06T01:17:33.757+0000"
#>   ..$ status   : int 401
#>   ..$ error    : chr "Unauthorized"
#>   ..$ message  : chr "No message available"
#>   ..$ path     : chr "/v1/registro-civil/informacionPersona"
#>  $ :List of 5
#>   ..$ timestamp: chr "2026-02-06T01:17:33.949+0000"
#>   ..$ status   : int 401
#>   ..$ error    : chr "Unauthorized"
#>   ..$ message  : chr "No message available"
#>   ..$ path     : chr "/v1/registro-civil/informacionPersona"
```
