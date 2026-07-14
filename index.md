# desuctools ![](reference/figures/desuctools.png)

Funciones y datos auxiliares para análisis de encuestas sociales
utilizados por la [Dirección de Estudios Sociales
UC](http://sociologia.uc.cl/desuc) (DESUC).

## Instalación

Para instalar la versión en desarrollo de desuctools desde
[GitHub](https://github.com/desuc/desuctools) puedes utilizar el
siguiente código:

``` r

# install.packages("devtools")
devtools::install_github("desuc/desuctools")
```

## Ejemplos

### Datos de región y comuna

Base de datos con información sobre regiones y comunas.

``` r

# Tabla con comunas capitales regionales.
desuctools::regiones_y_comunas |> 
  filter(region_capital) |> 
  arrange(region_orden) |> 
  select(region, comuna, comuna_nom) |> 
  desuctools::kable_desuc(align = 'rrl', font_size = 9)
```

| region | comuna | comuna_nom   |
|-------:|-------:|:-------------|
|     15 | 15.101 | Arica        |
|      1 |  1.101 | Iquique      |
|      2 |  2.101 | Antofagasta  |
|      3 |  3.101 | Copiapó      |
|      4 |  4.101 | La Serena    |
|      5 |  5.101 | Valparaíso   |
|      6 |  6.101 | Rancagua     |
|      7 |  7.101 | Talca        |
|     16 |  8.401 | Chillán      |
|      8 |  8.101 | Concepción   |
|      9 |  9.101 | Temuco       |
|     14 | 14.101 | Valdivia     |
|     10 | 10.101 | Puerto Montt |
|     11 | 11.101 | Coyhaique    |
|     12 | 12.101 | Punta Arenas |

### Recodificación de variables

``` r

# Cargar base de encuesta Bicentenario
file <- tempfile()
download.file(url = 'https://github.com/DESUC/30diasdegraficos/raw/master/inputs/12-lollipop-df_bicen_19_30diasdegraficos_2020.rds',
              destfile = file)

data <- readRDS(file)

head(data)
#> # A tibble: 6 × 15
#>    folio d07     t01_1   t01_2   t01_3   t02_1   t02_2   t03_1   t03_2   t03_3  
#>    <dbl> <dbl+l> <dbl+l> <dbl+l> <dbl+l> <dbl+l> <dbl+l> <dbl+l> <dbl+l> <dbl+l>
#> 1 100101 2 [Muj… 2 [Bas… 6 [No … 1 [Muc… 2 [Bas… 3 [Alg… 1 [Muc… 1 [Muc… 1 [Muc…
#> 2 100102 2 [Muj… 1 [Muc… 1 [Muc… 1 [Muc… 3 [Alg… 5 [Nad… 1 [Muc… 1 [Muc… 1 [Muc…
#> 3 100103 1 [Hom… 1 [Muc… 2 [Bas… 1 [Muc… 1 [Muc… 1 [Muc… 1 [Muc… 5 [Nad… 2 [Bas…
#> 4 100104 2 [Muj… 1 [Muc… 1 [Muc… 1 [Muc… 4 [Poc… 4 [Poc… 4 [Poc… 4 [Poc… 3 [Alg…
#> 5 100105 2 [Muj… 1 [Muc… 2 [Bas… 1 [Muc… 5 [Nad… 3 [Alg… 4 [Poc… 1 [Muc… 2 [Bas…
#> 6 100106 2 [Muj… 1 [Muc… 1 [Muc… 1 [Muc… 1 [Muc… 5 [Nad… 1 [Muc… 4 [Poc… 4 [Poc…
#> # ℹ 5 more variables: t03_4 <dbl+lbl>, t04_1 <dbl+lbl>, t04_2 <dbl+lbl>,
#> #   t04_3 <dbl+lbl>, pond_se <dbl>
```

La función [`desuctools::rec_cat_5a3`](reference/rec_cat_5a3.md)
recodifica preguntas likert de 5 a 3 categorías, entregando la opción de
agregar etiquetas a ellas.

``` r

data <- data |> 
  mutate(across(c(t01_1:t01_2), 
                ~desuctools::rec_cat_5a3(., labels = c('Bastante' = 1, 
                                                       'Algo' = 2, 
                                                       'Poco' = 3, 
                                                       'NA/NR' = 9))))

data |> head()
#> # A tibble: 6 × 15
#>    folio d07     t01_1   t01_2   t01_3   t02_1   t02_2   t03_1   t03_2   t03_3  
#>    <dbl> <dbl+l> <dbl+l> <dbl+l> <dbl+l> <dbl+l> <dbl+l> <dbl+l> <dbl+l> <dbl+l>
#> 1 100101 2 [Muj… 1 [Bas… 9 [NA/… 1 [Muc… 2 [Bas… 3 [Alg… 1 [Muc… 1 [Muc… 1 [Muc…
#> 2 100102 2 [Muj… 1 [Bas… 1 [Bas… 1 [Muc… 3 [Alg… 5 [Nad… 1 [Muc… 1 [Muc… 1 [Muc…
#> 3 100103 1 [Hom… 1 [Bas… 1 [Bas… 1 [Muc… 1 [Muc… 1 [Muc… 1 [Muc… 5 [Nad… 2 [Bas…
#> 4 100104 2 [Muj… 1 [Bas… 1 [Bas… 1 [Muc… 4 [Poc… 4 [Poc… 4 [Poc… 4 [Poc… 3 [Alg…
#> 5 100105 2 [Muj… 1 [Bas… 1 [Bas… 1 [Muc… 5 [Nad… 3 [Alg… 4 [Poc… 1 [Muc… 2 [Bas…
#> 6 100106 2 [Muj… 1 [Bas… 1 [Bas… 1 [Muc… 1 [Muc… 5 [Nad… 1 [Muc… 4 [Poc… 4 [Poc…
#> # ℹ 5 more variables: t03_4 <dbl+lbl>, t04_1 <dbl+lbl>, t04_2 <dbl+lbl>,
#> #   t04_3 <dbl+lbl>, pond_se <dbl>
```

### Tablas de resultados

Función `tabla_vars_segmentos`, la cual permite generar un `data.frame`
*tidy* con la cantidad y proporción de respuestas para un número
arbitrario de preguntas y un número arbitrario de segmentos de la
población.

``` r

data_tidy <- data |> 
  desuctools::tabla_vars_segmentos(
                  total       = TRUE, # Incluye dato total 
                  .vars       = c(t01_1, t01_2), # Listado de variables de interés
                  .segmentos  = d07, # Listado de segmentos de interés
                  .wt         = pond_se) # Ponderador

data_tidy |>
  glimpse()
#> Rows: 24
#> Columns: 8
#> $ segmento_var <chr> "d07", "d07", "d07", "d07", "d07", "d07", "d07", "d07", "…
#> $ segmento_lab <chr> "Registrar Sexo", "Registrar Sexo", "Registrar Sexo", "Re…
#> $ segmento_cat <fct> Hombre, Hombre, Hombre, Hombre, Mujer, Mujer, Mujer, Muje…
#> $ pregunta_var <chr> "t01_1", "t01_1", "t01_1", "t01_1", "t01_1", "t01_1", "t0…
#> $ pregunta_lab <chr> "(No tener suficiente dinero para afrontar la vejez) ¿Cuá…
#> $ pregunta_cat <fct> Bastante, Algo, Poco, NA/NR, Bastante, Algo, Poco, NA/NR,…
#> $ casos        <dbl> 666.867524, 169.146704, 148.294141, 3.352725, 818.345858,…
#> $ prop         <dbl> 0.675198738, 0.171259863, 0.150146788, 0.003394611, 0.772…
```

La misma función acepta diseños complejos de encuesta (`tbl_svy` de
`srvyr` o `survey.design2` de `survey`), agregando intervalos de
confianza y diferencias significativas entre categorías.

``` r

enc <- srvyr::as_survey_design(data, weights = pond_se)

enc |>
  desuctools::tabla_vars_segmentos(
    .vars = t01_1,
    .segmentos = d07,
    miss = 9) |> # Código de respuesta a excluir del cálculo de prop_val
  glimpse()
#> Rows: 8
#> Columns: 16
#> $ segmento_var <chr> "d07", "d07", "d07", "d07", "d07", "d07", "d07", "d07"
#> $ segmento_lab <chr> "Registrar Sexo", "Registrar Sexo", "Registrar Sexo", "Re…
#> $ segmento_cat <fct> Hombre, Hombre, Hombre, Hombre, Mujer, Mujer, Mujer, Mujer
#> $ pregunta_var <chr> "t01_1", "t01_1", "t01_1", "t01_1", "t01_1", "t01_1", "t0…
#> $ pregunta_lab <chr> "(No tener suficiente dinero para afrontar la vejez) ¿Cuá…
#> $ pregunta_cat <fct> Bastante, Algo, Poco, NA/NR, Bastante, Algo, Poco, NA/NR
#> $ casos_unwt   <int> 511, 116, 120, 5, 987, 144, 155, 9
#> $ casos        <dbl> 666.867524, 169.146704, 148.294141, 3.352725, 818.345858,…
#> $ casos_se     <dbl> 40.999379, 24.217981, 19.791840, 1.757321, 33.613718, 15.…
#> $ prop         <dbl> 0.675198738, 0.171259863, 0.150146788, 0.003394611, 0.772…
#> $ prop_low     <dbl> 0.622313434, 0.132017935, 0.116772600, 0.001209209, 0.736…
#> $ prop_upp     <dbl> 0.72396351, 0.21922030, 0.19099679, 0.00949215, 0.8045073…
#> $ prop_se      <dbl> 0.025990056, 0.022182561, 0.018862927, 0.001784441, 0.017…
#> $ casos_val    <dbl> 984.3084, 984.3084, 984.3084, 984.3084, 1053.4241, 1053.4…
#> $ prop_val     <dbl> 0.6774986, 0.1718432, 0.1506582, NA, 0.7768437, 0.1118400…
#> $ diff_sig     <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
```

### Manejo de etiquetas

Extraer el texto del ítem de una pregunta en matrices de preguntas
likert.

``` r

data_tidy |> 
  mutate(pregunta_lab_short = desuctools::str_entre_parentesis(pregunta_lab),
         .keep = 'used', .before = 1) |> 
  distinct() |> 
  kableExtra::kable()
```

| pregunta_lab_short | pregunta_lab |
|:---|:---|
| No tener suficiente dinero para afrontar la vejez | (No tener suficiente dinero para afrontar la vejez) ¿Cuánto temor le producen las siguientes situaciones? @\_@ITERNAME@\_@ |
| Ser despedido/a de su trabajo | (Ser despedido/a de su trabajo) ¿Cuánto temor le producen las siguientes situaciones? @\_@ITERNAME@\_@ |

``` r

data_tidy <- data_tidy |> 
  mutate(pregunta_lab = desuctools::str_entre_parentesis(pregunta_lab))
```

### Gráficos

Función
[`desuctools::gg_bar_3_niveles_stack`](reference/gg_bar_3_niveles_stack.md)
para crear gráfico que compare categorías positivas, negativas y
neutras.

``` r

desuctools::gg_bar_3_niveles_stack(
  .df         = data_tidy,
  x           = segmento_cat, 
  facet_col   = pregunta_lab, 
  missing     = 'NA/NR', y_na = 1.1,  x_na = -2.5,
  title       = '¿Cuánto temor le producen las siguientes situaciones?',
  font_family = '')
```

![](reference/figures/grafico-1.png)
