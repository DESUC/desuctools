# Clasificación NSE AIM 2023

Clasifica el nivel socioeconómico (GSE) según la actualización AIM 2023,
usando: - edu_jh: educación jefe(a) de hogar (códigos edu_jh: 1–5) -
ocu_jh: ocupación jefe(a) de hogar (códigos ocu_jh: 1–6) - residentes:
número de personas residentes en el hogar (\>=1) - tramo_ingreso: Tramos
de ingreso autónomo per cápita del hogar (1–7, 99 = NS/NR)

Devuelve un vector de caracteres con el GSE: E, D, C3, C2, C1b, C1a, AB.

Preguntas:

Educación

\`edu_jh\`: ¿Cuál es el nivel educacional alcanzado (último año
aprobado) por el principal sostenedor del hogar? 1 a 1 Sin estudios
formales 2 a 1 Básica incompleta / primaria o preparatoria incompleta 3
a 2 Básica completa / primaria o preparatoria completa 4 a 3 Media
científico humanista o media técnico profesional incompleta /
humanidades incompleta 5 a 3 Media científico humanista o media técnico
profesional completa / humanidades completa 6 a 4 Instituto técnico
(CFT) o instituto profesional incompleto (carreras 1 a 3 años) 7 a 4
Instituto técnico (CFT) o instituto profesional completo (carreras 1 a 3
años) / hasta suboficial de FF.AA./Carabineros 8 a 4 Universitaria
incompleta (carreras de 4 o más años) 9 a 5 Universitaria completa
(carreras de 4 o más años) / oficial de FF.AA./Carabineros 10 a 5
Postgrado (Postítulo, Master, Magister, Doctor)

\`ocu_jh\` ¿Cuál de las siguientes ocupaciones corresponde al trabajo
del principal sostenedor del hogar? (SI EL PRINCIPAL SOSTENEDOR DEL
HOGAR ESTÁ CESANTE O ES JUBILADO, PREGUNTAR POR LA ÚLTIMA OCUPACIÓN
REMUNERADA QUE TUVO) (SI EL PRINCIPAL SOSTENEDOR TIENE MÁS DE 1 TRABAJO,
DEBE REGISTRARSE EL DE MAYOR INGRESO) 1 a 1 Trabajadores no calificados
en ventas y servicios, peones agropecuarios, forestales, construcción,
etc. 2 a 2 Obreros, operarios y artesanos de artes mecánicas y de otros
oficios 3 a 3 Trabajadores de los servicios y vendedores de comercio y
mercados 4 a 3 Agricultores y trabajadores calificados agropecuarios y
pesqueros 5 a 3 Operadores de instalaciones y máquinas y montadores /
conductores de vehículos 6 a 3 Otros grupos no identificados (incluye
rentistas, incapacitados, etc.) 7 a 4 Empleados de oficina públicos y
privados 8 a 5 Técnicos y profesionales de nivel medio (incluye hasta
suboficiales FF.AA./Carabineros) Alto ejecutivo (gerente general o
gerente de área o sector) de empresa privadas o públicas. Director o
dueño de grandes empresas. Alto directivo del poder ejecutivo, de los
cuerpos legislativos y de la administración pública (incluye oficiales
de FF.AA./carabineros) 9 a 6 Profesionales, científicos e intelectuales

## Usage

``` r
rec_nse_aim2023(.edu_jh, .ocu_jh, .tramo_ingreso, factor = FALSE)
```

## Source

https://aimchile.cl/wp-content/uploads/2025/06/Actualizacion-y-Manual-GSE-AIM-2023.pdf

## Arguments

- edu_jh:

  Integer. Vector de códigos de educación (P1 AIM: 1–5).

- ocu_jh:

  Integer. Vector de códigos de ocupación (P2 AIM: 1–6).

- residentes:

  Integer. Vector de número de residentes en el hogar.

- tramo_ingreso:

  Integer. Vector de tramo de ingreso (1–7, 99 = NS/NR).

## Value

labelled. Vector con la clasificación GSE.

## Examples

``` r
# personas <- data.frame(
#   edu_jh        = c(3L, 5L, 2L, 4L),
#   ocu_jh        = c(1L, 6L, 4L, 3L),
#   residentes    = c(4L, 5L, 2L, 9L),
#   tramo_ingreso = c(4L, 7L, 3L, 2L)
# )
# personas$gse <- rec_nse_aim2023(
#   edu_jh        = personas$edu_jh,
#   ocu_jh        = personas$ocu_jh,
#   residentes    = personas$residentes,
#   tramo_ingreso = personas$tramo_ingreso
# )
```
