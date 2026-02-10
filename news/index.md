# Changelog

## desuctools 0.2.0.9002

- Correcciones para pasar R CMD check.

## desuctools 0.2.0.9001

- [`tabla_vars_segmentos()`](../reference/tabla_vars_segmentos.md) queda
  como punto de entrada principal con despacho S3.
- Nuevos métodos S3 para `tabla_vars_segmentos`: `data.frame`, `tbl_svy`
  y `survey.design2`.
- [`svy_tabla_vars_segmentos()`](../reference/svy_tabla_vars_segmentos.md)
  se mantiene por compatibilidad y ahora delega en
  [`tabla_vars_segmentos()`](../reference/tabla_vars_segmentos.md).
- Se corrige bug en
  [`svy_tabla_vars_segmentos()`](../reference/svy_tabla_vars_segmentos.md)
  por validación de clase (`inherits(.df, "tbl_svy")`).
- Selección de variables y segmentos con tidyselect en todos los métodos
  de `tabla_vars_segmentos`
  ([`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html),
  `c(...)`) y compatibilidad con estilo previo `vars(...)`.
- Lógica explícita por tipo de variable en `.vars`:
  - Categóricas (incluye `haven_labelled` con labels): cálculo de `prop`
    y `prop_val`.
  - Numéricas: salida con `pregunta_cat == "mean"` y cálculo de `mean`
    (excluyendo `NA` y `miss`).
- En métodos de encuesta, variables numéricas ahora soportan errores de
  estimación para media vía `vartype` (`ci`, `se`).
- Ajustes de documentación (`roxygen`/`NAMESPACE`/`.Rd`) para nuevos
  métodos, argumentos y reglas de cálculo.
- Actualización y ampliación de tests en tablas de output y survey
  (despacho S3, tidyselect, `prop`/`prop_val`, `mean`).
- Nueva función para obtener información a partir de un rut:
  `rut_información`.

## desuctools 0.1.5.9010

- Creación de función `rec_nse_aim2023` para clasificar a personas según
  criterio de AIM 2023. Reemplaza antigua función `calculo_nse` por
  estár desactualizada.

## desuctools 0.1.5.9009

- Se agrega la función `sg_date` para formatear fechas según los
  requerimientos de la API de SurveyToGo.
- Se agregan funciones para codificar a factores variables habituales en
  bases de datos del [Ministerio de
  Educación](https://centroestudios.mineduc.cl/datos-abiertos/)

## desuctools 0.1.5.9008

- Se agrega la función `edad_rut` para calcular la edad utilizando el
  rut.

## desuctools 0.1.5.9007

- Se agrega la función `rec_labelled` para facilitar la recodificación
  de variables en formato labelled.
- Función para bajar y leer bases spss desde Alchemer.

## desuctools 0.1.5.9006

- Aumentar la cantidad de encuestas que descarga la función `sg_get`.

## desuctools 0.1.5.9005

- Actualizar función `sg_get` para lidiar con errores.

## desuctools 0.1.5.9004

- Actualizar función `sg_get` con el paquete `httr2`. Tiene sencillas
  maneras de controlar la cantidad de request por segundo y re–intentos.
- Remover dependencia de `httr`.
- Actualización de funciones para eliminar advertencias y errores.

## desuctools 0.1.5.9003

- Agregar pausa a llamados a API de SurveyToGo `sg_get`.
- Subir requerimientos de paquetes.

## desuctools 0.1.5.9002

- Cambio de caracteres en el nombre de regiones en función
  `region_orden`.

## desuctools 0.1.5.9001

- Added a `NEWS.md` file to track changes to the package.

### New functions

- Función `is_email` para validar sintaxis de correos electrónicos.
- Función `svy_tabla_vars_segmentos` para obtener estadísticos de
  multiples variables y segmentos.
- Función `fct_case_when` a partir de
  [@pewmethods](https://github.com/pewmethods).
- función `rec_ortografia` para corregir ortografía de palabras.
- función `format_num` para impresión de número con separador de miles.
- función `region_orden` para pasar de regiones como números a factor
  ordenadoh de norte a sur.
- función `sg_get` para rescatar información de SurveyToGo desde su
  [REST
  API](https://support.dooblo.net/hc/en-us/articles/208294645-How-To-Use-The-SurveyToGo-REST-API).
- función `varnames_to_label` para simplificar nombre de bases de datos
  obtenidas desde servicios web.

### Features

- Agregar columna `comuna18` para tener el código comunal luego de la
  creación de la región del Ñuble en 2018.
- Nuevas variables en `regiones_y_comunas` para ajustes de no respuesta.
- Base de datos con `codigos_ensenanza` para tener equivalencias y
  descripción de la nomenclatura utilizada por MINEDUC en sus bases de
  datos.

### Gráfico

- Función `gg_bar_3_niveles_stack`.

### Bug fix

- Correccion de bug por `get_label`.
- En `tabla_vars_segmentos` si una variable no tiene etiquetas, deja
  pregunta_lab en blanco.
- Corrección y simplificación de `shift_missing` para que mantenga
  atributos de variables de clase `haven_labelled`.
- Corregir test para missings en `tabla_vars_segmentos`.
- Actualizar testthat a 3a edición.
