# desuctools 0.1.5.9000

* Added a `NEWS.md` file to track changes to the package.

## New functions

* Función `is_email` para validar sintaxis de correos electrónicos.
* Función `svy_tabla_vars_segmentos` para obtener estadísticos de multiples variables y segmentos.
* Función `fct_case_when` a partir de @pewmethods.

## Features

* Agregar columna `comuna18` para tener el código comunal luego de la creación de la región del Ñuble en 2018.
* Nuevas variables en `regiones_y_comunas` para ajustes de no respuesta.
* Base de datos con `codigos_ensenanza` para tener equivalencias y descripción de la nomenclatura utilizada por MINEDUC en sus bases de datos.

### Gráfico

* Función `gg_bar_3_niveles_stack`.


## Bug fix

* Correccion de bug por `get_label`.
* En `tabla_vars_segmentos` si una variable no tiene etiquetas, deja pregunta_lab en blanco.
* Corrección y simplificación de `shift_missing` para que mantenga atributos de variables de clase `haven_labelled`.
