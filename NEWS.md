* Función para bajar y leer bases spss desde Alchemer.

# desuctools 0.1.5.9006

* Aumentar la cantidad de encuestas que descarga la función `sg_get`.

# desuctools 0.1.5.9005

* Actualizar función `sg_get` para lidiar con errores.

# desuctools 0.1.5.9004

* Actualizar función `sg_get` con el paquete `httr2`. 
Tiene sencillas maneras de controlar la cantidad de request por segundo y re--intentos.
* Remover dependencia de `httr`.
* Actualización de funciones para eliminar advertencias y errores.

# desuctools 0.1.5.9003

* Agregar pausa a llamados a API de SurveyToGo `sg_get`.
* Subir requerimientos de paquetes.

# desuctools 0.1.5.9002

* Cambio de caracteres en el nombre de regiones en función `region_orden`.


# desuctools 0.1.5.9001

* Added a `NEWS.md` file to track changes to the package.


## New functions

* Función `is_email` para validar sintaxis de correos electrónicos.
* Función `svy_tabla_vars_segmentos` para obtener estadísticos de multiples variables y segmentos.
* Función `fct_case_when` a partir de @pewmethods.
* función `rec_ortografia` para corregir ortografía de palabras.
* función `format_num` para impresión de número con separador de miles.
* función `region_orden` para pasar de regiones como números a factor ordenadoh de norte a sur.
* función `sg_get` para rescatar información de SurveyToGo desde su 
           [REST API](https://support.dooblo.net/hc/en-us/articles/208294645-How-To-Use-The-SurveyToGo-REST-API).
* función `varnames_to_label` para simplificar nombre de bases de datos obtenidas desde
      servicios web.
           


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
* Corregir test para missings en `tabla_vars_segmentos`.
* Actualizar testthat a 3a edición.
