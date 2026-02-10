# Datos de regiones y comunas de Chile

Tabla que contiene nombres y códigos de regiones y comunas de Chile,
además de algunas segmentación es utilizadas habitualmente.

## Usage

``` r
regiones_y_comunas
```

## Format

Un data.frame con 346 filas `comunas` y 19 variables.

## Source

<http://www.ine.cl/>

## Details

- `id`: orden de comunas siguiendo geografía y código comunal

- `region_orden`: numeración de regiones de norte a sur

- `comuna`: código de comuna

- `comuna18`: código de comuna luego de la creación de la región de
  Ñuble en 2018

- `region`: número de región

- `provincia`: código de provincia luego de la creación de la región de
  Ñuble

- `region_nom`: nombre oficial de la región

- `region_nom_small`: nombre breve de la región

- `comuna_nom`: nombre de la comuna

- `region_capital`: TRUE si comuna es capital regional

- `comuna_extrema`: 1 si comuna es extrema para INE, 2 si es extrema
  para DESUC

- `gran_santiago`: TRUE si comuna pertenece al Gran Santiago

- `gran_santiago_zonas`: nombre de zonas del Gran Santiago

- `educ_sup_com`: \\

- `hacinamiento`: \\

- `allegamiento_int`: \\

- `carente_ser_bas`: \\

- `carente_entorno`: \\

- `decil`: promedio de decir de ingresos de los hogares
