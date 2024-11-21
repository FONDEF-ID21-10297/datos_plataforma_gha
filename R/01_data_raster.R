# packages ---------------------------------------------------------------
library(earthdatalogin)
library(gdalcubes)
library(sf)
library(glue)

# raster -----------------------------------------------------------------
edl_netrc(username = Sys.getenv("USERNAME"), password = Sys.getenv("PASSWORD"))

with_gdalcubes()

read_sf(glue("input_data/shp/{site}.gpkg"), layer = "cuartel")
