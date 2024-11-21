# packages ---------------------------------------------------------------
library(earthdatalogin)
library(gdalcubes)

# raster -----------------------------------------------------------------
edl_netrc(username = Sys.getenv("USERNAME"), password = Sys.getenv("PASSWORD"))

with_gdalcubes()
