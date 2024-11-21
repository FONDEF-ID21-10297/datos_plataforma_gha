# Packages ---------------------------------------------------------------
cli::cli_h1("Packages")
library(earthdatalogin)
library(gdalcubes)
library(sf)
library(glue)
library(fs)
library(purrr)
library(rstac)
library(lubridate)
library(stringr)
library(terra)
library(dplyr)

# Parameters --------------------------------------------------------------
cli::cli_h1("Parameters")

edl_netrc(
  username = Sys.getenv("USERNAME"),
  password = Sys.getenv("PASSWORD")
  )

with_gdalcubes()

url <- "https://planetarycomputer.microsoft.com/api/stac/v1"

start <- as.Date(now() - days(15))
start <- as.Date(now() - days(15 * 2))

end <- as.Date(now() + days(5))

sites <- fs::dir_ls("data/vectorial/", regexp = "gpkg") |>
  basename() |>
  tools::file_path_sans_ext()


# Download rasters --------------------------------------------------------
cli::cli_h1("Download rasters")

purrr::walk(sites, function(site = "la_esperanza"){
  
  cli::cli_h2(site)
  
  cli::cli_inform("read_sf")
  
  pol <- sf::read_sf(glue("data/vectorial/original/{site}.gpkg"), layer = "cuartel")
  
  bb <- st_bbox(pol) |>
    as.numeric()
  
  cli::cli_inform("sentinel")
  
  items <-  stac(url) |>
    stac_search(
      collections = "sentinel-2-l2a",
      bbox = bb,
      datetime = paste(start, end, sep = "/")
    ) |>
    post_request() |>
    items_sign(sign_fn = sign_planetary_computer()) |>
    items_fetch()
  
  bb <- pol |>
    st_transform(32719) |>
    st_bbox() |>
    as.numeric()
  
  cli::cli_inform("cube_view")
  
  v <- cube_view(
    srs = "EPSG:32719",
    extent = list(
      t0 = as.character(start),
      t1 = as.character(end),
      left = bb[1],
      right = bb[3],
      top = bb[4],
      bottom = bb[2]
      ),
    dx = 10, 
    dy = 10,
    dt = "P5D"
    )
  
  col <- stac_image_collection(items$features)
  
  cloud_mask <- image_mask("SCL", values = c(3, 8, 9))
  
  cli::cli_inform("write_tif raster_cube")
  # debiese ser data temporal pues cada archivo pesa 9 MB por día en la_esperanza
  dir_out <- glue("outputs/sentinel/{site}/")
  
  if(fs::dir_exists(dir_out)) fs::dir_delete(dir_out)
  
  fs::dir_create(dir_out)
  
  raster_cube(col, v, mask = cloud_mask) |>
    write_tif(dir_out)
  
})


#  Indices ----------------------------------------------------------------
cli::cli_h1("Indices")

purrr::walk(sites, function(site = "rio_claro"){
  
  cli::cli_h2(site)
  
  files <- fs::dir_ls(glue("outputs/sentinel/{site}/"))
  
  # fechas <- gsub('_', '-', substr(files, nchar(files) - 13, nchar(files) - 4))
  fechas <- files |> 
    str_extract("(\\d{4}-\\d{2}-\\d{2})\\.tif$") |> 
    tools::file_path_sans_ext()
  
  band_names <- files |> 
    dplyr::first() |> 
    terra::rast() |> 
    names()
  
  band_names <- band_names[2:12]
  
  dfechas_id <- tidyr::crossing(sitio = site, fecha = fechas) |>
    # group_by(sitio) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::distinct(sitio, fecha, .keep_all = T) |>
    # filter(en.temporada(fecha, temporada)) |>
    dplyr::arrange(sitio, fecha)
  
  raster <- rast(files)
  
  raster <- lapply(band_names, function(x = "B01") {
    cli::cli_inform("band: {x}")
    band <- subset(raster, which(names(raster) == x))
    names(band) <- substr(sources(band), nchar(sources(band)) - 13, nchar(sources(band)) - 4)
    band[[sort(names(band))]] / 10000
  })
  
  names(raster) <- as.numeric(str_remove(band_names, "B"))
  
  dir_out <- fs::dir_create(glue("outputs/indices_raw/{site}/"))
  
  if(fs::dir_exists(dir_out)) fs::dir_delete(dir_out)
  
  fs::dir_create(dir_out)
  
  writeRaster(
    (raster$`8`-raster$`11`)/(raster$`8`+raster$`11`),
    glue("{dir_out}/NDMI.tif"),
    overwrite = TRUE
    )
  
  writeRaster(
    raster$`11`/raster$`8`,
    glue("{dir_out}/MSI.tif"),
    overwrite = TRUE
  )
  
  writeRaster(
    (raster$`8` - raster$`11` + raster$`12`) / (raster$`8` + raster$`11`-raster$`12`),
    glue("{dir_out}/NMDI.tif"),
    overwrite = TRUE
  )
  
  writeRaster(
    (raster$`8` + raster$`3`)/(raster$`11`+raster$`4`),
    glue("{dir_out}/DWSI.tif"),
    overwrite = TRUE
  )
  
  writeRaster(
    ((raster$`6` / raster$`5`) - 1)/sqrt((raster$`6`/raster$`5`)+1),
    glue("{dir_out}/msr705.tif"),
    overwrite = TRUE
  )
  
})

# Smoothing ---------------------------------------------------------------
cli::cli_h1("Smoothing")

purrr::walk(sites, function(site){
  
  cli::cli_h2(site)
  
})

