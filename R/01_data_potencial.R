# Packages ---------------------------------------------------------------
cli::cli_h1("Packages")
library(glue)
library(lubridate)
library(fs)
library(earthdatalogin)
library(gdalcubes)
library(sf)
library(purrr)
library(rstac)
library(stringr)
library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)
# library(agvAPI) # remotes::install_github('frzambra/agvAPI')
library(tidymodels)
library(terra)
library(readr)
library(agrometR) 
library(httr2)

# Parameters --------------------------------------------------------------
cli::cli_h1("Parameters")

edl_netrc(
  username = Sys.getenv("USERNAME"),
  password = Sys.getenv("PASSWORD")
  # username = "",
  # password = ""
  )

# api_url <- "https://agviewer.com/api/v1/device/data"
# usrpass <- c("agricolagarces", "6<9W=9S+tb:")

with_gdalcubes()

sites <- fs::dir_ls("data/vectorial/", regexp = "gpkg") |>
  basename() |>
  tools::file_path_sans_ext()

serials <- list(
  la_esperanza = c('00205018'),
  rio_claro = c('00203E6E')
  )

api_url <- "https://agviewer.com/api/v1/device/data"
usrpass <- c("agricolagarces", "6<9W=9S+tb:")

quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

# Funciones Abel ---------------------------------------------------------
calc_vpd <- \(temp, rh) {
  es <- 0.6108 * exp((17.27 * temp) / (temp + 237.3))
  ea <- es * (rh / 100)
  vpd <- es - ea
  return(vpd)
}

calc_ra <- \(fecha, sitio) {
  J <- as.numeric(format(as.Date(fecha), "%j"))
  
  latitudes <- list('rio_claro' = -34.7014,'la_esperanza' = -34.3266)
  
  if (!(sitio %in% names(latitudes))) {
    stop("Sitio inválido. Usa 'rio_claro' o 'la_esperanza'.")
  }
  
  lat <- latitudes[[sitio]]
  
  phi <- lat * pi / 180
  Gsc <- 0.0820
  dr <- 1 + 0.033 * cos((2 * pi / 365) * J)
  delta <- 0.409 * sin((2 * pi / 365) * J - 1.39)
  omega_s <- acos(-tan(phi) * tan(delta))
  
  Ra <- (24 * 60 / pi) * Gsc * dr * 
    (omega_s * sin(phi) * sin(delta) + cos(phi) * cos(delta) * sin(omega_s))
  
  return(Ra)
}

calc_et0 <- \(tmax, tmin, Ra) {
  
  Tmean <- (tmax + tmin) / 2

  ET0 <- 0.0023 * (Tmean + 17.8) * sqrt(tmax - tmin) * Ra
  return(ET0)
}

obtener_var <- \(sitio, inicio, final) {
  ema_id <- list('rio_claro' = 72, 'la_esperanza' = 71)

  if (!(sitio %in% names(ema_id))) {
    stop("Sitio inválido. Usa 'rio_claro' o 'la_esperanza'.")
  }

  id <- ema_id[[sitio]]

  df <- get_agro_data(
    stations_id = id,
    date_start = inicio,
    date_end = final
  )
  
  cli::cli_inform("obtener_var {sitio} {inicio} {final}")
  print(df)
  
  df <- df |>
    filter(as.Date(fecha_hora) < as.Date(now())) # LE BORRE EL ÚLTIMO DÍA YA QUE PUEDE QUE NO HAYAN DATOS COMPLETOS PARA TENER LA ET0

  df |>
    mutate(VPD = calc_vpd(temp_promedio_aire, humed_rel_promedio)) |>
    filter(between(hour(fecha_hora), 13, 14)) |>
    group_by(fecha = as.Date(fecha_hora)) |>
    reframe(
      Temperature = mean(temp_promedio_aire, na.rm = T),
      VPD = mean(VPD, na.rm = T),
      RH = mean(humed_rel_promedio, na.rm = T)
    ) |>
    left_join(
      df |>
        group_by(fecha = as.Date(fecha_hora)) |>
        reframe(
          tmin = min(temp_minima, na.rm = T),
          tmax = min(temp_maxima, na.rm = T)
        ) |>
        mutate(Ra = calc_ra(fecha, sitio), ETo = calc_et0(tmax, tmin, Ra)) |>
        select(-c(tmin, tmax, Ra))
    ) |>
    mutate(sitio = sitio, .before = fecha) |>
    suppressMessages()
}

# agvAPI -----------------------------------------------------------------
getDataAGV_clima <- function (station_id = "z6-14410", var = "Temperature", time_span = c("2022-07-20",  "2022-07-25")) {
    time_span <- lubridate::as_datetime(time_span)
    resp <- getdataAGV(station_id = station_id, time_span = time_span)
    type <- stringr::str_extract(station_id, "z6|[:alpha:]|[:digit:]+")
    if (is.numeric(type)) 
        out <- .parseAGV(resp, station_id, var)
    if (type == "z6") {
        out <- .parseAGV(resp, station_id, var, type = "z6")
    }
    else if (is.character(type)) 
        out <- .parseAGV(resp, station_id, var)
    stopifnot(`Does not correspond to any station type` = !is.na(type))
    return(out)
}

getdataAGV <- function (station_id, time_span) {
    resp <- tibble::as_tibble(as.data.frame(dplyr::pull(
      jsonlite::fromJSON(httr2::resp_body_string(httr2::req_perform(httr2::req_url_path_append(
        httr2::req_headers(
          httr2::req_auth_basic(httr2::request(api_url), usrpass[1], usrpass[2]),
          Accept = "application/json"
        ),
        glue::glue(
          "?stations={station_id}&start={time_span[1]}&end={time_span[2]}"
        )
      )))),
      sensors
    )))
}

.parseAGV <- function (resp, station_id, var, type = "generica") {
    if (type == "z6") {
        data_tim_mes <- tibble::as_tibble(as.data.frame(dplyr::pull(
          dplyr::filter(resp, grepl("Atmos|ATMOS", name)),
          data
        )))
        fun_sel <- function(d, v) {
          out <- dplyr::filter(d, grepl(v, description))
          stopifnot(
            `Variable name match more than one or none` = nrow(out) == 1
          )
          out
        }
        data_out <- purrr::map_df(
          dplyr::pull(data_tim_mes, measurements),
          fun_sel,
          {{ var }}
        )
        new_name <- dplyr::pull(
          dplyr::distinct(data_out, description),
          description
        )
        data_out <- tibble::as_tibble(dplyr::select(
          dplyr::mutate(
            purrr::set_names(dplyr::select(data_out, 2), new_name),
            datetime = lubridate::ymd_hm(data_tim_mes$timestamp)
          ),
          2:1
        ))
    }
    else {
        data_out <- dplyr::filter(resp, grepl(glue::glue("{var}|{tolower(var)}"), 
            name))
        stopifnot(`Variable name match more than one or none` = nrow(data_out) == 
            1)
        data_out <- purrr::map_df(dplyr::pull(tibble::as_tibble(as.data.frame(dplyr::pull(data_out, 
            data))), measurements), function(d) {
            tidyr::pivot_wider(d, names_from = description, values_from = value)
        })
        if (var == "Temperature" | var == "VPD" | stringr::str_detect(var, 
            "Relative humidity")) 
            data_out <- dplyr::select(dplyr::filter(data_out, 
                if_any(1, function(x) x == "avg")), 1:2)
        datetime <- dplyr::pull(tibble::as_tibble(as.data.frame(dplyr::pull(dplyr::filter(resp, 
            grepl(glue::glue("{var}|{tolower(var)}"), name)), 
            data))), timestamp)
        data_out <- dplyr::relocate(dplyr::mutate(dplyr::mutate(data_out, 
            datetime = datetime), datetime = lubridate::ymd_hm(datetime)), 
            datetime)
    }
    return(data_out)
}

# Functions ---------------------------------------------------------------
equipo_sector_a_sector_equipo <- function(x = c("a_b", "c_d")){
  x |> 
    str_split("_") |> 
    map_chr(function(l){
      str_c(l[2], "_", l[1])
    })
}

download_rasters_site_date <- function(site = "la_esperanza",
                                       end = today(),
                                       start = end - days(30)){
  
  # se le da la fecha de hoy y por defecto obtiene la fecha de hace un mes atrás
  # luego descarga los tif disponibles entre esas fechas
  
  cli::cli_h3("download_rasters_site_date: {site} {start} to {end}")
  
  cli::cli_inform("read_sf")
  
  pol <- sf::read_sf(glue("data/vectorial/original/{site}.gpkg"), layer = "cuartel")
  
  bb <- st_bbox(pol) |>
    as.numeric()
  
  cli::cli_inform("sentinel")
  
  items <-  stac("https://planetarycomputer.microsoft.com/api/stac/v1") |>
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
  
  v <- quiet(
    cube_view(
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
  
}

get_indices <-  function(site = "rio_claro"){
  
  cli::cli_h2("get_indices: {site}")
  
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
  
  cli::cli_inform("writeRaster")
  
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
    ((raster$`6` / raster$`5`) - 1)/sqrt((raster$`6`/raster$`5`) + 1),
    glue("{dir_out}/msr705.tif"),
    overwrite = TRUE
  )
  
}

smoothing_rasters <- function(site = "rio_claro", date = today()){
  
  # esta funcion obtiene suaviza  considerando las valores/fechas que 
  # hay en la carpeta, para luego quedarse con el valor de la fecha de
  # definida en argumento "date", la cual debiese coincidir con el valor 
  # del argumento "end" al ejectuar la funcion download_rasters_site_date
  # 
  # notar que al ejecutar retroactivamente existiran observaciones para 
  # fechas que el dato exista, ie, el valor suavizado sera igual al real.
  # 
  cli::cli_h3("smoothing_rasters {site} {date}")
  
  cli::cli_inform("rast")
  
  tif_files <- fs::dir_ls(glue("outputs/indices_raw/{site}/"))
  
  names <- tif_files |> 
    basename() |> 
    tools::file_path_sans_ext()
  
  tif_files_dia <- map2(tif_files, names, function(tif = "outputs/indices_raw/rio_claro/NDMI.tif", name = "NDMI"){
    
    cli::cli_inform(tif)
    
    r <- rast(tif)
    
    cli::cli_inform("número de capas (fechas) en {name}: { nlyr(r)}")
    
    fechas <-  as.Date(names(r))
    fechas_continuas <-  seq(min(fechas), date, by = '1 day')
    
    cli::cli_inform("suavizando rast")
    
    r_suavizado <- app(r, function(y){
      # y <- runif(length(fechas))
      d <- tibble(date = fechas_continuas) |> 
        left_join(tibble(date = fechas, y = y), by = join_by(date)) |> 
        mutate(y2 = imputeTS::na_interpolation(y, option = "spline"))
      pull(d, y2)
    })
    
    names(r_suavizado) <- fechas_continuas
    time(r_suavizado) <- fechas_continuas
    
    r_tif_dia <- r_suavizado[as.character(date)]
    names(r_tif_dia) <- name
    r_tif_dia
    
  })
  
  tif_files_dia <- rast(tif_files_dia)
  names(tif_files_dia) <- names
  
  cli::cli_inform("saving in /outputs/{site} date:{date}")
  
  dir_out <- fs::dir_create(glue("outputs/suavizado/{site}/"))
  
  if(!fs::dir_exists(dir_out)) fs::dir_create(dir_out)
  
  terra::writeRaster(
    tif_files_dia,
    glue::glue("{dir_out}/{date}.tif"),
    gdal = c("COMPRESS=DEFLATE", "GDAL_PAM_ENABLED=NO"), # baja de 180 a 142 (reduccion 20%)
    overwrite = TRUE
    )

}

# GHA debe descargar los datos con fecha de "ayer".
# Por defecto obtiene datos de ayer
get_climate <- function(site = "la_esperanza", date = today() - days(1)){
  
  cli::cli_h3("get_climate: {site} date {date}")

  dET0 <- getDataAGV_clima(serials[[site]][1], var = 'ETo', time_span = c(date - days(1), date + days(1)))
  dVPD <- getDataAGV_clima(serials[[site]][1], var = 'VPD', time_span = c(date - days(1), date + days(1)))
  dTmp <- getDataAGV_clima(serials[[site]][1], var = 'Temperature', time_span = c(date - days(1), date + days(1)))
  dHR  <- getDataAGV_clima(serials[[site]][1], var = 'HC Relative humidity', time_span = c(date - days(1), date + days(1)))
  
  dET0 <- dET0 |> 
    filter(as_date(datetime) == date)
  
  datas <- list(VPD = dVPD, Temperature = dTmp, HC = dHR) |> 
    map(mutate, datetime = as_datetime(datetime, tz = "America/Santiago")) |> 
    map(mutate, datetime = floor_date(datetime), hora = hour(datetime), date_registro = as_date(datetime)) |> 
    # me quedo con los del día de interés
    map(filter, date_registro == date) |>
    map(filter, between(hora, 13, 14)) |> 
    map(select, -c(datetime, extract_id, hora)) |> 
    map(group_by, date_registro) |> 
    map(summarise_all, function(x) mean(x,  na.rm = TRUE)) |> 
    map(select, -date_registro) |> 
    map(pull)
  
  # as.character(stringr::str_glue("{year(date)}-{str_pad(month(date), width = 2, pad = 0)}-{str_pad(day(date), width = 2, pad = 0)} 01:00:00"))
  # # inicio <- format(date, "%Y-%m%-%d 01:00:00")
  # # final  <- format(date, "%Y-%m%-%d 24:00:00")
  # inicio <- as.character(stringr::str_glue("{year(date)}-{str_pad(month(date), width = 2, pad = 0)}-{str_pad(day(date), width = 2, pad = 0)} 01:00:00"))
  # final <- as.character(stringr::str_glue("{year(date)}-{str_pad(month(date), width = 2, pad = 0)}-{str_pad(day(date), width = 2, pad = 0)} 24:00:00"))
  
  # datas <- obtener_var(site, inicio, final)
  # datas
   
  # esta parte requiere NO borrar todavias los tiffs
  raster <- fs::dir_ls(glue::glue("outputs/sentinel/{site}")) |> 
    head(1) |> 
    rast() 
  
  raster <- raster[[1]]
  
  raster_et0 <- raster
  values(raster_et0) <- dET0$`ETo[mm] (mm)` # datas$ETo 
  
  raster_vpd <- raster
  values(raster_vpd) <- datas$VPD

  raster_tmp <- raster
  values(raster_tmp) <- datas$Temperature

  raster_hc <- raster
  values(raster_hc) <- datas$HC
  
  raster_climate <- c(raster_et0, raster_vpd, raster_tmp, raster_hc) 
  
  names(raster_climate) <- c("eto", "vpd_medio", "t_media", "rh_media")
  varnames(raster_climate) <- ""
  
  if(interactive()) plot(raster_climate)

  cli::cli_inform("write_tif raster_climate")
  # debiese ser data temporal pues cada archivo pesa 9 MB por día en la_esperanza
  dir_out <- glue("outputs/climate/{site}")
  
  if(fs::dir_exists(dir_out)) fs::dir_delete(dir_out)
  if(!fs::dir_exists(dir_out)) fs::dir_create(dir_out)
  
  terra::writeRaster(
    raster_climate,
    glue::glue("{dir_out}/{date}.tif"),
    gdal = c("COMPRESS=DEFLATE", "GDAL_PAM_ENABLED=NO"), # baja de 180 a 142 (reduccion 20%)
    overwrite = TRUE
  )
  
}

make_prediction_and_save <- function(site = "la_esperanza", date = today()){
  
  cli::cli_h3("make_prediction_and_save: {site} date {date}")
  
  # https://github.com/mherreradiaz/garces/blob/probar_modelos/script/14_1_predecir_potencial_raster.R#L83-L96
  
  r_ind <- rast(str_glue("outputs/suavizado/{site}/{date}.tif"))
  
  r_cli <- rast(str_glue("outputs/climate/{site}/{date - day(1)}.tif"))
  
  r <- c(r_ind, r_cli)
  
  names(r) <- tolower(names(r))
  names(r)

  if(interactive()) plot(r)

  modelo <- readRDS("data/model/random_forest_tme_split.rds")
  
  r_df <- as.data.frame(r) |> 
    mutate(fecha = as.character(date))
  
  out <- predict(modelo, r_df)
  
  out_ras         <- r[[1]]
  names(out_ras)  <- '.pred'
  values(out_ras) <- round(out$.pred, 5)
  
  if(interactive()) plot(out_ras)
  
  # potencial raster --------------------------------------------------------
  cli::cli_inform("export potencial raster")

  fs::dir_create(glue("data/potencial-raster/{site}/"))
  fout <- glue("data/potencial-raster/{site}/{date}.tif")
  
  terra::writeRaster(
    out_ras,
    fout,
    gdal = c("COMPRESS=DEFLATE", "GDAL_PAM_ENABLED=NO"), # baja de 180 a 142 (reduccion 20%)
    overwrite = TRUE
  )
  
  # potencial csv -----------------------------------------------------------
  cli::cli_inform("export potencial csv")
  
  site_sf <- read_sf(glue("data/vectorial/{site}.gpkg"), layer = 'sectores_riego') |>
    st_transform(32719) |>
    mutate(id = row_number()) |> 
    mutate(equipo_sector = coalesce(equipo_sector, "1_6")) |> 
    mutate(sector_equipo = equipo_sector_a_sector_equipo(equipo_sector))
  
  site_sf
  
  if(interactive()) plot(site_sf)

  #variación temporal del potencial en los sectores de riego
  dpot_site <- terra::extract(out_ras, site_sf, fun = mean) |> 
    as_tibble() |> 
    pivot_longer(-ID, names_to = "date", values_to = "potencial") |>
    mutate(date = ymd(!!date), potencial = round(potencial, 4)) |> 
    rename(id = ID) |> 
    mutate(site = site, .before = 1)
  
  dpot_site
  
  if(interactive()){
    site_sf |>
      left_join(dpot_site, by = join_by(id)) |> 
      plot()
  }
  
  fs::dir_create("data/potencial-csv")
  file_pot <- "data/potencial-csv/potencial-sites.csv"
  
  if(file.exists(file_pot)) {
    dpot <- read_csv(file_pot)
  } else {
    dpot <- tibble()
  }
  
  dpot_site |> 
    bind_rows(dpot) |> 
    distinct(date, site, id, .keep_all = TRUE) |> 
    arrange(date, site, id) |> 
    write_csv(file_pot)

  # climate -----------------------------------------------------------------
  fs::dir_create(glue("data/climate"))
  
  cli::cli_inform("write_csv")
  file_cli <- "data/climate/climate-sites.csv"
  
  if(file.exists(file_cli)) {
    dcli <- read_csv(file_cli)
  } else {
    dcli <- tibble()
  }
  
  dcli |> group_by(site) |> summarise_all(.funs = median, na.rm =TRUE)

  r_cli |> 
    as_tibble() |> 
    # hacer mas simple 
    mutate(across(where(is.numeric), function(x) round(x, 3))) |> 
    distinct() |> 
    mutate(
      site = site,
      date = date,
      .before = 1
    ) |> 
    bind_rows(dcli) |> 
    distinct(date, site, .keep_all = TRUE) |> 
    arrange(date, site) |> 
    write_csv(file_cli)
  
}

# Process -----------------------------------------------------------------
cli::cli_h1("Process")

fecha_hoy <- today()

cli::cli_h2("Download rasters")
purrr::walk2(sites, rep(fecha_hoy, 2), download_rasters_site_date)

cli::cli_h2("Indices")
purrr::walk(sites, get_indices)

cli::cli_h2("Smoothing")
purrr::walk(sites, smoothing_rasters, date = fecha_hoy)

cli::cli_h2("Climate")
purrr::walk(sites, get_climate, date = fecha_hoy - days(1))

cli::cli_h2("make_prediction_and_save")
purrr::walk(sites, make_prediction_and_save, date = fecha_hoy)

# cli::cli_h2("Cleanup")
fs::dir_delete("outputs/")


# All dates ---------------------------------------------------------------
dates <- seq(ymd("20231001"), today() - 1, by = "1 day")
dates <- dates[!month(dates) %in% 5:9]

dates_dwloaded <- fs::dir_ls("data/potencial-raster/", recurse = TRUE) |>
  basename() |>
  tools::file_path_sans_ext() |>
  str_subset("[a-z]", negate = TRUE) |>
  unique() |>
  ymd()

dates <- setdiff(as.character(dates), as.character(dates_dwloaded))
dates <- ymd(dates)

dates

walk(dates, safely(function(date = sample(dates, 1)){

  cli::cli_h1(as.character(date))

  cli::cli_progress_step(as.character(date))
  
  cli::cli_h2("Cleanup")

  cli::cli_h2("Download rasters")
  purrr::walk2(sites, rep(date, 2), download_rasters_site_date)

  cli::cli_h2("Climate")
  purrr::walk(sites, get_climate, date = date - days(1))

  cli::cli_h2("Indices")
  purrr::walk(sites, get_indices)

  cli::cli_h2("Smoothing")
  purrr::walk(sites, smoothing_rasters, date = date)

  cli::cli_h2("make_prediction_and_save")
  purrr::walk(sites, make_prediction_and_save, date = date)

  cli::cli_h2("Cleanup")
  fs::dir_delete("outputs/")

}))
