library(fs)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(tidyverse)
library(agvAPI)
library(terra)
library(earthdatalogin)
library(sf)
library(glue)
library(rstac)
library(gdalcubes)
library(mgcv)
library(zoo)
clima <- function(id_estacion, var, periodo) {
  
  for (x in 1:length(var)) {
    var_x <- getDataAGV_clima(station_id =id_estacion, var = var[x],
                              time_span = periodo) |> 
      mutate(datetime = as_datetime(datetime,tz = 'America/Santiago'))
    if (x == 1) {var_df <- var_x}
    else {var_df <- var_df |> 
      left_join(var_x, by = 'datetime')}
  }
  
  var_df <- var_df |> 
    group_by(datetime = floor_date(datetime,'30 min')) |> 
    summarise(t = mean(`avg (°C)`,na.rm=T),
              vpd = mean(`avg (mbar)`,na.rm=T),
              eto = sum(`ETo[mm] (mm)`,na.rm=T),
              pp = sum(`sum (mm)`,na.rm=T),
              rh = mean(`avg (%)`,na.rm=T))
  
}
fill_edges <- function(y) {
  y <- as.numeric(y)
  data <- data.frame(x = seq_along(y), y = y)
  l <- last(seq_along(y))
  avg <- mean(y,na.rm=T)
  modelo <- lm(data = data,y ~ x)
  m <- modelo$coefficients[2]
  i <- modelo$coefficients[1]
  y_predict <- (seq_along(y)*m+i+avg)/2
  id <- is.na(y) & seq_along(y) %in% c(1:2,(l-1):l)
  y[id] <- y_predict[id]
  return(y)
}
en.temporada <- function(fecha,temporada) {
  lapply(fecha, function(x) {
    año <- year(x)
    mes <- month(x)
    if (mes < 6) {año == substr(temporada,6,9)} else {
      año == substr(temporada,1,4)}
  }) |> 
    unlist()
} 

if (between(month(now()),5,9)) {
  print('FUERA DE TEMPORADA')
} else {
  
  temporada <- paste(ifelse(month(now())>5,year(now()),year(now())-1),
                     ifelse(month(now())>5,year(now())+1,year(now())),
                     sep = '-')
  
  start <- as.Date(now() - days(15))
  end <- as.Date(now() + days(5))
  
  # clima
  
  var <- c('Temperature','VPD','ETo','Precipitation','Relative humidity')
  
  data_le <- clima('00205018',var = var,
                   periodo = c(start,end)) |> 
    mutate(sitio = 'la_esperanza',.before=datetime)
  data_rc <- clima('00203E6E',var = var,
                   periodo = c(start,end)) |> 
    mutate(sitio = 'rio_claro',.before=datetime)
  
  data_clima <- bind_rows(data_rc,data_le) |> 
    mutate(datetime = with_tz(datetime, tzone = "America/Santiago"),
           fecha = as.Date(format(datetime, "%Y-%m-%d")),
           hora = hour(floor_date(datetime,'1 hour')),
           .before = t) |> 
    select(-datetime) |> 
    mutate(temporada = paste(ifelse(month(fecha)>5,year(fecha),year(fecha)-1),
                             ifelse(month(fecha)>5,year(fecha)+1,year(fecha)),
                             sep = '-'),
           .before = fecha)
  
  eto_pp <- data_clima |> 
    group_by(sitio,temporada,fecha) |> 
    reframe(eto = max(eto,na.rm=T),
            pp = sum(pp,na.rm=T))
  
  clima_dia <- data_clima |> 
    filter(hora %in% 13:14) |> 
    group_by(sitio,temporada,fecha) |> 
    reframe(t = mean(t,na.rm=T),
            rh = mean(rh,na.rm=T),
            vpd = mean(vpd,na.rm=T)) |> 
    left_join(eto_pp) |> 
    arrange(sitio,fecha)
  
  clima_update <- read_rds('output_data/clima/data_clima.rds') |> 
    bind_rows(clima_dia) |> 
    distinct(sitio,temporada,fecha, .keep_all = T)
  
  write_rds(clima_update,'output_data/clima/data_clima.rds')
  
  # descargar raster
  
  edl_netrc(username = 'asdf',password = '1234')
  with_gdalcubes()
  
  pol_rc <- read_sf(glue('input_data/shp/rio_claro.gpkg'),layer = 'cuartel')
  pol_le <- read_sf(glue('input_data/shp/la_esperanza.gpkg'),layer = 'cuartel')
  
  bb_rc <- st_bbox(pol_rc) |> 
    as.numeric()
  bb_le <- st_bbox(pol_le) |> 
    as.numeric()
  
  url <- "https://planetarycomputer.microsoft.com/api/stac/v1"
  
  items_rc <- stac(url) |> 
    stac_search(collections = "sentinel-2-l2a",
                bbox = bb_rc,
                datetime = paste(start,end, sep = "/")) |>
    post_request() |>
    items_sign(sign_fn = sign_planetary_computer()) |> 
    items_fetch()
  items_le <- stac(url) |> 
    stac_search(collections = "sentinel-2-l2a",
                bbox = bb_le,
                datetime = paste(start,end, sep = "/")) |>
    post_request() |>
    items_sign(sign_fn = sign_planetary_computer()) |> 
    items_fetch()
  
  bb_rc <- pol_rc |> 
    st_transform(32719) |> 
    st_bbox() |> 
    as.numeric()
  bb_le <- pol_le |> 
    st_transform(32719) |> 
    st_bbox() |> 
    as.numeric()
  
  v_rc = cube_view(srs = "EPSG:32719",
                extent = list(t0 = as.character(start), 
                              t1 = as.character(end),
                              left = bb_rc[1], right = bb_rc[3],
                              top = bb_rc[4], bottom = bb_rc[2]),
                dx = 10, dy = 10, dt = "P5D")
  v_le = cube_view(srs = "EPSG:32719",
                   extent = list(t0 = as.character(start), 
                                 t1 = as.character(end),
                                 left = bb_le[1], right = bb_le[3],
                                 top = bb_le[4], bottom = bb_le[2]),
                   dx = 10, dy = 10, dt = "P5D")
  
  col_rc <- stac_image_collection(items_rc$features)
  col_le <- stac_image_collection(items_le$features)
  
  cloud_mask <- image_mask("SCL", values=c(3,8,9))
  
  raster_cube(col_rc, v_rc, mask=cloud_mask) |> 
    write_tif('output_data/sentinel/rio_claro/')
  raster_cube(col_le, v_le, mask=cloud_mask) |> 
    write_tif('output_data/sentinel/la_esperanza/')
  
  # calcular indices
  
  files_rc <- list.files('outputs/sentinel/rio_claro/',
                         full.names=T,pattern = '.tif')
  files_le <- list.files('outputs/sentinel/la_esperanza/',
                         full.names=T,pattern = '.tif')
  
  fechas_rc <- gsub('_','-',substr(files_rc,nchar(files_rc)-13,nchar(files_rc)-4))
  fechas_le <- gsub('_','-',substr(files_le,nchar(files_le)-13,nchar(files_le)-4))
  
  band_name <- names(rast(files_rc[1]))[2:12]

  fechas_id <- tibble(sitio = c(rep('rio_claro',length(fechas_rc)),rep('la_esperanza',length(fechas_le)))
         ,fecha = c(fechas_rc,fechas_le)) |> 
    group_by(sitio) |> 
    mutate(id = row_number()) |>
    distinct(sitio,fecha,.keep_all=T) |> 
    # filter(en.temporada(fecha,temporada)) |> 
    arrange(sitio,fecha)
  
  r_rc <- rast(files_rc[filter(fechas_id,sitio == 'rio_claro') |> pull(id)])
  r_le <- rast(files_le[filter(fechas_id,sitio == 'la_esperanza') |> pull(id)])
  
  rc <- lapply(band_name, function(x) {
    band <- subset(r_rc,which(names(r_rc)==x))
    names(band) <- substr(sources(band),nchar(sources(band))-13,
                          nchar(sources(band))-4)
    band[[sort(names(band))]]/10000
  })
  le <- lapply(band_name, function(x) {
    band <- subset(r_le,which(names(r_le)==x))
    names(band) <- substr(sources(band),nchar(sources(band))-13,
                          nchar(sources(band))-4)
    band[[sort(names(band))]]/10000
  })

  names(rc) <- as.numeric(gsub('B','',band_name))
  names(le) <- as.numeric(gsub('B','',band_name))
  
  vi <- list(rc,le)
  
  sitio_name <- c('rio_claro','la_esperanza')
  
  for (i in seq_along(sitio_name)) {
    
    b <- vi[[i]]
    
    writeRaster((b$`8`-b$`11`)/(b$`8`+b$`11`),
                glue::glue('output_data/indices/RAW/NDMI_{sitio_name[i]}.tif'),
                overwrite=T)
    writeRaster(b$`11`/b$`8`,
                glue::glue('output_data/indices/RAW/MSI_{sitio_name[i]}.tif'),
                overwrite=T)
    writeRaster((b$`8`-b$`11`+b$`12`)/(b$`8`+b$`11`-b$`12`),
                glue::glue('output_data/indices/RAW/NMDI_{sitio_name[i]}.tif'),
                overwrite=T)
    writeRaster((b$`8`+b$`3`)/(b$`11`+b$`4`),
                glue::glue('output_data/indices/RAW/DWSI_{sitio_name[i]}.tif'),
                overwrite=T)
    writeRaster(((b$`6`/b$`5`)-1)/sqrt((b$`6`/b$`5`)+1),
                glue::glue('output_data/indices/RAW/msr705_{sitio_name[i]}.tif'),
                overwrite=T)
    
  }
  
  # suavizar indices
  
  files <- list.files(glue::glue('output_data/indices/RAW/'),full.names=T)
  name <- str_match(files, ".*/RAW/(.*?)\\.tif$")[,2]
  
  vi_r <- lapply(files, function(x) rast(x))
  
  suavizado <- list()
  
  for (i in seq_along(vi_r)) {
    print(name[i])
    vi <- vi_r[[i]]
    names(vi)
    fechas <- as.Date(names(vi))
    fechas_continuas <- seq(min(fechas),max(fechas), by = '1 day')
    
    vi_suavizado <- app(vi, \(y) {
      y[which(y > 10)] <- NA
      y[is.infinite(y)] <- NA
      if (length(which(!is.na(y))) > 2) {
        data <- data.frame(x = as.numeric(fechas), y = fill_edges(y))
        model <- loess(y ~ x, data = data, span = .3)
        new_data <- data.frame(x = as.numeric(fechas_continuas))
        p <- tryCatch({
          as.numeric(predict(model,new_data))
        }, error = function(e) {
          left_join(new_data,data,by='x') |> 
            mutate(y = na.approx(y,x)) |> 
            pull(y)
        })
        return(p)} else {
          return(rep(NA,length(fechas_continuas)))
        }
    })
    names(vi_suavizado) <- fechas_continuas
    writeRaster(vi_suavizado,glue::glue('output_data/indices/SMOOTH/{name[i]}_{substr(temporada,1,4)}.tif'),
                overwrite=T)
  }
  
  # Predecir potencial
  
  modelo <- read_rds('input_data/xgboost.rds')
  
  clima_var <- read_rds('output_data/clima/data_clima.rds')
  
  vi_
  
  
  
}



  
