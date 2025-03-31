library(tidyverse)

d <- read_rds("data/old/clima_dia.rds")

d |> 
  group_by(temporada) |> 
  summarise(min(fecha), max(fecha), n())

r_cli <- d |> 
  mutate(fecha = ymd(fecha)) |> 
  select(
    site = sitio,
    date = fecha,
    eto,
    vpd_medio,
    t_media,
    rh_media
  )
# usamos la misma idea que en el script
# climate -----------------------------------------------------------------
fs::dir_create(glue("data/climate"))

cli::cli_inform("write_csv")
file_cli <- "data/climate/climate-sites.csv"

if(file.exists(file_cli)) {
  dcli <- read_csv(file_cli)
} else {
  dcli <- tibble()
}

dcli
dcli |> group_by(site) |> summarise_all(.funs = median, na.rm =TRUE)

r_cli |> 
  as_tibble() |> 
  # hacer mas simple 
  mutate(across(where(is.numeric), function(x) round(x, 3))) |> 
  distinct() |> 
  # mutate(
  #   site = site,
  #   date = date,
  #   .before = 1
  # ) |> 
  bind_rows(dcli) |> 
  distinct(date, site, .keep_all = TRUE) |> 
  arrange(date, site) |> 
  write_csv(file_cli)
