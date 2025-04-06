source("R/00-funs.R")
# Process -----------------------------------------------------------------
cli::cli_h1("Process")

dates <- today() - 0:1

purrr::walk(dates, safely(function(date = sample(dates, 1)){

  cli::cli_h1(as.character(date))

  cli::cli_progress_step(as.character(date))
  
  cli::cli_h2("Cleanup")
  safely(fs::dir_delete)("outputs/")

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

cli::cli_h2("Creando data/potencial-raster/dates.csv")
dir("data/potencial-raster", full.names = TRUE, recursive = TRUE) |> 
  basename() |> 
  stringr::str_subset("\\.tif") |> 
  stringr::str_remove("\\.tif") |> 
  unique() |> 
  sort() |> 
  tibble::tibble(date = _) |> 
  readr::write_csv("data/potencial-raster/dates.csv")


