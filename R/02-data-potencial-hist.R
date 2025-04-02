source("R/00-funs.R")

# All dates ---------------------------------------------------------------
dates <- seq(ymd("20231001"), today() - 1, by = "1 day")
dates <- dates[!month(dates) %in% 5:9]
dates <- rev(dates)

dates_dwloaded <- fs::dir_ls("data/potencial-raster/", recurse = TRUE) |>
  basename() |>
  tools::file_path_sans_ext() |>
  str_subset("[a-z]", negate = TRUE) |>
  unique() |>
  ymd()

length(dates_dwloaded)/length(dates)

dates <- setdiff(as.character(dates), as.character(dates_dwloaded))
dates <- ymd(dates)

dates

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
