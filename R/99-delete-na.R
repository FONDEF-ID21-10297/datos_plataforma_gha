
# script que borra raster si fueron generados con datos incompletos o eto = 0 (datos 2022)
library(tidyverse)

d <- read_csv("data/climate/climate-sites.csv")

d1 <- d |> 
  filter(eto == 0) |> 
  pull(date)

d2 <- d |> 
  filter(!complete.cases(d)) |> 
  pull(date)


dates <- c(d1, d2) |> 
  as.character() |> 
  unique()

dates

str_glue("data/potencial-raster/la_esperanza/{dates}.tif") |> as.character() |> map(safely(fs::file_delete))
str_glue("data/potencial-raster/rio_claro/{dates}.tif") |> as.character() |> map(safely(fs::file_delete))
