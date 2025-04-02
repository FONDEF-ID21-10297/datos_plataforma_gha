library(tidyverse)

d1 <- read_rds("data/old/la_esperanza.rds")
d2 <- read_rds("data/old/rio_claro.rds")

d1
d2

dpot_site <- bind_rows(
  d1 |> mutate(site = "la_esperanza") |> select( site, id, date = fecha, potencial),
  d2 |> mutate(site = "rio_claro") |> select( site, id, date = fecha, potencial)
)

# igual que script -------------------------------------------------------
fs::dir_create("data/potencial-csv")
file_pot <- "data/potencial-csv/potencial-sites.csv"

if(file.exists(file_pot)) {
  dpot <- read_csv(file_pot)
} else {
  dpot <- tibble()
}

dpot

dpot_site |>
  mutate(potencial = -potencial / 10) |> 
  bind_rows(dpot) |> 
  distinct(date, site, id, .keep_all = TRUE) |> 
  arrange(date, site, id) |> 
  mutate(across(where(is.numeric), function(x) round(x, 3))) |> 
  write_csv(file_pot)
