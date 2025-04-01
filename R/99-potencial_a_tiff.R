library(tidyverse)
library(terra)

d1 <- read_rds("data/old/la_esperanza copy.rds")
d1 <- unwrap(d1)
d1

names(d1)

map(names(d1), function(n = "2022-08-20"){
  cli::cli_inform(n)

  r <- d1[[n]]
  values(r) <-  -values(r)/ 10
  plot(r)
  
  r2 <- rast("data/potencial-raster/la_esperanza/2023-10-01.tif")
  plot(r2)

})

d2 <- read_rds("data/old/rio_claro copy.rds")
