library(tidyverse)
library(patchwork)

theme_set(theme_minimal())

na_lm <- function(y) {
  
  df <- data.frame(x = seq_along(y), y = y)
  
  # Ajustar modelo lineal ignorando los NA
  modelo <- lm(y ~ x + x*x + x*x*x, data = df, na.action = na.exclude)
  # modelo <- step(modelo, trace = -1)
  # Predecir los valores para las posiciones con NA
  df$y[is.na(df$y)] <- predict(modelo, newdata = df[is.na(df$y), ])
  
  df$y
  
} 

na_loess <- function(y, span = 0.75) {
  
  df <- data.frame(x = seq_along(y), y = y)
  
  modelo <- loess(y ~ x, data = df, span = span, na.action = na.exclude)
  
  df$y[is.na(df$y)] <- predict(modelo, newdata = df[is.na(df$y), ])
  
  df$y
  
}

na_approx <- function(y){
  
  y 
  
  first_value <- y[which(!is.na(y))[1]]
  y[1:which(!is.na(y))[1]] <- first_value
  
  # Replace trailing NA values with the last non-NA value
  last_value <- y[which(!is.na(y))[length(which(!is.na(y)))]]
  y[which(!is.na(y))[length(which(!is.na(y)))] : length(y)] <- last_value
  
  y <- zoo::na.approx(y)
  
  y
}

fill_data <- function(y){
  
  data <- data_frame(id = seq_along(y), y = y) 
  
  d2 <- data |> 
    mutate(
      class = is.na(y),
      na_approx = na_approx(y),
      na_lm = na_lm(y),
      na_loess = na_loess(y),
      imputeTS = imputeTS::na_interpolation(y, option = "spline"),
      kalman = imputeTS::na_kalman(y)
    ) |> 
    pivot_longer(cols = -c(id, class)) |> 
    mutate(
      name = forcats::fct_inorder(name),
      name = forcats::fct_relevel(name, "y")
    ) |> 
    arrange(name, id)
  
  d2
  
}

plots <- function(d2){
  
  p1 <- ggplot(d2, aes(id, value)) +
    # geom_smooth(color = "darkred", size = 0.5, fill = "gray90") + 
    geom_line(color = "gray", size = 1.5) + 
    geom_point(aes(color = class), size = 2) +
    scale_color_viridis_d(begin = .1, end = .9) + 
    facet_wrap(vars(name)) 
  
  p1
  
  p2 <- ggplot(d2, aes(id, value, color = name)) +
    geom_line() +
    geom_line(aes(group = name), color = "gray") +
    scale_color_viridis_d() + 
    geom_point(
      size = 4,
      data  = d2 |> filter(class)
    )
  
  p1 + p2 + patchwork::plot_layout(guides = "collect")

}


# Crear un vector de ejemplo con valores NA en los bordes
set.seed(123)
y <- c(NA, NA, 2 * runif(5, 0, 10), NA, 3*runif(5, 0, 10), NA)
y <- round(y, 2)
y

plots(d2 <- fill_data(y))


y <- c(NA, NA, 1:5 + rnorm(5), NA, 7:11 + rnorm(5), NA)
y <- round(y, 2)
y

plots(d2 <- fill_data(y))
  
