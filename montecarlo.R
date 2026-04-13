monte_carlo_sim <- function(n, xmin, xmax, ymin, ymax, f) {
  x <- runif(n, xmin, xmax)
  y <- runif(n, ymin, ymax)
  y_func <- f(x)
  inside <- y <= y_func
  data.frame(
    x = x,
    y = y,
    y_func = y_func,
    inside = inside
  )
}

library(tidyverse)

n <- 1000

df <- monte_carlo_sim(
  n = n,
  xmin = 0, xmax = 4,
  ymin = 0, ymax = 0.8,
  f = function(x) dweibull(x, shape = 1.5, scale = 1)
)

df <- df %>%
  mutate(region = ifelse(inside, "below", "above"))

area_box <- (4 - 0) * (0.8 - 0)
estimate <- mean(df$inside) * area_box

ggplot(df, aes(x = x, y = y, color = region)) +
  geom_point(alpha = 0.6) +
  stat_function(
    fun = dweibull,
    args = list(shape = 1.5, scale = 1),
    color = "black",
    linewidth = 1
  ) +
  labs(
    title = paste("Monte Carlo Integration (n =", n, ")"),
    subtitle = paste("Estimated Area =", round(estimate, 4)),
    x = "x",
    y = "y",
    color = "Region"
  ) +
  theme_minimal()

library(tidyverse)
library(patchwork)

make_plot <- function(n) {
  df <- monte_carlo_sim(n, 0, 4, 0, 0.8, function(x) dweibull(x, 1.5, 1))
  
  df <- df %>%
    mutate(region = ifelse(inside, "below", "above"))
  
  est <- mean(df$inside) * (4 * 0.8)
  
  ggplot(df, aes(x, y, color = region)) +
    geom_point(alpha = 0.6) +
    stat_function(fun = dweibull, args = list(shape = 1.5, scale = 1)) +
    labs(title = paste("n =", n, "| est =", round(est, 3))) +
    theme_minimal()
}

plot10 <- make_plot(10)
plot100 <- make_plot(100)
plot1000 <- make_plot(1000)
plot10000 <- make_plot(10000)

plot10 + plot100 + plot1000 + plot10000