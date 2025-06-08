library(zoo)
library(ggplot2)

data <- Nolan_gdp_usd

window_size <- 250
time <- 1:nrow(usd_data)

data$rolling_variance <- rollapply(data$returns, width = window_size, FUN = var, fill = NA, align = "right")
ggplot(data, aes(x = time, y = rolling_variance)) +
    geom_line(color = "blue") +
    labs(title = "Rolling Volatility Over Time",
         x = "Time",
         y = "Volatility4") +
    theme_minimal()

View(data)
