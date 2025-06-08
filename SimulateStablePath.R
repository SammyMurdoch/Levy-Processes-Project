library(tidyr)
library(dplyr)

print(sample(0:100, 1))
set.seed(sample(0:50000, 1))
pars <- c(1.7310550534, -0.1932653374, 0.0033295311, -0.0003105492)

path_length <- 1000

stable_path1 <- cumsum(stable_rnd(path_length, pars, parametrization=1L))
set.seed(sample(0:50000, 1))
stable_path2 <- cumsum(stable_rnd(path_length, pars, parametrization=1L))
set.seed(sample(0:50000, 1))
stable_path3 <- cumsum(stable_rnd(path_length, pars, parametrization=1L))

stable_paths_df <- data.frame(
    x = 1:path_length,
    stable_path1 = stable_path1,
    stable_path2 = stable_path2,
    stable_path3 = stable_path3
)

# Convert to long format correctly
stable_path_df_long <- stable_paths_df %>%
    pivot_longer(
        cols = c("stable_path1", "stable_path2", "stable_path3"),
        names_to = "vector",
        values_to = "value"
    )

# Plot
lt_plot <- ggplot(stable_path_df_long, aes(x = x, y = value, color = vector)) +
    geom_line() +
    labs(title = "Simulated Stable Process Paths", x = "t", y = expression(L[t])) +
    theme(legend.position = "none")

exp_lt_plot <- ggplot(stable_path_df_long, aes(x = x, y = exp(value), color = vector)) +
    geom_line() +
    labs(title = "Simulated Exponentiated Stable Process Paths", x = "t", y = expression(X[t])) +
    theme(legend.position = "none")

lt_plot | exp_lt_plot
