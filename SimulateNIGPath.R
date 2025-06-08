library(tidyr)
library(dplyr)

path_length <- 1000
N <- path_length # T/N=1 for comparisons with real data
T <- path_length

print(nig_pars)


t <- 1:1000


nig_path1 <- simulate_nig_path(N, T, usd_nig_pars)

nig_path2 <- simulate_nig_path(N, T, usd_nig_pars)

nig_path3 <- simulate_nig_path(N, T, usd_nig_pars)

nig_paths_df <- data.frame(
    x = 1:path_length,
    nig_path1 = nig_path1,
    nig_path2 = nig_path2,
    nig_path3 = nig_path3
)

# Convert to long format correctly
nig_path_df_long <- nig_paths_df %>%
    pivot_longer(
        cols = c("nig_path1", "nig_path2", "nig_path3"),
        names_to = "vector",
        values_to = "value"
    )

# Plot
nig_lt_plot <- ggplot(nig_path_df_long, aes(x = x, y = value, color = vector)) +
    geom_line() +
    labs(title = "Simulated NIG Process Paths", x = "t", y = expression(L[t])) +
    theme(legend.position = "none")

nig_exp_lt_plot <- ggplot(nig_path_df_long, aes(x = x, y = exp(value), color = vector)) +
    geom_line() +
    labs(title = "Simulated Exponentiated NIG Process Paths", x = "t", y = expression(X[t])) +
    theme(legend.position = "none")

nig_lt_plot | nig_exp_lt_plot


exp_lt_plot | nig_exp_lt_plot
