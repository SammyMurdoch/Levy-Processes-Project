meixner_simulated_data0 <- unlist(read.csv("/Users/Sammy Murdoch/Documents/Imperial/Year 4/Project/Simulation/meixner_simulation0.csv"))
# meixner_simulated_data1 <- unlist(read.csv("/Users/Sammy Murdoch/Documents/Imperial/Year 4/Project/Simulation/meixner_simulation1.csv"))
# meixner_simulated_data2 <- unlist(read.csv("/Users/Sammy Murdoch/Documents/Imperial/Year 4/Project/Simulation/meixner_simulation2.csv"))

meixner_paths_df <- data.frame(
    x = 1:length(meixner_simulated_data0),
    meixner_path1 = meixner_simulated_data0
    # meixner_path2 = exp(meixner_simulated_data1),
    # meixner_path3 = exp(meixner_simulated_data2)
)

# Convert to long format correctly
meixner_path_df_long <- meixner_paths_df %>%
    pivot_longer(
        cols = c("meixner_path1"),#, "meixner_path2", "meixner_path3"),
        names_to = "vector",
        values_to = "value"
    )

# Plot
meixner_exp_paths_plot <- ggplot(meixner_path_df_long, aes(x = x, y = value)) +
    geom_line(color="blue") +
    labs(title = "Simulated Meixner Process Path", x = "t", y = expression(L[t])) +
    theme(legend.position = "none")

meixner_exp_paths_plot | meixner_sim_hist