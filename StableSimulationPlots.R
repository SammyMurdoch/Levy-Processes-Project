simulated_data <- read.csv("/Users/Sammy Murdoch/Documents/Imperial/Year 4/Project/Simulation/StableLevySim.csv")
simulated_returns <- diff(unlist(simulated_data))

# theoretical_parameters <- c(1.7310550534, 0, 0.0033295311, -2.128838e-05)
theoretical_parameters <- c(1.7310550534, 0,  0.0033295311, 0)
theoretical_parameters <- c(1.7310550534, -0.1932653374,  0.0033295311, -0.0003105492) # Potentially missing a zero
theoretical_parameters <- c(1.7310550534, 0,  0.00372, 0) # Potentially missing a zero

stable_x_vals <- seq(min(simulated_returns), max(simulated_returns), length.out = length(simulated_returns))
stable_pdf_values <- stable_pdf(stable_x_vals, theoretical_parameters, parametrization = 1L)
# 1.731055, -1.932653e-01, 3.329531e-03, -2.128838e-05)

stable_sim_hist <- ggplot(data.frame(increments=simulated_returns), aes(x = increments)) +
    labs(title = "Increment Histogram.",
         x = "Increment Size", y = "Density") +
    geom_histogram(aes(y = ..density..), bins = 60, fill = "lightblue", color = "black") +
    xlim(-0.03, 0.03) +
    geom_line(aes(x = stable_x_vals, y = stable_pdf_values), color = "blue", size = 0.6)

simulated_stable_path_df <- data.frame(
    time = 1:length(unlist(simulated_data)),
    value = unlist(simulated_data))

stable_paths_plot <- ggplot(simulated_stable_path_df, aes(x = time, y = value)) +
    geom_line(color = "blue") +
    labs(title = "Simulated Stable Process Path", x = "t", y = expression(L[t])) +
    theme(legend.position = "none")

stable_paths_plot | stable_sim_hist