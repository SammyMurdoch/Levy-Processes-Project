meixner_simulated_data <- read.csv("/Users/Sammy Murdoch/Documents/Imperial/Year 4/Project/Simulation/meixner_simulation0.csv")
#meixner_simulated_data <- read.csv("/Users/Sammy Murdoch/Documents/Imperial/Year 4/Project/Simulation/meixner_simulation_test.csv")

meixner_simulated_returns <- diff(unlist(meixner_simulated_data))
meixner_parameters <- c(-1.645186e-05, 1.704519e-02, 0, 2.264988e-01)
#meixner_parameters <- c(0, 1.704519e-02, 0, 2.264988e-01)


m_sim_x_vals <- seq(min(meixner_simulated_returns), max(meixner_simulated_returns), length.out = length(meixner_simulated_returns))
m_pdf_vals <- meixner_density(m_sim_x_vals, 
                            initial_param_est[1],
                            initial_param_est[2],
                            initial_param_est[3],
                            initial_param_est[4])

meixner_sim_hist <- ggplot(data.frame(increments=meixner_simulated_returns), aes(x = increments)) +
    labs(title = "Increment Histogram",
         x = "Increment Size", y = "Density") +
    geom_histogram(aes(y = ..density..), bins = 60, fill = "lightblue", color = "black") +
    xlim(-0.03, 0.03) +
    geom_line(aes(x = m_sim_x_vals, y = m_pdf_vals), color = "blue", size = 0.6)

meixner_sim_hist | symmetric_nig_hist

