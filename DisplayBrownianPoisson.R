library(tidyr)


brownian_sim <- read.csv("/Users/Sammy Murdoch/Documents/Imperial/Year 4/Project/Simulation/BrownianSim.csv")

sim_df <- as.data.frame(brownian_sim)
colnames(sim_df) <- c("line1", "line2", "line3")

new_row <- data.frame(line1 = 0, line2 = 0, line3=0)

# Add the row at the start
sim_df <- rbind(new_row, sim_df)
t <- seq(0, 999)

sim_df$t <- t
sim_df_long <- pivot_longer(sim_df, cols = -t, names_to = "Variable", values_to = "Value")



plt1 <- ggplot(sim_df_long, aes(x = t, y = Value, group=Variable, colour=Variable)) +
    geom_line() +
    labs(title = "Simulated Standard Brownian Motion Paths", x = "t", y = expression(W[t])) +
    theme(legend.position = "none")


poisson_sim <- read.csv("/Users/Sammy Murdoch/Documents/Imperial/Year 4/Project/Simulation/PoissonSim.csv")
split_indices <- which(poisson_sim$Value==0)
split_indices <- c(split_indices, nrow(poisson_sim) + 1)
split_poisson_sim <- list()

for (i in 1:(length(split_indices)-1)) {
    split_poisson_sim[[i]] <- poisson_sim[split_indices[i]:(split_indices[i+1]-1),]
}

split_poisson_sim2 = list()

for (i in 1:length(split_poisson_sim)) {
    split_poisson_sim2[[i]] <- na.omit(split_poisson_sim[[i]])
}

for (i in 1:length(split_poisson_sim2)) {
    if (tail(split_poisson_sim2[[i]]$X..Time, 1) > 10) {
        split_poisson_sim2[[i]] <- split_poisson_sim2[[i]][-nrow(split_poisson_sim2[[i]]), ]  # Remove the last row
    }
}

for (i in 1:length(split_poisson_sim2)) {
    last_y_val <- tail(split_poisson_sim2[[i]]$Value, 1)
    split_poisson_sim2[[i]] <- bind_rows(split_poisson_sim2[i], data.frame(X..Time = 10, Value = last_y_val))
}


poisson_sim_combined <- bind_rows(split_poisson_sim2, .id = "group")

plt2 <- ggplot(poisson_sim_combined, aes(x = X..Time, y = Value, colour = group)) +
    geom_step(direction = "hv") +
    xlim(0, 10) +
    labs(title = "Simulated Poisson Process Paths", x = "t", y = expression(P[t])) +
    theme(legend.position = "none")


plt1 | plt2

    
