library(GeneralizedHyperbolic)

data <- Nolan_gbp_usd$returns

norminvgaufit <- nigFit(data)
nig_pars <- norminvgaufit$param
print(nig_pars)
x_vals <- seq(min(data), max(data), length.out = length(data))

pdf_vals <- dnig(x_vals, alpha = nig_pars["alpha"], beta = nig_pars["beta"], 
                 delta = nig_pars["delta"], mu = nig_pars["mu"])

ggplot(data.frame(Nolan_gbp_usd), aes(x = returns)) +
    geom_histogram(aes(y = ..density..), bins = 50, fill = "lightblue", color = "black") +
    labs(title = "GBP/USD daily returns from 02-01-2020 to 09-01-2025 with ML fitted NIG distribution.",
         x = "Daily Log Proportional Change", y = "Density") +
    geom_line(aes(x = x_vals, y = pdf_vals), color = "blue", size = 0.6)


simulate_nig_path <- function(N, T, pars) {
    increment_time <- T/N
    pars["mu"] <- pars["mu"] * increment_time # IS this actually correct for T/N not = 1 multiply delta by t instead
    pars["delta"] <- pars["delta"] * increment_time
    
    nig_samples <- rnig(N, mu=pars["mu"], delta=pars["delta"], beta=pars["beta"], alpha=pars["alpha"])
    
    return(cumsum(nig_samples))
}
print(nig_pars)

N <- 100000 # T/N=1 for comparisons with real data
T <- 100000

# nig_pars <- c(4.592591e-04, 5.879460e-03, 184.9736, -15.95070+1.04589)

values <- simulate_nig_path(N, T, nig_pars)


plot(1:N, values, type = "s", lwd = 2, col = "blue", main = "Simulated GBP/USD log returns using an NIG Process", 
     xlab = "Time", ylab = "Return")


simulated_returns <- diff(values)

nig_pars <- norminvgaufit$param  # This is repeated


x_vals2 <- seq(min(data), max(data), length.out = length(k_simulated_returns))

pdf_vals2 <- dnig(x_vals2, alpha = nig_pars["alpha"], beta = nig_pars["beta"], 
                  delta = nig_pars["delta"], mu = nig_pars["mu"])

ggplot(data.frame(increments=simulated_returns), aes(x = increments)) +
    labs(title = "NIG simulated GBP/USD exchange rate return histogram.",
         x = "Daily Log Proportional Change", y = "Density") +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
    xlim(-0.03, 0.03)

k_simulated_data <- read.csv("/Users/Sammy Murdoch/Documents/Imperial/Year 4/Project/Simulation/NIGLevySim.csv")
k_simulated_returns <- diff(unlist(k_simulated_data))

# hist(simulated_returns, bins=30)

nig_pars <- norminvgaufit$param
nig_pars["mu"] <- 0
nig_pars["beta"] <- 0
print("Here are the nig pars")
print(nig_pars)


x_vals2 <- seq(min(data), max(data), length.out = length(k_simulated_returns))

pdf_vals2 <- dnig(x_vals2, alpha = nig_pars["alpha"], beta = nig_pars["beta"], 
                 delta = nig_pars["delta"], mu = nig_pars["mu"])


symmetric_nig_hist <- ggplot(data.frame(increments=k_simulated_returns), aes(x = increments)) +
    labs(title = "Increment Histogram",
         x = "Increment Size", y = "Density") +
    geom_histogram(aes(y = ..density..), bins = 60, fill = "lightblue", color = "black") +
    xlim(-0.03, 0.03) +
    geom_line(aes(x = x_vals2, y = pdf_vals2), color = "blue", size = 0.6)

simulated_NIG_path_df <- data.frame(
    time = 1:length(unlist(k_simulated_data)),
    value = unlist(k_simulated_data))

nig_paths_plot <- ggplot(simulated_NIG_path_df, aes(x = time, y = value)) +
    geom_line(color = "blue") +
    labs(title = "Simulated NIG Process Path", x = "t", y = expression(L[t])) +
    theme(legend.position = "none")

nig_paths_plot | symmetric_nig_hist