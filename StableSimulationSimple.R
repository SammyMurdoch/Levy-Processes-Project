sample_stable_distribution <- function(N, T, pars) {
    increment_time <- T/N
    
    adjusted_scale <- increment_time^(1/pars[1]) * pars[3]
    pars[3] <- adjusted_scale

    pars[4] <- increment_time * pars[4]
    
    sample <- stable_rnd(N, pars, parametrization = 0L)

    
    return(cumsum(sample))
}

N <- 100000 # For test to see whether simulation matches reality T/N = 1
T <- 100000

# pars <- c(1.532385e+00, -2.335780e-02,  3.720541e-03, -2.177178e-05)
pars <- c(1.5841320038, 0.0055478219, 0.0039250669, -0.0001298384)

values <- sample_stable_distribution(N, T, pars)
print(length(values))


plot(1:N, values, type = "s", lwd = 2, col = "blue", main = "Simulated GBP/USD log returns using a Stable Process", 
     xlab = "Time", ylab = "Return")


simulated_returns <- diff(values)

ggplot(data.frame(increments=simulated_returns), aes(x = increments)) +
    labs(title = "Stable simulated GBP/USD exchange rate return histogram.",
         x = "Daily Log Proportional Change", y = "Density") +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
    xlim(-0.03, 0.03)

# SHOULD OVERLAY THE CORRECT DISTRIBUTION