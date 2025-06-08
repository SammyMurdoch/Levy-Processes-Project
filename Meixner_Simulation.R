simulate_meixner_path <- function(a, b, d, m, r, q) {
    theta <- -1/a * (b + 2*atan((-cos(a/2) + exp((m-r+q)/(2*d)))/(sin(a/2))))
}

levy_measure <- function(x, a, b, d) {
    d * (exp((b*x)/a))/(x * sinh((pi * x)/a))
}

compute_intensities_jump_sizes <- function(epsilon, upper_bound, positive_interval_count, a, b, d) {
    poisson_intensities <- vector(mode="numeric", length=positive_interval_count*2)
    jump_sizes <- vector(mode="numeric", length=positive_interval_count*2)
    
    step_size <- (upper_bound - epsilon) / positive_interval_count
    
    current_lower_i_bound <- epsilon
    
    for (i in 1:positive_interval_count) {
        current_upper_i_bound <- current_lower_i_bound + step_size
        
        n_i <- positive_interval_count-i+1
        p_i <- positive_interval_count+i
        
        poisson_intensities[n_i] = integrate(levy_measure, lower=-1*current_upper_i_bound, upper=-1*current_lower_i_bound, a=a, b=b, d=d)$value
        jump_sizes[n_i] = -1*compute_interval_jump_sizes(poisson_intensities[n_i], -1*current_upper_i_bound, -1*current_lower_i_bound, a, b, d)
        
        poisson_intensities[p_i] = integrate(levy_measure, lower=current_lower_i_bound, upper=current_upper_i_bound, a=a, b=b, d=d)$value
        jump_sizes[p_i] = compute_interval_jump_sizes(poisson_intensities[p_i], current_lower_i_bound, current_upper_i_bound, a, b, d)
        
        current_lower_i_bound <- current_lower_i_bound + step_size
    }
    
    return(list(poisson_intensities, jump_sizes))
}

compute_meixner_interval_variance <- function(lower_bound, upper_bound, a, b, d) {
    integrand <- function(x, a, b, d) {
        return(d * x * (exp((b*x)/a))/(sinh((pi * x)/a)))
    }
    integrate(integrand, lower=lower_bound, upper=upper_bound, a=a, b=b, d=d)$value
}

compute_interval_jump_sizes <- function(interval_intensity, lower_bound, upper_bound, a, b, d) {
    variance <- compute_meixner_interval_variance(lower_bound, upper_bound, a, b, d)
    
    return(sqrt(variance/interval_intensity))
}

simulate_full_jump_data <- function(T, intensities, jump_sizes) {
    
    jump_df <- data.frame(time=numeric(), jump_size=numeric())

    for (i in 1:length(intensities)) {
        jump_times <- get_poisson_jump_times(T, intensities[i])
        
        if (!is.null(jump_times)) {
            new_rows <- data.frame(time=jump_times, jump_size=jump_sizes[i])
            jump_df <- rbind(jump_df, new_rows)
        }
    }

    sorted_jump_df <- jump_df[order(jump_df$time), ]
}
    
    
get_poisson_jump_times <- function(T, l) {
    N <- rpois(1, lambda = l*T)
    if (N==0) {
        return(NULL)
    }
    
    uniform_samples <- runif(N)

    return(T*uniform_samples) # Unsorted for efficiency
}


discritise_poisson_jump_data <- function(poisson_jump_time_df, T, N) {
    scaling_factor <- T/N
    poisson_jump_time_df$jump_interval <- ceiling(poisson_jump_time_df$time / scaling_factor)
    
    compressed_jump_data <- aggregate(jump_size ~ jump_interval, data = poisson_jump_time_df, FUN = sum)

    poisson_values <- vector(mode="numeric", length=N)
    
    current_value <- 0

    n <- 1
    
    for (i in 1:nrow(compressed_jump_data)) {
        while (n < compressed_jump_data$jump_interval[i]) {
            poisson_values[n] = current_value
            n <- n + 1
        }
        
        current_value <- current_value + compressed_jump_data$jump_size[i]
    }
    
    while (n <= N) {
        poisson_values[n] = current_value
        n <- n + 1
    }
    
    return(poisson_values)
    
}

simulate_brownian_motion <- function(sig, T, N) {
    interval_length <- T/N
    normal_samples <- rnorm(N, 0, sqrt(interval_length))
    print(normal_samples)
    values <- sig*cumsum(normal_samples)
    
    return(values)
}

simulate_small_jump_compensation <- function(intensities, jump_sizes, T, N) {
    values <- 1:N
    values <- values * (T/N)

    jump_sizes[jump_sizes<1] <- 0
    combined_term <- -1 * sum(jump_sizes * intensities)
    
    return(values*combined_term)
}

small_jump_brownian_sim <- function(epsilon, sig, a, b, d, T, N) {
    # var <- 0.00004817075621264678 # Cheated a bit
    var <- 0.00001468341676331476

    # var <- compute_meixner_interval_variance(-epsilon, epsilon, a, b, d)

    sig <- sqrt(sig^2 + var)
    simulated_values <- simulate_brownian_motion(sig, T, N)
    
    return(simulated_values)
}

# get_meixner_linear_term <- function(a, b, d) {
#     f_t <- a*d*tan(b/2)
#     int_val <- 2*d*
# 
# }
    
N <- 10000
T <- 10000
epsilon <- 0.004


pars <- c(0, 0.01690119, 0, 0.35156535)

intensities_jump_sizes <- compute_intensities_jump_sizes(epsilon, 0.06, 100, pars[2], pars[3], pars[4])
print(intensities_jump_sizes[[2]])
jump_data <- simulate_full_jump_data(T, intensities_jump_sizes[[1]], intensities_jump_sizes[[2]])

jump_values <- discritise_poisson_jump_data(jump_data, T, N)
compensated_jumps <- simulate_small_jump_compensation(intensities_jump_sizes[[1]], intensities_jump_sizes[[2]], T, N)
small_jump_brownian <- small_jump_brownian_sim(epsilon, 0, pars[2], pars[3], pars[4], T, N)

print(length(small_jump_brownian))

full_values <- jump_values + compensated_jumps + small_jump_brownian

print(full_values)

simulated_increments <- diff(full_values)



x_vals <- seq(-0.025, 0.025, length.out = 9999)
pdf_vals <- meixner_density(x_vals, 
                            pars[1],
                            pars[2],
                            pars[3],
                            pars[4])

    






ggplot(data.frame(increments=simulated_increments), aes(x = increments)) +
    labs(title = "Meixner simulated GBP/USD exchange rate return histogram (no longer dodgy shape?).",
         x = "Daily Log Proportional Change", y = "Density") +
    geom_histogram(aes(y = ..density..), bins = 17, fill = "lightblue", color = "black") +
    xlim(-0.025,0.025) +
    geom_line(aes(x = x_vals, y = pdf_vals), color = "blue", size = 0.6)


# ggplot(plot_data, aes(x = time, y = value)) +
#     geom_step(direction = "hv", color = "blue", size = 1.2) +  # hv = horizontal-vertical
#     labs(title = "Step Line Graph", x = "Time", y = "Value") +
#     theme_minimal()
# 


# Plot the step function
plot(1:N, full_values, type = "s", lwd = 2, col = "blue", main = "Simulated GBP/USD log returns using a Meixner Process", 
     xlab = "Time", ylab = "Return")



# indicator on 1 s due to truncation func




# Meixner has no brownian part and so poisson approx as discussed in eqr470: Simulation of Lévy processes should work


# Could probobly do some statistical test to see if the resulting simulation
# Increments are from a Meixner distribution
# Can replace small jumps by brownian for this specific distribution