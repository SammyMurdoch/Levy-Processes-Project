tau_indicator <- function(x1, x2, x3, alpha) {
    indicator <- ifelse((x1+x2)/(2^(1/alpha)) <= x3, 1, 0)

    indicator

}

tau <- function(x1, x2, x3, alpha) {
 1/3 * (tau_indicator(x1, x2, x3, alpha) +
            tau_indicator(x1, x3, x2, alpha) +
            tau_indicator(x2, x3, x1, alpha)) - 1/2
}

delta <- function (data, alpha) {
    combinations <- combn(data, 3)
    total <- 0
    
    for (i in 1:ncol(combinations)) {
        total <- total + tau(combinations[1, i], combinations[2, i], combinations[3, i], alpha)
    }
    
    return(total / choose(length(data), 3))

}


compute_jackknife_values(data, alpha) {
    N <- length(data)
    values <- rep(NA, N)
    
    full_estimate <- delta(data, alpha)
    
    for (i in 1:N) {
        values[i] <- N * full_estimate - (N-1) * delta(data[-i], alpha)
    }
    
    values
}