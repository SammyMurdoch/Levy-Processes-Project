usd_stable_pars <- c(1.731055e+00, -1.932653e-01,  3.329531e-03, -2.128838e-05)
aud_stable_pars <- c(1.742644e+00, 1.048777e-01, 3.014181e-03, 5.977782e-05)
jpy_stable_pars <- c(1.587013004, -0.091840629,  0.003595222,  0.000304176)

usd_norm_pars <- c(-9.542546e-05, sqrt(5.021235e-05))
aud_norm_pars <- c(5.972597e-05, sqrt(7.713897e-05))
jpy_norm_pars <- c(-0.0002861068, sqrt(4.74532e-05))

usd_data <- Nolan_gbp_usd$returns
aud_data <- Nolan_gbp_aud$returns
jpy_data <- Nolan_gbp_jpy$returns

# get_class_count <- function(N, alpha) {
#     c <- qnorm(1-alpha)
#     k <- 4 * (2*(N-1)^2 / c^2)^(1/5)
#     
#     k
# }


#get_class_count(length(usd_data), 0.05)

get_stable_chi_sq <- function(data, pars, interval_count) {
    interval_length <- 1/interval_count
    p <- seq(0, 1, by=interval_length)
    
    expected_frequency <- rep(length(data) * interval_length, interval_count)
    quantiles <- stable_q(p, pars, parametrization = 0L, tol = 1e-12)
    
    buckets <- cut(data, breaks = quantiles, include.lowest = TRUE, labels = FALSE)
    bucket_counts <- as.numeric(table(buckets))

    for (value in bucket_counts) {
        if (value < 5) {
            stop(paste("Error: Bucket contains", value, "is less than 5"))
        }
    }
    
    chisq_stat <- sum((bucket_counts - expected_frequency)^2 / expected_frequency)
    p_value <- pchisq(chisq_stat, interval_count-5, lower.tail = FALSE)
    
    return(p_value)
    
}

get_normal_chi_sq <- function(data, mean, sd, interval_count) {
    interval_length <- 1/interval_count
    p <- seq(0, 1, by=interval_length)
    
    expected_frequency <- rep(length(data) * interval_length, interval_count)
    quantiles <- qnorm(p, mean=mean, sd=sd)
    
    buckets <- cut(data, breaks = quantiles, include.lowest = TRUE, labels = FALSE)
    bucket_counts <- as.numeric(table(buckets))

    for (value in bucket_counts) {
        if (value < 5) {
            stop(paste("Error: Bucket contains", value, "data points, need at least 5"))
        }
    }
    
    chisq_stat <- sum((bucket_counts - expected_frequency)^2 / expected_frequency)
    
    p_value <- pchisq(chisq_stat, interval_count-3, lower.tail = FALSE)
}
m_values <- 5:75
print(length(m_values))

# usd_p_values <- numeric(30)
aud_p_values <- numeric(length(m_values))
# jpy_p_values <- numeric(30)


for (i in m_values) {
    # usd_p_values[i] <- get_stable_chi_sq(usd_data, usd_stable_pars, i)$p.value
    aud_p_values[i-m_values[1]+1] <- get_stable_chi_sq(aud_data, aud_stable_pars, i)
    # jpy_p_values[i] <- get_stable_chi_sq(jpy_data, jpy_stable_pars, i)$p.value
    
}
aud_p_value_df <- data.frame(m=m_values, aud_p_values)
ggplot(aud_p_value_df, aes(x = m, y = aud_p_values)) + 
    geom_point() +
    geom_abline(slope = 0, intercept = 0.05, linetype = "dashed", color = "red") +
    labs(title="P-value of the chi-squared test for different class counts", x = expression("Number of classes," ~ m), y = "P-value")

    
    


print(get_stable_chi_sq(usd_data, usd_stable_pars, 50))
print(get_stable_chi_sq(aud_data, aud_stable_pars, 50))
print(get_stable_chi_sq(jpy_data, jpy_stable_pars, 50)) # 100 causes small number of samples


# print(get_normal_chi_sq(usd_data, usd_norm_pars[1], usd_norm_pars[2], 50))
# print(get_normal_chi_sq(aud_data, usd_norm_pars[1], usd_norm_pars[2], 40))
# print(get_normal_chi_sq(jpy_data, usd_norm_pars[1], usd_norm_pars[2], 50)) # 100 causes small number of samples
# 


#data.frame(Data = usd_data, Bucket = buckets)
