library(moments)

data <- Nolan_gbp_jpy$returns

# 1. Compute observed skewness
obs_skew <- skewness(data)

# 2. Bootstrap resampling
n_boot <- 10000
boot_mean <- replicate(n_boot, {
    sample_x <- sample(data, replace = TRUE)
    mean(sample_x)
})

# 3. Two-sided p-value
p_value <- mean(abs(boot_skew) >= abs(obs_skew))
print(quantile(boot_skew, c(0.025, 0.975)))




obs_skew <- skewness(data)

# 2. Bootstrap resampling
n_boot <- 10000
boot_skew <- replicate(n_boot, {
    sample_x <- sample(data, replace = TRUE)
    skewness(sample_x)
})

# 3. Two-sided p-value
p_value <- mean(abs(boot_skew) >= abs(obs_skew))
print(quantile(boot_skew, c(0.025, 0.975)))


print(cor(boot_mean, boot_skew))