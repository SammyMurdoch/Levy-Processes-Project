library(moments)

data <- Nolan_gdp_usd$returns

mean <- mean(data)
variance <- var(data)
skewness <- skewness(data)
kurtosis <- kurtosis(data)

summary(data)
g <- function(vars) {
    C <- vars[1]
    G <- vars[2]
    M <- vars[3]
    Y <- vars[4]
    
    
    vec <- c(C(M^(Y-1) - G^(Y-1)) * gamma(1-Y) - mean,
      C(M^(Y-2) + G^(Y-2)) * gamma(2-Y) - variance,
      (C(M^(Y-3) - G^(Y-3)) * gamma(3-Y)) / (C(M^(Y-2) + G^(Y-2)) * gamma(2-Y))^(3/2) - skewness,
      3 + (C(M^(Y-4) + G^(Y-4)) * gamma(4-Y)) / (C(M^(Y-2) + G^(Y-2)) * gamma(2-Y))^2 - kurtosis)
    
    return(vec)
}

initial_guess <- c(1, 1, 1, 1)
result <- nleqslv(initial_guess, g)$x

result

