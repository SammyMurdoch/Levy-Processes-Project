library(moments)
library(nleqslv)
library(pracma)

data <- Nolan_gbp_usd$returns

mean <- mean(data)
variance <- var(data)
skewness <- skewness(data)
kurtosis <- kurtosis(data)
print("hi")
print(mean)
print(variance)
print(skewness)
print(kurtosis)

print("hi")

f <- function(vars) {
    a <- vars[1]
    d <- vars[2]
    
    c(1/2 * a**2*d - variance,
      3 + 1/d - kurtosis)
    
}

initial_guess <- c(0.01, 0.3)
result <- nleqslv(initial_guess, f)$x

initial_param_est <- c(0, result[1], 0, result[2])


meixner_density <- function(x, m, a, b, d) {
    frac <- (2*cos(b/2))^(2*d)/(2*a*pi*gamma(2*d))
    expo <- exp((b*(x-m))/a)
    mod <- Mod(gammaz(complex(real=d, imaginary=(x-m)/a)))^2
    
    return(frac*expo*mod)
}


x_vals <- seq(min(data), max(data), length.out = length(data))
pdf_vals <- meixner_density(x_vals, 
                            initial_param_est[1],
                            initial_param_est[2],
                            initial_param_est[3],
                            initial_param_est[4])

print("look below")
initial_param_est
print("look above")


ggplot(data.frame(Nolan_gbp_usd), aes(x = returns)) +
    geom_histogram(aes(y = ..density..), bins = 50, fill = "lightblue", color = "black") +
    labs(title = "GBP/USD daily changes from 2-01-1980 to 21-05-1996 with ML fitted Meixner distribution.",
         x = "Daily Log Proportional Change", y = "Density") +
    
    geom_line(aes(x = x_vals, y = pdf_vals), color = "blue", size = 0.6)


compute_theta_star <- function(pars, r, q) {
    m <- pars[1]; a <- pars[2]; b <- pars[3]; d <- pars[4]
    print("para here")
    print(a)
    print(b)
    print(d)
    print(m)
    
    
    arctanarg <- (-cos(a/2) + exp((m-r+q)/(2*d)))/sin(a/2)
    print(arctanarg)
    theta_star <- -1/a * (b + 2*atan(arctanarg))
    
    return(theta_star)
}

m <- initial_param_est[1]; a <- initial_param_est[2]; b <- initial_param_est[3]; d <- initial_param_est[4]
theta_star <- compute_theta_star(initial_param_est, 0, 0)
new_beta <- a*theta_star + b
first_drift_term <- a*d*tan(new_beta/2) 
print("theta star")
print(theta_star)

print("new drift (assumning integral = 0)")
print(first_drift_term)

print(initial_param_est[2]*theta_star)



