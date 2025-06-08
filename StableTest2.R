# The goodness of fit paper relies on the L1 parameterisation

sqcv <- function(data, a, b) {
    n <- length(data)
    na <- floor(a * n)
    nb <- floor(b * n)
    sorted_data <- sort(data)
    quantile_slice <- sorted_data[na:nb]

    return(var(quantile_slice))
}

test_statistic_N <- function(data, a_vec, b_vec, d_vec, pars) {
    n <- length(data)

    t1 <- d_vec[1] * sqcv(data, a_vec[1], b_vec[1])
    t2 <- d_vec[2] * sqcv(data, a_vec[2], b_vec[2])
    t3 <- d_vec[3] * sqcv(data, a_vec[3], b_vec[3])

    t_vec <- c(t1, t2, t3)

    theoretical_sqcvs <- numeric(length(d_vec)+1)

    for(i in 1:length(d_vec)) {
        theoretical_sqcvs[i] <- estimate_distribution_sqcv(pars, a[i], b[i])
    }
    theoretical_sqcvs[length(d_vec)+1] <- estimate_distribution_sqcv(pars, a[1], tail(b, n=1))
    print(theoretical_sqcvs)
    print(t_vec)
    total <- 0

    for(i in 1:(length(theoretical_sqcvs)-1)) {
        total <- total + (t_vec[i]-d_vec[i]*theoretical_sqcvs[i])
    }
    total <- sqrt(n) * total/tail(theoretical_sqcvs, n=1)
    print(total)

    return(total)
}

estimate_distribution_sqcv <- function(pars, a, b) {
    # This is correct for alpha>=1
    quantile_values <- stable_q(c(a, b), pars,  parametrization = 1L)
    inv_a <- quantile_values[1]
    inv_b <- quantile_values[2]


    t1_integrand <- function(x) {
        x^2 * stable_pdf(x, pars, parametrization=1L)
    }


    t1 <- (1/(b-a)) * integrate(t1_integrand, lower=inv_a, upper=inv_b)$value

    t2_integrand <- function(x) {
        x * stable_pdf(x, pars, parametrization=1L)
    }

    t2 <- ((1/(b-a)) * integrate(t2_integrand, lower=inv_a, upper=inv_b)$value)^2

    return(t1-t2)
}

print("varianc3")
print(Nolan_gbp_usd)

# print("normal estimte")
# print(estimate_distribution_sqcv(c(2, 0, sqrt(2)/2, 0), 0, 1))
# print("end")

test_stat_var_estimate <- function(pars, a, b, d, sample_count=2000000, N=200) {
    differences <- numeric(N)

    theoretical_sqcvs <- numeric(length(d)+1)

    for(i in 1:length(d)) {
        theoretical_sqcvs[i] <- estimate_distribution_sqcv(pars, a[i], b[i])
    }
    theoretical_sqcvs[length(d)+1] <- estimate_distribution_sqcv(pars, a[1], tail(b, n=1))

    stable_samples <- sample(stable_rnd(sample_count*N, pars, parametrization = 1L))

    for(difference in 1:N) {
        total <- 0
        for(i in 1:length(d)) {
            lower <- sample_count*(difference-1) + 1
            upper <- sample_count*difference
            sample_sqcv <- sqcv(stable_samples[lower:upper], a[i], b[i])
            total <- total + d[i] * (sample_sqcv - theoretical_sqcvs[i])
        }
        total <- total * sqrt(sample_count)
        total <- total / tail(theoretical_sqcvs, n=1)

        differences[difference] = total
    }

    print(length(differences))
    print("above length")
    plot(differences)
    hist(differences, breaks=10)

    difference_variance <- var(differences)
    difference_mean <- mean(differences)
    print("here")
    print(difference_mean)

    return(difference_variance)
}

get_stable_test_statistic <- function(test_pars) {
    tau <- tau_estimate(pars)
}

# print(var(Nolan_gbp_usd$returns))
# print("yrd")
# print(var(Nolan_gbp_usd$returns)/estimate_distribution_sqcv(c(1.731, 0, 0.00333, 0), 0.01, 0.99))


estimated_parameters <- c(1.731055e+00, 0,  3.329531e-03, -2.128838e-05)
estimated_parameters <- c(1.74, 0,  0.00301, 0.000195)


#print(estimated_parameters)

a1 <- 0.005
a2 <- 0.25

a <- c(a1, a2, 1-a2)
b <- c(a2, 1-a2, 1-a1)

d <- c(0.6, -1.61, 0.6)
print("test stat")
print(test_statistic_N(Nolan_gbp_aud$returns, a, b, d, estimated_parameters))


var_estimate <- test_stat_var_estimate(estimated_parameters, a, b, d)
print(var_estimate)
print(sqrt(var_estimate)*qnorm(0.975))

