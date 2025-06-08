library(libstable4u)
library(yahoofinancer)
library(quantmod)
library(ggplot2)
library(gridExtra)
library(glue)
library(tseries)
library(dplyr)
library(StableEstim)
library(patchwork)
library(purrr)
library(ggfortify)

display_exchange_rate_ts <- function(df_list, titles, x, y) {
    plot_list <- list()
    
    for (i in seq_along(df_list)) {
        print(typeof(df))
        plot_list[[length(plot_list) + 1]] <- ggplot(df_list[[i]], aes(x = Date, y = Price)) +
                           geom_line(color = "blue", size = 0.5) +  # Line plot
                           labs(title = titles[i],
                                x = "Date",
                                y = "Exchange Rate")
    }
    
    combined_plot <- Reduce(`|`, plot_list)
    combined_plot
}

display_return_ts <- function(df_list, titles, x, y) {
    plot_list <- list()
    
    for (i in seq_along(df_list)) {
        print(typeof(df))
        plot_list[[length(plot_list) + 1]] <- ggplot(df_list[[i]], aes(x = Date, y = returns)) +
            geom_line(color = "blue", size = 0.2) +  # Line plot
            labs(title = titles[i],
                 x = "Date",
                 y = "Exchange Rate")
    }
    
    combined_plot <- Reduce(`|`, plot_list)
    combined_plot
}

get_Nolan_subset <- function(data) {
    #return(subset(data, Date >= as.Date('1980-01-02') & Date <= as.Date('1996-05-21')))
    return(subset(data, Date >= as.Date('2020-01-02') & Date <= as.Date('2025-01-09')))
}

gbp_usd <- read.csv("USDFull.csv")
gbp_aud <- read.csv("AUDFull.csv")
gbp_jpy <- read.csv("JPYFull.csv")

gbp_usd$Date <- as.Date(gbp_usd$Date,format="%d/%m/%Y")
gbp_aud$Date <- as.Date(gbp_aud$Date,format="%d/%m/%Y")
gbp_jpy$Date <- as.Date(gbp_jpy$Date,format="%d/%m/%Y")

Nolan_gbp_usd <- get_Nolan_subset(gbp_usd)
Nolan_gbp_aud <- get_Nolan_subset(gbp_aud)
Nolan_gbp_jpy <- get_Nolan_subset(gbp_jpy)
print("type")
print(typeof(Nolan_gbp_aud))

titles = list("GBP/USD", "GBP/AUD", "GBP/JPY")
display_exchange_rate_ts(list(Nolan_gbp_usd, Nolan_gbp_aud, Nolan_gbp_jpy), titles)




Nolan_gbp_usd <- Nolan_gbp_usd %>% mutate(returns = c(NA, diff(log(Nolan_gbp_usd[,2])))) %>% slice(-1)
Nolan_gbp_aud <- Nolan_gbp_aud %>% mutate(returns = c(NA, diff(log(Nolan_gbp_aud[,2])))) %>% slice(-1)
Nolan_gbp_jpy <- Nolan_gbp_jpy %>% mutate(returns = c(NA, diff(log(Nolan_gbp_jpy[,2])))) %>% slice(-1)

# Nolan_gbp_usd <- Nolan_gbp_usd[Nolan_gbp_usd$returns != 0, ]
# Nolan_gbp_aud <- Nolan_gbp_aud[Nolan_gbp_aud$returns != 0, ]
# Nolan_gbp_jpy <- Nolan_gbp_jpy[Nolan_gbp_jpy$returns != 0, ]

# SHOULD REMOVE ZEROS PROBOBLY AS THEY ARE LIKELY HOLIDAYS

display_return_ts(list(Nolan_gbp_usd, Nolan_gbp_aud, Nolan_gbp_jpy), titles)

get_daily_changes <- function(data) {
    log_returns <- diff(log(data[,2]))
    return(log_returns)
}

# Create ggplot ACF plots
acf1 <- autoplot(acf(Nolan_gbp_usd$returns, plot = FALSE, lag.max = 60)) + ggtitle("Autocorrelation of the GBP/USD log returns")
acf2 <- autoplot(acf((Nolan_gbp_usd$returns)^2, plot = FALSE, lag.max = 60)) + ggtitle("Autocorrelation of the square of the GBP/USD log returns")

acf1 | acf2




get_stable_ML_parameters <- function(data, initial_estimates, print_paramters=FALSE) {
    if (is.null(initial_estimates)) {
        pars_init <- stable_fit_init(data, parametrization  = 1L)
        pars_est_K <- stable_fit_koutrouvelis(data, pars_init=pars_init, parametrization  = 1L)
        pars_est_ML <- stable_fit_mle2d(data, pars_init=pars_est_K, parametrization = 1L)
        # print(pars_est_ML)
        # print("HERqE")
        #pars_est_ML <- Estim(EstimMethod = "ML", data, theta0 = pars_est_ML,
                             #ComputeCov = FALSE, HandleError = TRUE)
    }

    else {
        pars_est_ML <- stable_fit_mle2d(data, pars_init = initial_estimates, parametrization = 1L)
        #pars_est_ML <- MLParametersEstim(data, theta0 = initial_estimates, pm = 0)

    }

    if (print_paramters) {
        print(pars_est_ML)
    }

    return (pars_est_ML)
}

plot_normal_ML <- function(data, x_values) {
    mean <- mean(data)
    variance <- var(data)

    print("meanvar")

    print(mean)
    print(variance)

    stat_function(fun = dnorm, args = list(mean = mean, sd = sqrt(variance)), color = "blue", size = 0.6)
}


plot_stable_ML <- function(data, x_values, initial_est=NULL, print_parameters=FALSE) {
    pars_est_ML <- get_stable_ML_parameters(data, initial_est, print_parameters)
    stable_pdf_values <- stable_pdf(x_values, pars_est_ML)

    geom_line(aes(x = x_values, y = stable_pdf_values), color = "black", size = 0.8)
}

plot_fx_hist_analysis <- function(data, initial_est=NULL) {
    returns <- data$returns
    x_values <- seq(min(returns), max(returns), length.out = length(returns)) # Needs to be the same as the number of days

    ggplot(data, aes(x = returns)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
    plot_stable_ML(returns, x_values, initial_est, print_parameters=TRUE) +
    plot_normal_ML(returns)
    # stat_function(fun = dnorm, args = list(mean = mean, sd = sqrt(variance)), color = "black", size = 0.6)
}

plot_fx_qq_plot <- function(data) {
    ggplot(data, aes(sample = returns)) +
        stat_qq() +
        stat_qq_line(color = "red", linetype = "dotted")
}


test_stable_fit <- function(data, stable_parameters) {
    print("data check")
    noise <- rnorm(length(data), 0, 0.0000001)  # Perturbs the data slightly as ties are not allowed
    data <- data + noise
    p_value <- ks.test((data+noise), stable_cdf, stable_parameters)  # Takes 0 parametersisation

    print(p_value)

}


bootstrap_stable_estimate <- function(data, N) {
    par_estimates <- matrix(0, nrow=N, ncol=4)

    for (sample in 1:N) {
        bs_sample <- sample(data, size = length(data), replace = TRUE)
        par_estimate <- get_stable_ML_parameters(bs_sample, NULL)

        par_estimates[sample, ] <- par_estimate
    }

    means <- colMeans(par_estimates)
    
    percentiles <- apply(par_estimates, 2, function(col) {
        quantile(col, probs = c(0.025, 0.975))  # Calculate the 5th and 95th percentiles
    })
    
    print(means)
    print(percentiles)
}

# suppressWarnings({
plt1 <- plot_fx_hist_analysis(Nolan_gbp_usd) +
    labs(title = "GBP/USD",
         x = "Daily Log Proportional Change", y = "Density")


plt2 <- plot_fx_hist_analysis(Nolan_gbp_aud)  +
    # labs(title = "GBP/AUD daily changes from 13-12-1983 to 21-05-1996 with ML fitted stable/normal distributions.",
    #      x = "Daily Log Proportional Change", y = "Density")
    labs(title = "GBP/AUD",
         x = "Daily Log Proportional Change", y = "Density")


plt3 <- plot_fx_hist_analysis(Nolan_gbp_jpy)  +
    labs(title = "GBP/JPY",
         x = "Daily Log Proportional Change", y = "Density")

plt1 | plt2 | plt3

plt4 <- plot_fx_qq_plot(Nolan_gbp_usd) +
    labs(title = "GBP/USD Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles")
usd_shapiro <- shapiro.test(Nolan_gbp_usd$returns)
glue("The p-value for the shapiro-wilk test on the usd data is {usd_shapiro$p.value}")

plt5 <- plot_fx_qq_plot(Nolan_gbp_aud) +
    labs(title = "GBP/AUD Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles")
aud_shapiro <- shapiro.test(Nolan_gbp_aud$returns)
glue("The p-value for the shapiro-wilk test on the aud data is {aud_shapiro$p.value}")

plt6 <- plot_fx_qq_plot(Nolan_gbp_jpy) +
    labs(title = "GBP/JPY Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles")
jpy_shapiro <- shapiro.test(Nolan_gbp_jpy$returns)
glue("The p-value for the shapiro-wilk test on the jpy data is {jpy_shapiro$p.value}")

plt4 | plt5 | plt6


duplicates <- Nolan_gbp_usd[duplicated(Nolan_gbp_usd$returns) | duplicated(Nolan_gbp_usd$returns, fromLast = TRUE), ]
print(duplicates)

test_stable_fit(Nolan_gbp_usd$returns, c(1.731055, -1.932653e-01, 3.329531e-03, -2.128838e-05))
test_stable_fit(Nolan_gbp_aud$returns, c(1.742644, 1.048777e-01, 3.014181e-03, 5.977782e-05))
test_stable_fit(Nolan_gbp_jpy$returns, c(1.587013004, -0.091840629, 0.003595222, 0.000304176))


# bootstrap_stable_estimate(Nolan_gbp_usd$returns, 1000)
# bootstrap_stable_estimate(Nolan_gbp_aud$returns, 1000)
# bootstrap_stable_estimate(Nolan_gbp_jpy$returns, 1000)


# Confidence intervals for stable parameters (importantly we have delta interval containing 0)
# [1]  1.5778835324 -0.0121864586  0.0039088700 -0.0001186815
# [,1]        [,2]        [,3]          [,4]
# 5%  1.534203 -0.09921512 0.003782442 -0.0003507648
# 95% 1.623251  0.07061482 0.004028436  0.0001679826


# print("hi")
# skewness(Nolan_gbp_usd)
# print("hii")

