library(moments)
library(nleqslv)
library(pracma)

usd_data <- Nolan_gbp_usd$returns
aud_data <- Nolan_gbp_aud$returns
jpy_data <- Nolan_gbp_jpy$returns

f <- function(vars, variance, kurtosis) {
    a <- vars[1]
    d <- vars[2]
    
    c(1/2 * a^2*d - variance,
      3 + 1/d - kurtosis)
    
}

fit_symmetric_meixner_model <- function(intial_guess, data) {
    variance <- var(data)
    kurtosis <- kurtosis(data)
    result <- nleqslv(initial_guess, f, variance=variance, kurtosis=kurtosis)$x
    
    param_est <- c(0, result[1], 0, result[2])
}

usd_meixner_fit <- fit_symmetric_meixner_model(c(0.01, 0.3), usd_data)
aud_meixner_fit <- fit_symmetric_meixner_model(c(0.01, 0.3), aud_data)
jpy_meixner_fit <- fit_symmetric_meixner_model(c(0.01, 0.3), jpy_data)


print(usd_meixner_fit)
print(aud_meixner_fit)
print(jpy_meixner_fit)


display_meixner_fit <- function(data, fitted_params, nig_x_vals, nig_pdf, title_str) {
    x_vals <- seq(min(data$returns), max(data$returns), length.out = length(data$returns))
    pdf_vals <- meixner_density(x_vals,
                                fitted_params[1],
                                fitted_params[2],
                                fitted_params[3],
                                fitted_params[4])
    
    print("LOOK TighT herE")
    print(length(x_vals))
    print(length(nig_pdf))

    ggplot(data.frame(data), aes(x = returns)) +
        geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
        labs(title = title_str,
             x = "Daily Log Proportional Change", y = "Density") +

        geom_line(aes(x = x_vals, y = pdf_vals), color = "blue", size = 0.6) +
        geom_line(aes(x = nig_x_vals, y = nig_pdf), color = "black", size = 0.6)
    
}

usd_title <- "GBP/USD"
usd_nig_meixner_plot <- display_meixner_fit(Nolan_gbp_usd, usd_meixner_fit, usd_x_vals, usd_nig_pdf_vals, usd_title)

aud_title <- "GBP/AUD"
aud_nig_meixner_plot <- display_meixner_fit(Nolan_gbp_aud, aud_meixner_fit, aud_x_vals, aud_nig_pdf_vals, aud_title)

jpy_title <- "GBP/JPY"
jpy_nig_meixner_plot <- display_meixner_fit(Nolan_gbp_jpy, jpy_meixner_fit, jpy_x_vals, jpy_nig_pdf_vals, jpy_title)

usd_nig_meixner_plot | aud_nig_meixner_plot | jpy_nig_meixner_plot



# "GBP/USD daily changes from 02-01-2020 to 09-01-2025 with ML fitted Meixner distribution."


print("m new bar")
print(2*0.22649881*log(cos(0.01704519/2)))
print(2*0.641313035*log(cos(0.008672299/2)))
print(2*0.3778890*log(cos(0.0145535/2)))
