library(GeneralizedHyperbolic)

usd_data <- Nolan_gbp_usd$returns
aud_data <- Nolan_gbp_aud$returns
jpy_data <- Nolan_gbp_jpy$returns

usd_norminvgaufit <- nigFit(usd_data)
aud_norminvgaufit <- nigFit(aud_data)
jpy_norminvgaufit <- nigFit(jpy_data)

usd_nig_pars <- usd_norminvgaufit$param
aud_nig_pars <- aud_norminvgaufit$param
jpy_nig_pars <- jpy_norminvgaufit$param


print(usd_norminvgaufit$param)
print(aud_norminvgaufit$param)
print(jpy_norminvgaufit$param)

usd_nig_x_vals <- seq(min(usd_data), max(usd_data), length.out = length(usd_data))
usd_nig_pdf_vals <- dnig(usd_nig_x_vals, alpha = usd_nig_pars["alpha"], beta = usd_nig_pars["beta"], 
                 delta = usd_nig_pars["delta"], mu = usd_nig_pars["mu"])

usd_nig_plot <- ggplot(data.frame(Nolan_gbp_usd), aes(x = returns)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
    labs(title = "GBP/USD",
         x = "Daily Log Proportional Change", y = "Density") +
    geom_line(aes(x = usd_nig_x_vals, y = usd_nig_pdf_vals), color = "blue", size = 0.6)


aud_nig_x_vals <- seq(min(aud_data), max(aud_data), length.out = length(aud_data))
aud_nig_pdf_vals <- dnig(aud_nig_x_vals, alpha = aud_nig_pars["alpha"], beta = aud_nig_pars["beta"], 
                     delta = aud_nig_pars["delta"], mu = aud_nig_pars["mu"])

aud_nig_plot <- ggplot(data.frame(Nolan_gbp_aud), aes(x = returns)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
    labs(title = "GBP/AUD",
         x = "Daily Log Proportional Change", y = "Density") +
    geom_line(aes(x = aud_nig_x_vals, y = aud_nig_pdf_vals), color = "blue", size = 0.6)


jpy_nig_x_vals <- seq(min(jpy_data), max(jpy_data), length.out = length(jpy_data))
jpy_nig_pdf_vals <- dnig(jpy_nig_x_vals, alpha = jpy_nig_pars["alpha"], beta = jpy_nig_pars["beta"], 
                     delta = jpy_nig_pars["delta"], mu = jpy_nig_pars["mu"])

jpy_nig_plot <- ggplot(data.frame(Nolan_gbp_jpy), aes(x = returns)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
    labs(title = "GBP/JPY",
         x = "Daily Log Proportional Change", y = "Density") +
    geom_line(aes(x = jpy_nig_x_vals, y = jpy_nig_pdf_vals), color = "blue", size = 0.6)


usd_nig_plot |aud_nig_plot |jpy_nig_plot


get_m_new_bar <- function(pars) {
    m <- pars["mu"]
    a <- pars["alpha"]
    b <- pars["beta"]
    d <- pars["delta"]
    
    m_new_bar <- m + d * (sqrt(a^2 - (b+1)^2) - sqrt(a^2-b^2))
    
}


# daily returns from 02-01-2020 to 09-01-2025 with ML fitted NIG distribution.

print(get_m_new_bar(usd_nig_pars))
print(get_m_new_bar(aud_nig_pars))
print(get_m_new_bar(jpy_nig_pars))