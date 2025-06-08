library(ggpubr)

# For historical data
# usd_stable_pars <- c(1.5841320038, 0.0055478219, 0.0039250669, -0.0001298384)
# aud_stable_pars <- c(1.6675651952, 0.1065047547, 0.0051083961, -0.0002097653)
# jpy_stable_pars <- c(1.5861452609, -0.1575227162,  0.0039153354, -0.0002029295)

# For more recent data excluding zeros
# usd_stable_pars <- c(1.734981, -0.2044126, 0.003349159, 0.00004129982)
# aud_stable_pars <- c(1.7496885868, 0.0778006562, 0.0030282873, 0.0001189809)
# jpy_stable_pars <- c(1.587013004, -0.091840629,  0.003595222, 0.000304176)

# usd_stable_pars <- c(1.731055, -0.1932653, 0.003329531, 0.00002128838)
# aud_stable_pars <- c(1.742644, 0.1048777, 0.003014181, 0.00005977782)
# jpy_stable_pars <- c(1.587013004, -0.091840629,  0.003595222, 0.000304176)

usd_stable_pars <- c(1.731055e+00, -1.932653e-01,  3.329531e-03, -2.128838e-05) # 0L
aud_stable_pars <- c(1.742644e+00, 1.048777e-01, 3.014181e-03, 5.977782e-05)
jpy_stable_pars <- c(1.587013004, -0.091840629,  0.003595222,  0.000304176)


p_usd <- seq(0, 1, length.out=length(Nolan_gbp_usd$returns))
p_aud <- seq(0, 1, length.out=length(Nolan_gbp_aud$returns))
p_jpy <- seq(0, 1, length.out=length(Nolan_gbp_jpy$returns))

# p <- seq(0, 1, length.out=10)


usd_quantiles <- stable_q(p_usd, usd_stable_pars, parametrization = 0L, tol = 1e-12)
aud_quantiles <- stable_q(p_aud, aud_stable_pars, parametrization = 0L, tol = 1e-12)
jpy_quantiles <- stable_q(p_jpy, jpy_stable_pars, parametrization = 0L, tol = 1e-12)
print(length(Nolan_gbp_usd$returns))
print(length(Nolan_gbp_aud$returns))
print(length(Nolan_gbp_jpy$returns))

usd_returns <- Nolan_gbp_usd$returns
qq_usd_data <- data.frame(usd_quantiles, usd_returns)

plt1 <- ggplot(qq_usd_data, aes(x = usd_quantiles, y = sort(usd_returns))) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Reference line
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "GBP/USD Q-Q Plot")



aud_returns <- Nolan_gbp_aud$returns
qq_aud_data <- data.frame(aud_quantiles, aud_returns)

plt2 <- ggplot(qq_aud_data, aes(x = aud_quantiles, y = sort(aud_returns))) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Reference line
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "GBP/AUD Q-Q Plot")



jpy_returns <- Nolan_gbp_jpy$returns
qq_jpy_data <- data.frame(jpy_quantiles, jpy_returns)

plt3 <- ggplot(qq_jpy_data, aes(x = jpy_quantiles, y = sort(jpy_returns))) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Reference line
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "GBP/JPY Q-Q Plot")

plt1 | plt2 | plt3












# ggplot(qq_data, aes(y = usd_returns)) +
#     stat_qq(aes(x = usd_quantiles))+
#     #geom_point() +
#     geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")  # Reference line



# qqplot(usd_quantiles, Nolan_gbp_usd$returns,
#        main = "GDP/USD Stable Distribution QQ Plot",
#        xlab = "Theoretical Quantiles",
#        ylab = "Sample Quantiles")
# abline(0, 1, col = "red", lwd = 2)
# 
# qqplot(aud_quantiles, Nolan_gbp_aud$returns,
#        main = "GDP/AUD Stable Distribution QQ Plot",
#        xlab = "Theoretical Quantiles",
#        ylab = "Sample Quantiles")
# abline(0, 1, col = "red", lwd = 2)
# 
# qqplot(jpy_quantiles, Nolan_gbp_jpy$returns,
#        main = "GDP/JPY Stable Distribution QQ Plot",
#        xlab = "Theoretical Quantiles",
#        ylab = "Sample Quantiles")
# abline(0, 1, col = "red", lwd = 2)