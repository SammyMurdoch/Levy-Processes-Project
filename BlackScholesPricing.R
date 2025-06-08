aud_full_strike_prices <- seq(from = 1.8, to = 2.3, length.out = 51)

meixner_full_aud_prices <- c(0.29216416, 0.28225366, 0.27234316, 0.26246588, 0.25265548, 0.24284509,
                        0.2330347, 0.22322431, 0.21341391, 0.20360352, 0.19380113, 0.18409193,
                        0.17449342, 0.16501294, 0.15560297, 0.14642686, 0.1377587,  0.12922156,
                        0.12075421, 0.1124174, 0.10428086, 0.09634494, 0.08882673, 0.0819054,
                        0.07524061, 0.068954, 0.06332657, 0.05820808, 0.05334484, 0.04864108,
                        0.04417596, 0.03982108, 0.03576482, 0.0320426, 0.02846344, 0.02531696,
                        0.02254603, 0.01993155, 0.01747834, 0.0152239, 0.0130848, 0.01108268,
                        0.00912828, 0.00738852, 0.00594451, 0.00492758, 0.00407129, 0.00327044,
                        0.00254606, 0.00203314, 0.00171725)


usd_full_strike_prices <- seq(from = 1.25, to = 1.45, length.out = 51)

meixner_full_usd_prices <- c(0.09357446, 0.09006021, 0.08654596, 0.08304944, 0.0795967, 0.07622542,
                        0.0730091, 0.06986647, 0.06682338, 0.06384778, 0.06093254, 0.05805506,
                        0.05521538, 0.05246591, 0.04978939, 0.04714521, 0.04457818, 0.04214123,
                        0.03986205, 0.03769437, 0.0355379, 0.03345882, 0.03148956, 0.02958991,
                        0.02784003, 0.02620271, 0.02456539, 0.02297221, 0.02145423, 0.02001092,
                        0.01857681, 0.01721384, 0.015896, 0.01462136, 0.01342331, 0.01229272,
                        0.01120893, 0.01020523, 0.00931826, 0.00847963, 0.00770153, 0.00707842,
                        0.00651934, 0.00596025, 0.00542677, 0.0049115, 0.00447222, 0.00403293,
                        0.0036475, 0.00338512, 0.00314551)

jpy_full_strike_prices <- seq(from = 165, to = 210, length.out = 51)

meixner_full_jpy_prices <- c(25.16427111, 24.26752059, 23.38323314, 22.50478365, 21.63543293, 20.79613141,
                        19.96578926, 19.1529941,  18.34774873, 17.5488048,  16.76186046, 15.97528585,
                        15.19749203, 14.43187272, 13.67237992, 12.92400422, 12.19865915, 11.49397591,
                        10.81532796, 10.18698565, 9.5680078, 8.96144919, 8.35953375, 7.77906758,
                        7.23231031, 6.71235645, 6.21997329, 5.76214018, 5.30908084, 4.88554339,
                        4.47535975, 4.08703732, 3.75924788, 3.45608556, 3.16182625, 2.87819897,
                        2.60979638, 2.35737323, 2.1196156, 1.90629881, 1.70267248, 1.5131714,
                        1.35795562, 1.21412387, 1.09488592, 0.97898949, 0.87614046, 0.78463531,
                        0.69313015, 0.60262557, 0.52606648)



uk_q <- 3.91/100 # UK
us_r <- 4.25/100
aud_r <- 3.69/100
jpy_r <- 0.52/100


get_black_scholes_option_price <- function(sigma, r, q, K, T, X0) {
    d1 <- (log(X0/K) + (r-q+(1/2 * sigma^2))*T)/(sigma * sqrt(T))
    d2 <- d1 - sigma*sqrt(T)
    
    price <- X0*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
}


# generate_bs_price_graph <- function(sigma, r, q, T_min, T_max, T_step, k_min, k_max, k_step) {
#     k_values <- seq(k_min, k_max, by=k_step)
#     T_values <- seq(T_min, T_max, by=T_step)
#     
#     prices <- outer(k_values, T_values, Vectorize(function(K, T) get_black_scholes_option_price(sigma, r, q, K, T, 1)))
#     
#     plot_ly(x = k_values, y = T_values, z = prices, type = "surface")
# }


volatility_usd <- sqrt(var(Nolan_gbp_usd$returns)) * sqrt(252)
volatility_aud <- sqrt(var(Nolan_gbp_aud$returns)) * sqrt(252)
volatility_jpy <- sqrt(var(Nolan_gbp_jpy$returns)) * sqrt(252)


# print(volatility_usd)
# print(volatility_aud)
# print(volatility_jpy)

# generate_bs_price_graph(volatility_usd, r, q, 0.01, 1, 12, 0.1, 1.5, 0.005)
# print(get_black_scholes_option_price(volatility_usd, r, us_q, 1.29, 0.5, 1.35))


# 
# usd_K_vals <- seq(1.25, 1.45, by = 0.0005)
# usd_option_prices <- sapply(usd_K_vals, function(K) {
#     get_black_scholes_option_price(volatility_usd, r, us_q, K, 0.5, 1.35)
# })

# usd_option_price_df <- data.frame(
#     Strike = usd_K_vals,
#     OptionPrice = usd_option_prices
# )


usd_strike_prices <- c(1.25, 1.30, 1.32, 1.35, 1.38, 1.40, 1.45)
usd_market_prices <- c(0.101, 0.059, 0.043, 0.029, 0.017, 0.011, 0.004)

jpy_strike_prices <- c(168.7, 180.58, 185.27, 190.18, 195.43, 199.35, 208.32)
jpy_market_prices <- c(22.1, 11.9, 8.5, 5.5, 3.16, 1.97, 0.59)

usd_bs_model_prices <- sapply(usd_full_strike_prices, function(K) {
    get_black_scholes_option_price(volatility_usd, us_r, uk_q, K, 0.5, 1.35)
})

usd_bs_model_prices_few <- sapply(usd_strike_prices, function(K) {
    get_black_scholes_option_price(volatility_usd, us_r, uk_q, K, 0.5, 1.35)
})

aud_bs_model_prices <- sapply(aud_full_strike_prices, function(K) {
    get_black_scholes_option_price(volatility_aud, aud_r, uk_q, K, 0.5, 2.09)
})

aud_bs_model_prices_few <- sapply(aud_strike_prices, function(K) {
    get_black_scholes_option_price(volatility_aud, aud_r, uk_q, K, 0.5, 2.09)
})

jpy_bs_model_prices <- sapply(jpy_full_strike_prices, function(K) {
    get_black_scholes_option_price(volatility_jpy, jpy_r, uk_q, K, 0.5, 193.91)
})

jpy_bs_model_prices_few <- sapply(jpy_strike_prices, function(K) {
    get_black_scholes_option_price(volatility_jpy, jpy_r, uk_q, K, 0.5, 193.91)
})




meixner_usd_prices <- c(0.1066567, 0.06684712, 0.05355229, 0.03743363, 0.02522726, 0.01910851,
                       0.00956494)  # Update this for 1000 simulations

meixner_jpy_prices <- c(21.92415041, 11.5824136, 8.45253663, 5.80871875, 3.66368946, 2.51433732,
                        0.93630807)



plot(meixner_aud_prices)

# NOTE THAT THE NIG PRICES HAVENT BEEN ALTERED WITH THE INTEREST RATE FIXED

# aus high of 2.3
# aus low of 1.8

# plot_option_price_curve <- function(strike_prices, market_prices, bs_prices, nig_prices, meixner_prices) {
#     option_price_df <- data.frame(
#         Strike = rep(strike_prices, 4),
#         Price = c(market_prices, bs_prices, nig_prices, meixner_prices),
#         Type = rep(c("Market Price", "BS Model", "NIG Model", "Meixner Model"), each = length(strike_prices)))
#     
#     ggplot(option_price_df, aes(x = Strike, y = Price, color = Type, shape = Type)) +
#         geom_point(size = 3) +
#         geom_line() +
#         labs(
#             title = "Market vs Model Prices",
#             x = "Strike Price",
#             y = "Option Price"
#         ) +
#         scale_color_manual(values = c("Market Price" = "red", "BS Model" = "blue", "NIG Model"="darkgreen", "Meixner Model"="cyan"))
# }

plot_option_price_curve <- function(
        market_strike_prices, market_prices,
        model_strike_prices, bs_prices, nig_prices, meixner_prices, title
) {
    # Market price data (short)
    market_df <- data.frame(
        Strike = market_strike_prices,
        Price = market_prices,
        Line = "Market Price"
    )
    
    # Model price data (long)
    bs_df <- data.frame(
        Strike = model_strike_prices,
        Price = bs_prices,
        Line = "BS Model"
    )
    
    nig_df <- data.frame(
        Strike = model_strike_prices,
        Price = nig_prices,
        Line = "NIG Model"
    )
    
    meixner_df <- data.frame(
        Strike = model_strike_prices,
        Price = meixner_prices,
        Line = "Meixner Model"
    )
    
    # Combine all data
    option_price_df <- rbind(market_df, bs_df, nig_df, meixner_df)
    
    # Plot
    ggplot(option_price_df, aes(x = Strike, y = Price, color = Line, shape = Line)) +
        geom_point(data = subset(option_price_df, Line == "Market Price"), size = 3) +  # Market: points
        geom_line(data = subset(option_price_df, Line == "Market Price"), size = 1) +   # Market: line
        geom_line(data = subset(option_price_df, Line != "Market Price"), size = 1) +   # Models: lines
        labs(
            title = title,
            x = "Strike Price",
            y = "Option Price"
        ) +
        scale_color_manual(values = c(
            "Market Price" = "red",
            "BS Model" = "blue",
            "NIG Model" = "darkgreen",
            "Meixner Model" = "cyan"
        ))
}


plot_option_price_curve_no_market <- function(
        model_strike_prices, bs_prices, nig_prices, meixner_prices
) {
    # Model price data (long)
    bs_df <- data.frame(
        Strike = model_strike_prices,
        Price = bs_prices,
        Line = "BS Model"
    )
    
    nig_df <- data.frame(
        Strike = model_strike_prices,
        Price = nig_prices,
        Line = "NIG Model"
    )
    
    meixner_df <- data.frame(
        Strike = model_strike_prices,
        Price = meixner_prices,
        Line = "Meixner Model"
    )
    
    # Combine all model data
    option_price_df <- rbind(bs_df, nig_df, meixner_df)
    
    # Plot: only model lines
    ggplot(option_price_df, aes(x = Strike, y = Price, color = Line)) +
        geom_line(size = 1) +
        labs(
            title = "GBP/AUD Model Option Price Curves",
            x = "Strike Price",
            y = "Option Price"
        ) +
        scale_color_manual(values = c(
            "BS Model" = "blue",
            "NIG Model" = "darkgreen",
            "Meixner Model" = "red"
        )
        )
}



usd_price_plot <- plot_option_price_curve(usd_strike_prices, usd_market_prices, usd_full_strike_prices, usd_bs_model_prices, usd_nig_model_prices, meixner_full_usd_prices, "GBP/USD Market vs Model Prices")
aud_price_plot <- plot_option_price_curve_no_market(aud_full_strike_prices, aud_bs_model_prices, aud_nig_model_prices, meixner_full_aud_prices)
jpy_price_plot <- plot_option_price_curve(jpy_strike_prices, jpy_market_prices, jpy_full_strike_prices, jpy_bs_model_prices, jpy_nig_model_prices, meixner_full_jpy_prices, "GBP/JPY Market vs Model Prices")

usd_price_plot | jpy_price_plot
aud_price_plot

print("USD RMSES")
print(sqrt(mean((usd_market_prices-usd_bs_model_prices_few)^2)))
print(sqrt(mean((usd_market_prices-usd_few_model_prices)^2)))
print(sqrt(mean((usd_market_prices-meixner_usd_prices)^2)))

print("JPY RMSEs")
print(sqrt(mean((jpy_market_prices-jpy_bs_model_prices_few)^2)))
print(sqrt(mean((jpy_market_prices-jpy_few_model_prices)^2)))
print(sqrt(mean((jpy_market_prices-meixner_jpy_prices)^2)))

print("USD ARPEs")
mean(abs((usd_market_prices-usd_bs_model_prices_few)/usd_market_prices))
mean(abs((usd_market_prices-usd_few_model_prices)/usd_market_prices))
mean(abs((usd_market_prices-meixner_usd_prices)/usd_market_prices))

print("JPY ARPEs")
mean(abs((jpy_market_prices-jpy_bs_model_prices_few)/jpy_market_prices))
mean(abs((jpy_market_prices-jpy_few_model_prices)/jpy_market_prices))
mean(abs((jpy_market_prices-meixner_jpy_prices)/jpy_market_prices))

# print(abs((usd_market_prices-meixner_usd_prices)/usd_market_prices))
# print(abs((usd_market_prices-usd_few_model_prices)/usd_market_prices))
# 
# print(meixner_usd_prices)
# print(usd_few_model_prices)

# plot_option_price_curve(usd_full_strike_prices, usd_market_prices, usd_bs_model_prices, usd_nig_model_prices, meixner_usd_prices)
# plot_option_price_curve(jpy_strike_prices, jpy_market_prices, jpy_bs_model_prices, jpy_nig_model_prices, meixner_jpy_prices)