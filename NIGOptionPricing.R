library(plotly)

uk_q <- 3.91/100 # UK
uk_q <- (1 + uk_q)^(1/252) - 1

us_r <- 4.25/100
us_r <- (1 + us_r)^(1/252) - 1

aud_r <- 3.69/100
aud_r <- (1 + aud_r)^(1/252) - 1

jpy_r <- 0.52/100
jpy_r <- (1 + jpy_r)^(1/252) - 1


# AHAHAHA Data increments is on the scale of days, interest rates are on the scale of years
# Double AH HAHAHAHAHA, r, q are percentages and, therefore, should be divided by 100

# Note that the interest rate we want is for years. The time to maturity is on on the scale of years
# Need to use annualised volatility for black scholes

# The dometic market is actually the non GBP currency


get_nig_martingale_parameters <- function(pars, r, q) {  # no longer have r - q in the mu drift so don't multiply it by discounting
    mu <- pars["mu"]
    a <- pars["alpha"]
    b <- pars["beta"]
    d <- pars["delta"]
    
    new_mu <- as.numeric(d*(sqrt(a^2 - (b+1)^2) - sqrt(a^2-b^2)))
    new_pars <- pars
    new_pars["mu"] <- new_mu
    
    return(new_pars)
}

calculate_nig_call_option_price <- function(pars, r, q, X0, K, T, N) {
    # print("pars")
    # print(pars)
    new_mu <- pars["mu"]*T
    new_delta <- pars["delta"]*T
    exchange_rate_final_vals <- X0*exp(rnig(N, mu=new_mu, delta=new_delta, beta=pars["beta"], alpha=pars["alpha"]))
    discounted_k <- K*exp(T*(q-r))

    payoffs <- pmax(exchange_rate_final_vals-discounted_k, 0)

    mean(payoffs)
    
    
    # payoff_sum <- 0
    # print(K)
    # for (i in 1:100) {
    #     final_lp_value <- 0
    # 
    #     for (j in 1:T) {
    #         final_lp_value <- final_lp_value + rnig(1, mu=new_mu, delta=new_delta, beta=pars["beta"], alpha=pars["alpha"])
    #     }
    #     
    #     exchange_rate_final_val2 <- X0 * exp(final_lp_value)
    #     payoff_sum <- payoff_sum + max(exchange_rate_final_val2-discounted_k, 0)
    # }
    # return(payoff_sum/100)
}

generate_price_graph <- function(pars, X0, r, q, T_min, T_max, T_step, k_min, k_max, k_step, N) {
    k_values <- seq(k_min, k_max, by=k_step)
    T_values <- seq(T_min, T_max, by=T_step)
    
    prices <- outer(k_values, T_values, Vectorize(function(K, T) calculate_nig_call_option_price(pars, r, q, X0, K, T, N)))
    
    plot_ly(x = k_values, y = T_values, z = prices, type = "surface")
}


usd_nig_martingale_pars <- get_nig_martingale_parameters(usd_nig_pars, ud_r, uk_q)
aud_nig_martingale_pars <- get_nig_martingale_parameters(aud_nig_pars, aud_r, uk_q)
jpy_nig_martingale_pars <- get_nig_martingale_parameters(jpy_nig_pars, jpy_r, uk_q)
print("from here")

print(usd_nig_martingale_pars)
print(aud_nig_martingale_pars)
print(jpy_nig_martingale_pars)

usd_X0 <- 1.35

usd_strike_prices <- c(1.25, 1.30, 1.32, 1.35, 1.38, 1.40, 1.45)
usd_few_model_prices <- sapply(usd_strike_prices, function(K) {
    calculate_nig_call_option_price(usd_nig_martingale_pars, us_r, uk_q, usd_X0, K, 126, 10000)
})

usd_nig_model_prices <- sapply(usd_full_strike_prices, function(K) {
    calculate_nig_call_option_price(usd_nig_martingale_pars, us_r, uk_q, usd_X0, K, 126, 10000)
})

aud_X0 <- 2.09

aud_nig_model_prices <- sapply(aud_full_strike_prices, function(K) {
    calculate_nig_call_option_price(aud_nig_martingale_pars, aud_r, uk_q, aud_X0, K, 126, 100000)
})

jpy_X0 <- 193.91

jpy_strike_prices <- c(168.7, 180.58, 185.27, 190.18, 195.43, 199.35, 208.32)

jpy_few_model_prices <- sapply(jpy_strike_prices, function(K) {
    calculate_nig_call_option_price(jpy_nig_martingale_pars, jpy_r, uk_q, jpy_X0, K, 126, 10000)
})

jpy_nig_model_prices <- sapply(jpy_full_strike_prices, function(K) {
    calculate_nig_call_option_price(jpy_nig_martingale_pars, jpy_r, uk_q, jpy_X0, K, 126, 100000)
})

# generate_price_graph(aud_nig_martingale_pars, X0, r, q, 5, 365, 12, 0.5, 1.5, 0.005, 10000)
# generate_price_graph(usd_nig_martingale_pars, X0, r, q, 5, 365, 12, 0.5, 1.5, 0.005, 10000)
# generate_price_graph(jpy_nig_martingale_pars, x0, r, q, 5, 365, 12, 0.5, 1.5, 0.005, 10000)