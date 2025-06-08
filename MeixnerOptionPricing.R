r <- 4.7/365
q <- 4.42/365

get_meixner_martingale_parameters <- function(pars, r, q) {  # no longer have r - q in the mu drift so don't multiply it by discounting
    mu <- pars[1]
    a <- pars[2]
    b <- pars[3]
    d <- pars[4]
    
    new_mu <- as.numeric(-2*d*(log(cos(b/2)) - log(cos((a+b)/2))))
    new_pars <- pars
    new_pars[1] <- new_mu
    
    return(new_pars)
}



print(get_meixner_martingale_parameters(usd_meixner_fit)) # m, a, b, d