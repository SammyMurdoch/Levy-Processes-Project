# x_values <- seq(-10, 10, length.out = 1000)
# pars <- c(1.5, 0.9, 1, 0)
# stable_pdf_values <- stable_pdf(x_values, pars)
# 
# plot(x_values, stable_pdf_values)
# sample <- stable_rnd(10000, c(1.5, 0.9, 1, 0), parametrization = 0L)
# 
# ggplot(data.frame(datapoints=sample), aes(x = datapoints)) +
#     labs(title = "Stable simulated GBP/USD exchange rate return histogram.",
#          x = "Daily Log Proportional Change", y = "Density") +
#     geom_histogram(aes(y = ..density..), bins = 75, fill = "lightblue", color = "black")

qnorm(1/8, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
