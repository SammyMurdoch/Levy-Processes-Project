library(tidyr)

N <- 1000

dist1_pars <- c(0.5, 1,  0.75, 0)
dist2_pars <- c(1, 0,  0.75, 0)
dist3_pars <- c(2, 0,  1, 0)

stable_x_vals <- seq(-5, 5, length.out = N)
stable1_pdf_values <- stable_pdf(stable_x_vals, dist1_pars, parametrization = 1L)
stable2_pdf_values <- stable_pdf(stable_x_vals, dist2_pars, parametrization = 1L)
stable3_pdf_values <- stable_pdf(stable_x_vals, dist3_pars, parametrization = 1L)


stable_pdf_df <- data.frame(x_values=stable_x_vals,
                            values1=stable1_pdf_values,
                            values2=stable2_pdf_values,
                            values3=stable3_pdf_values)
ggplot(stable_pdf_df, aes(x = x_values)) +
    geom_line(aes(y = values1, color = "Line 1"), size = 0.6) +
    geom_line(aes(y = values2, color = "Line 2"), size = 0.6) +
    geom_line(aes(y = values3, color = "Line 3"), size = 0.6) +
    labs(title = "Pdfs of Different Stable Distributions",
         color = "Distribution") +
    scale_color_manual(
        values = c("Line 1" = "blue", "Line 2" = "darkgreen", "Line 3" = "red"),
        breaks = c("Line 1", "Line 2", "Line 3"),
        labels = c(expression(alpha == 0.5 * "," ~ beta == 1 * "," ~ gamma == 0.75),
                   expression(alpha == 1 * "," ~ beta == 0 * "," ~ gamma == 0.75),
                   expression(alpha == 2 * "," ~ beta == 0 * "," ~ gamma == 1))) + 
    theme(
        legend.position = c(1, 1),  # inside top-right of plot
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(fill = "white", color = "black"),  # full box
        legend.background = element_blank(),  # don't interfere
        legend.key = element_rect(fill = NA),  # no background behind key symbols
        legend.margin = margin(6, 20, 6, 6),  # spacing inside the white box
        axis.title = element_blank()       # remove axis titles
        
    )