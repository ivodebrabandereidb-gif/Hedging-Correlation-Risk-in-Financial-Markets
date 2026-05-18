#------------------------- General Purpose -------------------------
# This file provides the results explained in section 1.2 of the thesis. 
# Furthermore, it constructs figure 1. The exact parameter settings to get to the figures are explained in the thesis.

library(MASS)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)

#-------------------------------
#Input variables
#-------------------------------

#Variables concerning the stocks
S_1 = 20
S_2 = 30
mu_1 = 0.05
mu_2 = 0.04
#mu_1 = -0.1
#mu_2 = -0.08
sigma_1 = 0.3
sigma_2 = 0.25
#sigma_1 = 0.4
#sigma_2 = 0.35

#Variables between stocks
#rho = 0.4
rho_values <- seq(-0.99, 0.99, by = 0.01)

#Variables concerning the options
K_a = 42
put_premium =0
#put_premium = 0

K_b = 16
WOput_premium = 0
#WOput_premium = 0

#1-confidence level for VaR and ES calculation
alpha = 0.05

#Variables regarding simulation settings
n = 100000
T = 1

#-------------------------------
#Initializing
#-------------------------------

VaR_1 <- numeric(length(rho_values))
VaR_2 <- numeric(length(rho_values))
VaR_3 <- numeric(length(rho_values))

ES_1 <- numeric(length(rho_values))
ES_2 <- numeric(length(rho_values))
ES_3 <- numeric(length(rho_values))


#-------------------------------
#Simulation and calculation loss, value-at-risk and expected shortfall.
#-------------------------------

set.seed(123)

Z <- matrix(rnorm(n*2), ncol = 2)
Z

#simulation from a multivariate normal distribution 
for (i in seq_along(rho_values)) {
  rho <- rho_values[i]
  R <- matrix(c(1, rho, rho, 1), nrow = 2, byrow = TRUE)
  
  # Cholesky factorization
  L <- t(chol(R))   
  X <- Z %*% t(L)
  
  Stocks_at_T <- data.frame(
    Stock_1 = numeric(n),
    Stock_2 = numeric(n)
  )
  
  
  Stocks_at_T$Stock_1 <- exp((mu_1-sigma_1^2/2) * T + X[,1] * sqrt(T) * sigma_1) * S_1
  Stocks_at_T$Stock_2 <- exp((mu_2-sigma_2^2/2) * T + X[,2] * sqrt(T) * sigma_2) * S_2 
  
  Stocks_at_T$sum_col <- Stocks_at_T$Stock_1 + Stocks_at_T$Stock_2
  Stocks_at_T$hedged <- pmax(K_a - Stocks_at_T$sum_col, 0) + Stocks_at_T$sum_col
  Stocks_at_T$worst <- pmin(Stocks_at_T$Stock_1,Stocks_at_T$Stock_2)
  Stocks_at_T$worst_put <- pmax(K_b - Stocks_at_T$worst, 0) + Stocks_at_T$sum_col
  
  loss_unhedged <- (S_1 + S_2) - Stocks_at_T$sum_col
  loss_hedged   <- (S_1 + S_2 + put_premium) - Stocks_at_T$hedged
  loss_WOhedged   <- (S_1 + S_2 + WOput_premium) - Stocks_at_T$worst_put 
  
  
  VaR_1[i] <- quantile(loss_unhedged, 1 - alpha)
  VaR_2[i] <- quantile(loss_hedged, 1 - alpha)
  VaR_3[i] <- quantile(loss_WOhedged, 1 - alpha)
  
  ES_1[i] <- mean(loss_unhedged[loss_unhedged >= quantile(loss_unhedged, 1 - alpha)])
  ES_2[i] <- mean(loss_hedged[loss_hedged >= quantile(loss_hedged, 1 - alpha)])
  ES_3[i] <- mean(loss_WOhedged[loss_WOhedged >= quantile(loss_WOhedged, 1 - alpha)])
  
}

#-------------------------------
#Putting results together.
#-------------------------------

results_df <- data.frame(
  rho = rho_values,
  VaR_1 = VaR_1,
  VaR_2 = VaR_2,
  VaR_3 = VaR_3
)

results_ES_df <- data.frame(
  rho = rho_values,
  ES_1 = ES_1,
  ES_2 = ES_2,
  ES_3 = ES_3
)

# Reshape data
plot_data_var <- results_df %>%
  pivot_longer(
    cols = c(VaR_1, VaR_2, VaR_3),
    names_to = "ID",
    values_to = "Value"
  ) %>%
  mutate(
    ID = recode(ID,
                "VaR_1" = "Unhedged Portfolio",
                "VaR_2" = "Hedged by index put option",
                "VaR_3" = "Hedged by worst-of put option"
    )
  )

plot_data_es <- results_ES_df %>%
  pivot_longer(
    cols = c(ES_1, ES_2, ES_3),
    names_to = "ID",
    values_to = "Value"
  ) %>%
  mutate(
    ID = recode(ID,
                "ES_1" = "Unhedged Portfolio",
                "ES_2" = "Hedged by index put option",
                "ES_3" = "Hedged by worst-of put option"
    )
  )

#-------------------------------
#Figure settings and plotting.
#-------------------------------

# Common y-axis limits
y_min <- min(c(plot_data_var$Value, plot_data_es$Value,0), na.rm = TRUE)
y_max <- max(c(plot_data_var$Value, plot_data_es$Value), na.rm = TRUE)

# Y-axis ticks every 0.1
y_breaks <- seq(
  from = floor(y_min * 10) / 10,
  to = ceiling(y_max * 10) / 10,
  by = 5
)

# Legend styling
my_colors <- c(
  "Unhedged Portfolio" = "#DD8A2E",
  "Hedged by index put option" = "#0B3D91",
  "Hedged by worst-of put option" = "#1FABD5"
)

my_linetypes <- c(
  "Unhedged Portfolio" = "solid",
  "Hedged by index put option" = "21",
  "Hedged by worst-of put option" = "solid"
)

# VaR plot
pl_var <- ggplot(
  plot_data_var,
  aes(x = rho, y = Value, color = ID, group = ID, linetype = ID)
) +
  geom_line(linewidth = 0.5) +
  scale_x_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(y_min, y_max),
    breaks = y_breaks,
    labels = number_format(accuracy = 0.1)
  ) +
  scale_color_manual(values = my_colors) +
  scale_linetype_manual(values = my_linetypes) +
  labs(
    x = "Correlation",
    y = "Value-at-Risk",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # straight labels
    plot.title = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# ES plot
pl_var_ES <- ggplot(
  plot_data_es,
  aes(x = rho, y = Value, color = ID, group = ID, linetype = ID)
) +
  geom_line(linewidth = 0.5) +
  scale_x_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(y_min, y_max),
    breaks = y_breaks,
    labels = number_format(accuracy = 0.1)
  ) +
  scale_color_manual(values = my_colors) +
  scale_linetype_manual(values = my_linetypes) +
  labs(
    x = "Correlation",
    y = "Expected shortfall",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # straight labels
    plot.title = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Combine plots
combined_plot <- pl_var + pl_var_ES +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(size = 12),
    plot.tag.position = c(0, 1)  # top-left corner
  )


# Display
combined_plot

#-------------------------------
#Saving figure.
#-------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("Figures")
# Save
ggsave(
  "Fig1_VaR_ES_Portfolios.png",
  plot = combined_plot,
  width = 8.5,
  height = 4.5,
  units = "in",
  dpi = 300
)