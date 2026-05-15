library(MASS)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)

#Varaibles
#stocks
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

#between stocks
#rho = 0.4
rho_values <- seq(-0.99, 0.99, by = 0.01)

#option
strike = S_1 + S_2
put_premium = 7
#put_premium = 0

WOstrike = S_1
WOput_premium = 4.5
#WOput_premium = 0

#VaR
alpha = 0.05

#simulation settings
n = 100000
T = 1

VaR_1 <- numeric(length(rho_values))
VaR_2 <- numeric(length(rho_values))
VaR_3 <- numeric(length(rho_values))

ES_1 <- numeric(length(rho_values))
ES_2 <- numeric(length(rho_values))
ES_3 <- numeric(length(rho_values))


# ======================option 1 =========================================
set.seed(123)

Z <- matrix(rnorm(n * 2), ncol = 2)
Z

#simulation from a multivariate normal distribution 
for (i in seq_along(rho_values)) {
  rho <- rho_values[i]
  R <- matrix(c(1, rho,
                rho, 1), nrow = 2, byrow = TRUE)
  
  # Cholesky factor
  L <- t(chol(R))   # lower triangular
  X <- Z %*% t(L)
  
  T_year_return <- data.frame(
    Stock_1 = numeric(n),
    Stock_2 = numeric(n)
  )
  
  
  T_year_return$Stock_1 <- exp((mu_1-sigma_1^2/2) * T + X[,1] * sqrt(T) * sigma_1) * S_1
  T_year_return$Stock_2 <- exp((mu_2-sigma_2^2/2) * T + X[,2] * sqrt(T) * sigma_2) * S_2 
  
  T_year_return$sum_col <- T_year_return$Stock_1 + T_year_return$Stock_2
  T_year_return$hedged <- pmax(strike - T_year_return$sum_col, 0) + T_year_return$sum_col
  T_year_return$worst <- pmin(T_year_return$Stock_1,
                              T_year_return$Stock_2)
  T_year_return$worst_put <- pmax(WOstrike - T_year_return$worst, 0) + T_year_return$sum_col
  
  #T_year_return$sum_col <- T_year_return$sum_col / (S_1 + S_2)
  #T_year_return$hedged <- T_year_return$hedged / (S_1 + S_2 + put_premium)
  #T_year_return$hedgedWO <- T_year_return$worst_put / (S_1 + S_2 + WOput_premium) 
  
  #T_year_return$sum_col <- T_year_return$sum_col - (S_1 + S_2)
  #T_year_return$hedged <- T_year_return$hedged - (S_1 + S_2 + put_premium)
  #T_year_return$hedgedWO <- T_year_return$worst_put - (S_1 + S_2 + WOput_premium)
  
  #loss_unhedged <- 1 - T_year_return$sum_col
  #loss_hedged   <- 1 - T_year_return$hedged
  #loss_WOhedged   <- 1 - T_year_return$hedgedWO
  
  loss_unhedged <- (S_1 + S_2) - T_year_return$sum_col
  loss_hedged   <- (S_1 + S_2 + put_premium) - T_year_return$hedged
  loss_WOhedged   <- (S_1 + S_2 + WOput_premium) - T_year_return$worst_put 
  
  
  VaR_1[i] <- quantile(loss_unhedged, 1 - alpha)
  VaR_2[i] <- quantile(loss_hedged, 1 - alpha)
  VaR_3[i] <- quantile(loss_WOhedged, 1 - alpha)
  
  ES_1[i] <- mean(loss_unhedged[loss_unhedged >= quantile(loss_unhedged, 1 - alpha)])
  ES_2[i] <- mean(loss_hedged[loss_hedged >= quantile(loss_hedged, 1 - alpha)])
  ES_3[i] <- mean(loss_WOhedged[loss_WOhedged >= quantile(loss_WOhedged, 1 - alpha)])
  
}

# ======================option 2 (not used)=========================================

set.seed(123)



#simulation from a multivariate normal distribution 
for (i in seq_along(rho_values)) {
  rho <- rho_values[i]
  
  cov = rho * sigma_1 * sigma_2
  Sigma <- matrix(c(sigma_1^2, cov, cov, sigma_2^2),2,2)
  
  
  simulated_results <- mvrnorm(n = n, mu = c(0,0), Sigma = Sigma)
  head(simulated_results)
  simulated_results <- as.data.frame(simulated_results)
  names(simulated_results) <- c("Stock_1", "Stock_2")
  head(simulated_results)
  
  T_year_return <- data.frame(
    Stock_1 = numeric(n),
    Stock_2 = numeric(n)
  )
  
  
  T_year_return$Stock_1 <- exp((mu_1-sigma_1^2/2) * T + simulated_results$Stock_1 * sqrt(T)) * S_1
  T_year_return$Stock_2 <- exp((mu_2-sigma_2^2/2) * T + simulated_results$Stock_2 * sqrt(T)) * S_2 
  
  T_year_return$sum_col <- T_year_return$Stock_1 + T_year_return$Stock_2
  T_year_return$hedged <- pmax(strike - T_year_return$sum_col, 0) + T_year_return$sum_col
  T_year_return$worst <- pmin(T_year_return$Stock_1,
                              T_year_return$Stock_2)
  T_year_return$worst_put <- pmax(WOstrike - T_year_return$worst, 0) + T_year_return$sum_col
  
  T_year_return$sum_col <- T_year_return$sum_col / (S_1 + S_2)
  T_year_return$hedged <- T_year_return$hedged / (S_1 + S_2 + put_premium)
  T_year_return$hedgedWO <- T_year_return$worst_put / (S_1 + S_2 + WOput_premium) 
  
  loss_unhedged <- 1 - T_year_return$sum_col
  loss_hedged   <- 1 - T_year_return$hedged
  loss_WOhedged   <- 1 - T_year_return$hedgedWO
  
  VaR_1[i] <- quantile(loss_unhedged, 1 - alpha)
  VaR_2[i] <- quantile(loss_hedged, 1 - alpha)
  VaR_3[i] <- quantile(loss_WOhedged, 1 - alpha)
  
  ES_1[i] <- mean(loss_unhedged[loss_unhedged >= quantile(loss_unhedged, 1 - alpha)])
  ES_2[i] <- mean(loss_hedged[loss_hedged >= quantile(loss_hedged, 1 - alpha)])
  ES_3[i] <- mean(loss_WOhedged[loss_WOhedged >= quantile(loss_WOhedged, 1 - alpha)])
  
}

#============================both options========================================


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
                "VaR_3" = "Hedged by worst-off put option"
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
                "ES_3" = "Hedged by worst-off put option"
    )
  )

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
  "Hedged by worst-off put option" = "#1FABD5"
)

my_linetypes <- c(
  "Unhedged Portfolio" = "solid",
  "Hedged by index put option" = "dashed",
  "Hedged by worst-off put option" = "solid"
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
rainbows <- pl_var + pl_var_ES +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(size = 12),
    plot.tag.position = c(0, 1)  # top-left corner
  )


# Display
rainbows

# Save
ggsave(
  "C:/Users/ivode/OneDrive - KU Leuven/Mafi 2/Thesis niet gedeeld/rainbow_options.png",
  plot = rainbows,
  width = 8.5,
  height = 4.5,
  units = "in",
  dpi = 300
)