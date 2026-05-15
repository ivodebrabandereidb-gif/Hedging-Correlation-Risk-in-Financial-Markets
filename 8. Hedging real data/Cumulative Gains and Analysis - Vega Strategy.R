#------------------------- General Purpose -------------------------
# This file is constructed to give insight in the results of the gamma trading strategy 
# in accordance to what is written in the thesis in Chapter 6.2 .
# Furthermore, it constructs figure 15 and Table 6 (data is printed throughout the script).

#------------------------- !! Important !! -------------------------
# To obtain the figures laid out in the thesis:
# Run this script up to line  using,
# Maturity = 30 and 90,
# delta = "delta"
# When both maturities are in memory then run the last lines of the script.

library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
#detach("package:MASS", unload = TRUE)

#-------------------------------
#Input
#-------------------------------

#To obtain the results from the thesis, maturity can be put to 30 days or 90 days.
#We define maturity to target options with roughly the same maturity
maturity = 30
#Use this program only for the vega-hedged analysis, for the gamma-hedged analysis
#Put equal to 'delta'.
delta = "delta"
#we refer the reader to 'Cumulative Gains and Analysis - Gamma Strategy.R'.
greek_hedge = "vega"

months = c("1","2","3","4","5","6","7","8","9","10","11","12")
years = c("2008","2009","2010")

#-------------------------------
# Step 0: loading/preparing data
#-------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../3. Data loader")
source("Loading Data.R")

#-------------------------------
# Step 1: Summary statistics for investment in DJIA index
#-------------------------------

df_interest_rate <- zcb %>%
  group_by(quote_date) %>%
  summarise(
    interest_rate = approx(days, rate, xout = 1, rule = 2)$y,
    .groups = "drop")

#summary statistics for investment in DJIA index
df_index <- data %>% filter(security_ID==102456) %>% distinct(quote_date,close_price)
df_index <- df_index %>%
  arrange(quote_date) %>%
  mutate(return = (lead(df_index$close_price) / df_index$close_price) - 1) %>%
  filter(!is.na(return))
df_index <- df_index %>%
  left_join(df_interest_rate, by = c("quote_date")) %>%
  filter(!is.na(interest_rate))

mean = mean(df_index$return)
Excess_return = mean(df_index$return-((1+df_index$interest_rate)^(1/365)-1))
n = length(df_index$return)
sd = sqrt(1/(n-1)*sum((df_index$return-mean)^2))
second_central_moment = mean((df_index$return-mean)^2)
third_central_moment = mean((df_index$return-mean)^3)
fourth_central_moment = mean((df_index$return-mean)^4)

annualization_factor = n/3
skew = third_central_moment/(sd^3)
excess_kurtosis = fourth_central_moment/sd^4-3
Sharpe_ratio = Excess_return*annualization_factor/(sd*sqrt(annualization_factor))
cat("mean:", 100*mean*annualization_factor, "\n", "Excess return", Excess_return*100*annualization_factor,"\n", "sd:", sd*sqrt(annualization_factor), "\n", "skew", skew, "\n", "excess_kurtosis", excess_kurtosis, "\n", "Sharpe_ratio", Sharpe_ratio)

mean_interest = mean((1+df_index$interest_rate)^(1/365)-1)*100*annualization_factor

#-------------------------------
# Step 2: Loading P&L Option dispersion trade
#-------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(paste0("Data/",maturity))
df_PNL <-as.data.frame(read.csv2(paste0("Option_dispersion_trade_",delta,"_",greek_hedge,"_",maturity,".csv"),sep=";", dec=",")) %>% mutate(quote_date = as.Date(quote_date)) %>% select(quote_date,PNL_realized, PNL_theoretical, PNL_theoretical_w_vega,Gamma_index, S_I)
df_PNL_cumulative <- df_PNL %>%
  mutate(PNL_realized_cum = cumsum(PNL_realized),
         PNL_theoretical_cum = cumsum(PNL_theoretical))
df_PNL_one_column <- df_PNL_cumulative %>% pivot_longer(cols = c(PNL_realized_cum, PNL_theoretical_cum), names_to = "type", values_to = "PNL")
df_PNL_one_column <- df_PNL_one_column %>% mutate(ID = if_else(type=="PNL_realized_cum",1,2)) %>% select(quote_date,PNL,ID)

df_cumulative_PNL <- df_PNL_one_column %>% group_by(ID)

#-------------------------------
# Step 3: Summary statistics
#-------------------------------

df_PNL <- df_PNL %>%
  left_join(df_interest_rate, by = c("quote_date"))

mean = mean(df_PNL$PNL_realized)
Excess_return = mean(df_PNL$PNL_realized-((1+df_index$interest_rate)^(1/365)-1))
n = length(df_PNL$PNL_realized)
sd = sqrt(1/(n-1)*sum((df_PNL$PNL_realized-mean)^2))
second_central_moment = mean((df_PNL$PNL_realized-mean)^2)
third_central_moment = mean((df_PNL$PNL_realized-mean)^3)
fourth_central_moment = mean((df_PNL$PNL_realized-mean)^4)

annualization_factor = n/3 # We divide by 3 because we use three years.
skew = third_central_moment/(sd^3)
excess_kurtosis = fourth_central_moment/sd^4-3
Sharpe_ratio = Excess_return*annualization_factor/(sd*sqrt(annualization_factor))
cat("mean:", 100*mean*annualization_factor, "\n", "Excess return", Excess_return*100*annualization_factor,"\n", "sd:", sd*sqrt(annualization_factor), "\n", "skew", skew, "\n", "excess_kurtosis", excess_kurtosis, "\n", "Sharpe_ratio", Sharpe_ratio)


#-------------------------------
# Step 4: Development CAPM model.
#-------------------------------

df_CAPM <- df_PNL %>%
  left_join(df_index, by = c("quote_date")) %>%
  mutate(Re = PNL_realized-((1+interest_rate.x)^(1/365)-1),
         Re_theoretical = PNL_theoretical-((1+interest_rate.x)^(1/365)-1),
         Re_theoretical_w_vega = PNL_theoretical_w_vega-((1+interest_rate.x)^(1/365)-1),
         RMe = return-((1+interest_rate.x)^(1/365)-1),
         Gamma_index) %>%
  select(quote_date, Re, RMe, Re_theoretical,Re_theoretical_w_vega,Gamma_index,S_I)

capm_model <- lm(Re ~ RMe, data = df_CAPM)
multi_factor_model <-lm(Re ~ RMe+Re_theoretical, data = df_CAPM)

# View the full summary (Coefficients, R-squared, p-values)
mean(df_PNL$PNL_realized)
summary(capm_model)
cat("Single index model: \n", "Alpha", capm_model$coefficients[1]*annualization_factor, "Beta", capm_model$coefficients[2])

#-------------------------------
# Step 5: Understanding results and building plots for figure 15.
#-------------------------------

pl_maturity <- ggplot(df_PNL, aes(x = quote_date, y = cumsum(PNL_realized))) +
  # Hard-code color and linetype here since there is only one group
  geom_line(linewidth = 0.5, color = "#1FABD5", linetype = "solid") + 
  scale_x_date(
    limits = c(as.Date("2008-01-01"), as.Date("2010-12-31")),
    breaks = seq(as.Date("2008-01-01"), as.Date("2010-12-31"), by = "6 months"),
    date_labels = "%b/%y",
    expand = c(0, 0) 
  ) +
  scale_y_continuous(breaks = c(-1, 0, 1, 2, 3)) +
  labs(
    x = NULL,
    y = "Cumulative gains",
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black")
  )

assign(
  paste0("pl_", maturity),
  pl_maturity
)

print(get(paste0("pl_", maturity)))


pl_capm_maturity <- ggplot(df_CAPM, aes(x = RMe*100, y =Re*100)) +
  geom_point(alpha = 0.5, color = "#00407A", size = 1) +
  scale_y_continuous(expand = c(0, 0), breaks  = seq(-60,20,20), limits = c(-60, 20))+
  labs(
    x = "Index excess return (%)",
    y = "Trade excess return (%)",
    color = "Legend Title",
    linetype = "Legend Title"  )+
  # 50-day Simple Moving Average
  
  theme_minimal() +
  theme(panel.grid.minor = element_blank())+
  theme(
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),panel.grid.minor = element_blank()
  )+theme(
    axis.title.x = element_text(margin = margin(t = -15)) # 't' is top margin
  )

assign(
  paste0("pl_capm_", maturity),
  pl_capm_maturity
)

print(get(paste0("pl_capm_", maturity)))

#------------------------- 
#Constructing the plot from the thesis, make sure to have run up to line  for both maturities.
#Both maturity 30 and 90 should be in memory
#-------------------------

combined_plot <- ((pl_30+pl_capm_30)/ (pl_90+pl_capm_90)) + plot_annotation(tag_levels = 'a')

# Display the result
combined_plot

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("Figures")
ggsave('C:/Users/bramv/Documents/Universiteit/2025-2026/Master thesis/Data/data results/Realmarketdata_analysis_vega.png', 
       plot=combined_plot,
       width = 6.5,
       height = 5,
       units = "in",   # Always specify inches
       dpi = 300       # High resolution for printing
)

#------------------------- 
# EXTRA: Statistical test: relation between sign of returns and correlation gap
# First, Box-Ljung test to test for dependence
#-------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../5. Model-free")
setwd(paste0("C:/Users/bramv/Documents/Universiteit/2025-2026/Master thesis/Data/data results/",maturity))

# Loading implied correlations
df_implied_cor <- as.data.frame(read.csv2(paste0("Model_Free_Implied_Correlationv2.csv"),sep=";", dec=","))
df_implied_cor <- df_implied_cor %>% mutate(quote_date =as.Date(quote_date), correlation = implied_corr) %>% arrange(quote_date)  %>% select(quote_date, correlation)

#loading realized correlations
df_realized_cor <- as.data.frame(read.csv2(paste0("Realized_Correlation_",maturity,".csv"),sep=";", dec=","))
df_realized_cor <- df_realized_cor  %>% mutate(quote_date =as.Date(quote_date)) %>% rename(correlation = realized_cor)

#Correlation gap
df_spread <- df_implied_cor %>%
  inner_join(df_realized_cor, by = "quote_date") %>%
  mutate(rho_spread = correlation.x-correlation.y) %>%
  select(quote_date, IC = correlation.x, rho_spread)

df_matches <- df_PNL %>%
  inner_join(df_spread, by = c("quote_date")) %>%
  mutate(matches = if_else(sign(rho_spread) == sign(PNL_realized),1,0)) 

matches = as.numeric(df_matches$matches)
Box.test(as.numeric(matches), lag = 10, type = "Ljung-Box")


binom.test(sum(matches), length(matches), p = 0.5, alternative = "greater")





