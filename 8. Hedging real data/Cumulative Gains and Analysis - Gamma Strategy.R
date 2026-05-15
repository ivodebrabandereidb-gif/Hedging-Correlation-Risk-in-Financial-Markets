#------------------------- General Purpose -------------------------
# This file is constructed to give insight in the results of the gamma trading strategy 
# in accordance to what is written in the thesis in Chapter 6.2
# Furthermore, it constructs figures 12, 13 and 14, and Tables 4 (data is printed throughout the script) and 5.

#------------------------- !! Important !! -------------------------
# To obtain figures 12 and 13 in the thesis:
# Run this script up to line  using,
# maturity = 30 and 90,
# delta = "delta"
# When both maturities are in memory then run the last lines of the script.
# Figure 14 and tables 4 and 5 are made in the same process.

library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
#detach("package:MASS", unload = TRUE)

#-------------------------------
#Input
#-------------------------------

#To obtain the results from the thesis, maturity can be put to 30 days or 90 days.
#We define maturity to target options with roughly the same maturity
maturity = 90
#Use this program only for the gamma-hedged analysis, for the vega-hedged analysis
#we refer the reader to 'Cumulative Gains and Analysis - Vega Strategy.R'.
greek_hedge = "gamma"
#Put equal to 'delta_sticky' when using the sticky delta or 'delta' when using the standard delta.
delta = "delta"
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
# Step 2: Option dispersion trade
#-------------------------------

# Step 2.1: Loading P&L
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("Data")
df_PNL <-as.data.frame(read.csv2(paste0(maturity,"/Option_dispersion_trade_", delta, "_",greek_hedge,"_",maturity,".csv"),sep=";", dec=",")) %>% mutate(quote_date = as.Date(quote_date)) %>% select(quote_date,PNL_realized, PNL_theoretical, PNL_theoretical_w_vega,Gamma_index, S_I)
df_PNL_cumulative <- df_PNL %>%
    mutate(PNL_realized_cum = cumsum(PNL_realized),
           PNL_theoretical_cum = cumsum(PNL_theoretical))
df_PNL_one_column <- df_PNL_cumulative %>% pivot_longer(cols = c(PNL_realized_cum, PNL_theoretical_cum), names_to = "type", values_to = "PNL")
df_PNL_one_column <- df_PNL_one_column %>% mutate(ID = if_else(type=="PNL_realized_cum",1,2)) %>% select(quote_date,PNL,ID)

df_cumulative_PNL <- df_PNL_one_column %>% group_by(ID)

#Summary statistics of option dispersion trade

df_PNL <- df_PNL %>%
  left_join(df_interest_rate, by = c("quote_date"))

mean = mean(df_PNL$PNL_realized)
Excess_return = mean(df_PNL$PNL_realized-((1+df_index$interest_rate)^(1/365)-1))
n = length(df_PNL$PNL_realized)
sd = sqrt(1/(n-1)*sum((df_PNL$PNL_realized-mean)^2))
second_central_moment = mean((df_PNL$PNL_realized-mean)^2)
third_central_moment = mean((df_PNL$PNL_realized-mean)^3)
fourth_central_moment = mean((df_PNL$PNL_realized-mean)^4)

annualization_factor = n/3
skew = third_central_moment/(sd^3)
excess_kurtosis = fourth_central_moment/sd^4-3
Sharpe_ratio = Excess_return*annualization_factor/(sd*sqrt(annualization_factor))
cat("mean:", 100*mean*annualization_factor, "\n", "Excess return", Excess_return*100*annualization_factor,"\n", "sd:", sd*sqrt(annualization_factor), "\n", "skew", skew, "\n", "excess_kurtosis", excess_kurtosis, "\n", "Sharpe_ratio", Sharpe_ratio)


#CAPM
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

#-------------------------------
# Step 3: Summarizing statistics
#-------------------------------

# View the full summary (Coefficients, R-squared, p-values)
mean(df_PNL$PNL_realized)
summary(capm_model)
cat("Single index model: \n", "Alpha", capm_model$coefficients[1]*annualization_factor, "Beta", capm_model$coefficients[2])

#-------------------------------
# Step 4: Preparing correct settings for figure 11.
#-------------------------------

Sys.setlocale("LC_TIME", "English")
plmaturity <- ggplot(df_PNL_one_column, aes(x = quote_date, y = PNL, color = as.factor(ID), group = ID,linetype = as.factor(ID))) +
  geom_line(linewidth = 0.5) + 
  scale_x_date(
    limits = c(as.Date("2008-01-01"), as.Date("2010-12-31")),
    breaks = seq(as.Date("2008-01-01"), as.Date("2010-12-31"), by = "6 months"),
    #Format of values on x-axis
    date_labels = "%b/%y",
    #Ensure the line touches the edges of the plot
    expand = c(0, 0) 
  ) +
  scale_y_continuous(breaks = c(-1,0,1,2,3,4,5))+ #c(-2,-1,0,1,2)
  labs(
    
    x = NULL,
    y = NULL,
    color = "Legend Title",
    linetype = "Legend Title"
  ) +
  scale_color_manual(
    values = c("1" = "#DD8A2E", "2" = "#1FABD5"),
    labels = c("2" = "Gamma P&L", "1" = "Realized P&L")
  )+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())+
  theme(
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),panel.grid.minor = element_blank()
  )+
  annotate("text", x = as.Date("2010-09-01"), y = 5.6 , label = paste0(maturity, "-day maturity"))+ #y= 2.26, 3.4, #5.6
  scale_linetype_manual(
    values = c("1" = "solid", "2" = "21"),
    labels = c("2" = "Gamma P&L", "1" = "Realized P&L")
  )

common_scale <- list(scale_color_manual(
  name = NULL,
  values = c("1" = "#DD8A2E", "2" = "#1FABD5"),
  labels = c("2" = "Gamma P&L", "1" = "Realized P&L")),
  scale_linetype_manual(
    name = NULL,
    values = c("1" = "solid", "2" = "21"), # Distinguishable linetypes
    labels = c("2" = "Gamma P&L", "1" = "Realized P&L")
  )
)

assign(
  paste0("pl", maturity),
  plmaturity
)

print(get(paste0("pl", maturity)))

#-------------------------------
# Step 5: Understanding returns.
#-------------------------------

if(delta== "delta")
{
if(maturity==90)
{
  ybreaks = c(-20,-15, -10,-5,0,5,10)
  ylimits = c(-23, 10)
  
  xbreaks = c(-10,-5, 0,5)
  xlimits = c(-13, 5)
} else
{
  ybreaks = seq(-60,20,20)
  ylimits = c(-60, 20)
  
  
  xbreaks = seq(-60,15,15)
  xlimits = c(-60, 15)
}} else
{
  if(maturity==90)
  {
    ybreaks = c(-20,-15, -10,-5,0,5,10)
    ylimits = c(-23, 10)
    
    xbreaks = c(-10,-5, 0,5)
    xlimits = c(-13, 5)
  } else
  {
    ybreaks = seq(-60,30,15)
    ylimits = c(-60, 30)
    
    
    xbreaks = seq(-60,15,15)
    xlimits = c(-60, 15)
  }
  
}

#-------------------------------
# Step 6: Settings figure 13.
#-------------------------------

pl_capm_maturity <- ggplot(df_CAPM, aes(x = RMe*100, y =Re*100)) +
  geom_point(alpha = 0.5, color = "#00407A") +
  scale_y_continuous(expand = c(0, 0), breaks = ybreaks, limits = ylimits)+
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
  )

assign(
  paste0("pl_capm_", maturity),
  pl_capm_maturity
)

print(get(paste0("pl_capm_", maturity)))

#Regression analysis from Section 6.2: Realized P&L on (theoretical) gamma P&L. (Table 5)
Re_against_Retheor <-lm(Re ~ Re_theoretical, data = df_CAPM)
summary(Re_against_Retheor)


pl_real_vs_theor_maturity <- ggplot(df_CAPM, aes(x = Re_theoretical*100, y = Re*100)) +
  geom_point(alpha = 0.5, color = "#00407A") +
  geom_abline(intercept = Re_against_Retheor$coefficients[1] , slope = Re_against_Retheor$coefficients[2], color = "#DD8A2E", linetype = "21", linewidth = 0.78) +
  scale_y_continuous(expand = c(0, 0), breaks = ybreaks, limits = ylimits)+
  scale_x_continuous(expand = c(0, 0), breaks = xbreaks, limits = xlimits)+
  labs(
    x = "Gamma P&L (%)",
    y = "Realized P&L (%)",
    color = "Legend Title",
    linetype = "Legend Title"  )+
  # 50-day Simple Moving Average
  
  theme_minimal() +
  theme(panel.grid.minor = element_blank())+
  theme(
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),panel.grid.minor = element_blank()
  )

assign(
  paste0("pl_real_vs_theor_", maturity),
  pl_real_vs_theor_maturity
)

print(get(paste0("pl_real_vs_theor_", maturity)))

#-------------------------------
# Step 6: Settings and saving figure 14: vega corrected realized vs index returns
#-------------------------------

pl_capm_vega_corrected_maturity <- ggplot(df_CAPM, aes(x = RMe*100,y =100*(Re-(Re_theoretical_w_vega-Re_theoretical)))) +
  geom_point(alpha = 0.5, color = "#00407A") +
  scale_y_continuous(expand = c(0, 0))+
  labs(
    x = "Index excess return (%)",
    y = "Vega-corrected trade excess return (%)",
    color = "Legend Title",
    linetype = "Legend Title"  )+
  # 50-day Simple Moving Average
  #stat_function(fun = function(x) -x^2, color = "red", linewidth = 1) +
  #geom_smooth(method = "lm", formula = y ~ I(x^2), color = "darkorange", se = FALSE) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())+
  theme(
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),panel.grid.minor = element_blank()
  )

assign(
  paste0("pl_capm_vega_corrected_", maturity),
  pl_capm_vega_corrected_maturity
)
#Regression analysis from Section 6.2.1: Realized P&L on (theoretical) gamma-vega P&L.
Re_against_Retheor_vega <- lm(Re ~ Re_theoretical_w_vega, data = df_CAPM)
summary(Re_against_Retheor_vega)

pl_real_vs_theor_vega_maturity <- ggplot(df_CAPM, aes(x = Re_theoretical_w_vega*100, y = Re*100)) +
  geom_point(alpha = 0.5, color = "#00407A") +
  geom_abline(intercept = Re_against_Retheor_vega$coefficients[1] , slope = Re_against_Retheor_vega$coefficients[2], color = "#DD8A2E", linetype = "21", linewidth = 0.78) +
  scale_y_continuous(expand = c(0, 0), breaks=seq(-60,20,20), limits =c(-60,20))+
  scale_x_continuous(expand = c(0, 0),breaks=seq(-80,20,20),limits = c(-80,20))+
  labs(
    x = "Gamma + vega P&L (%)",
    y = "Realized P&L (%)",
    color = "Legend Title",
    linetype = "Legend Title"  )+
  # 50-day Simple Moving Average
  
  theme_minimal() +
  theme(panel.grid.minor = element_blank())+
  theme(
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),panel.grid.minor = element_blank()
  )

assign(
  paste0("pl_real_vs_theor_vega_", maturity),
  pl_real_vs_theor_vega_maturity
)

pl_vega_maturity <- get(paste0("pl_real_vs_theor_vega_", maturity))+get(paste0("pl_capm_vega_corrected_", maturity))

pl_vega_maturity

#Save Plot for Figure 14.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("Figures")
ggsave(paste0('Figure14_realmarket_analysis_gamma_vega_corrected_',maturity,'.png'), 
       plot=pl_vega_maturity,
       width = 7,
       height = 3.5,
       units = "in",   # Always specify inches
       dpi = 300       # High resolution for printing
)

CAPM_vega_corrected <- lm(Re-(Re_theoretical_w_vega-Re_theoretical) ~RMe, data = df_CAPM)
summary(CAPM_vega_corrected)
confint(CAPM_vega_corrected, level = 0.95)

#-------------------------------
# Step 7: Settings and saving figure 12 and 13
#-------------------------------

top_row <- pl30/ pl90 + plot_layout(guides = "collect") & common_scale & theme(legend.position = "bottom")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
top_row
ggsave(paste0('Figures/Figure12_PNL_cummulative_',greek_hedge,'_',delta,'_30_90.png'), plot=top_row,  width = 6.5,height = 5.25)


combined_plot <- ((pl_real_vs_theor_30+pl_capm_30)/ (pl_real_vs_theor_90+pl_capm_90)) + plot_annotation(tag_levels = 'a')

# Display the result
combined_plot
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ggsave(paste0('Figures/Figure13_Scatter_Realized_daily_vs_',greek_hedge,'_',delta,'_PL.png'), 
       plot=combined_plot,
       width = 6.5,
       height = 5,
       units = "in",   # Always specify inches
       dpi = 300       # High resolution for printing
)
