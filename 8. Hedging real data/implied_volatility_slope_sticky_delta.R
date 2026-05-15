#library(lubridate)
#library(ggplot2)
#library(dplyr)
#library(tidyr)

#maturity = 30
#months = c("1","2","3","4","5","6","7","8","9","10","11","12")
#years = c("2008","2009","2010")

#source("C:/Users/bramv/Documents/Universiteit/2025-2026/Master thesis/R programmas/Hedging real data/Loading data.R")


#OTM-options
data_cleaned_sticky_delta = data[data["open_interest"]>0 & data["best_bid"]>0 & (data["delta"]< 0.5 & data["delta"]>-0.5),]
# Filter out short and long maturities
data_cleaned_sticky_delta = data_cleaned_sticky_delta[data_cleaned_sticky_delta["expiration"]>=6 & data_cleaned_sticky_delta["expiration"]<=maturity+200,]
# Only keep maturities with more than two strikes

#Sometimes, for the same strike there is both a call and a put with delta <0.5 and delta >-0.5 (like delta 0.49 and -0.49).
#This causes problems. Below, we keep the option with highest open_interest and lowes bid/ask-spread
data_cleaned_sticky_delta <- data_cleaned_sticky_delta  %>%
  group_by(security_ID, quote_date, expiration, strike_price) %>%
  filter(open_interest == max(open_interest)) %>%
  filter((best_offer-best_bid) == min(best_offer-best_bid)) %>%
  filter(max(delta)== delta) %>% #to remove any duplicates left
  ungroup()

df_iv_slope <- data_cleaned_sticky_delta %>%
  group_by(security_ID, quote_date, expiration) %>%
  arrange(strike_price) %>%
  mutate(h2 = lead(strike_price)-strike_price,
         h1 = strike_price-lag(strike_price),
         iv_slope = case_when(
            !is.na(h2) & !is.na(h1) ~ (h1^2*lead(implied_volatility)+(h2^2-h1^2)*implied_volatility-h2^2*lag(implied_volatility))/(h1*h2*(h1+h2)),
            !is.na(h2) ~ (lead(implied_volatility)-implied_volatility)/h2,
            !is.na(h1) ~ (implied_volatility-lag(implied_volatility))/h1,
            TRUE ~NA_real_),
         iv_slope = -strike_price/close_price*iv_slope
        ) %>%
  ungroup() %>%
  select(security_ID, quote_date, expiration, strike_price, iv_slope) %>%
  filter(!is.na(iv_slope))



