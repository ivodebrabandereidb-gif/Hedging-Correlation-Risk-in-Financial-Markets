#------------------------- General Purpose -------------------------
# This program applies the option dispersion trades discussed in Chapter 4 to the empirical dataset.
# This program is the basis for all the figures and empirical results in Chapter 6.
# Mainly, this output outputs a .csv with the daily realized and theoretical P&L of each trade.
# This output file is later used by the other programs in the folder for further analysis

#-------------------------      Steps      -------------------------
# STEP 0: loading/preparing data
# STEP 1: creating dataframe: df_TandTp with each record a liquid option, its Greeks and price the current and next trading day.
#         It includes theoretical P&L of only delta-hedging only this option
# STEP 2: take linear combinations (coefficient = 0 often) of options in df_TandTp to construct synthetic options with which is delta-hedged
#         Example (straddles): for a portfolio of a straddle, shares_option = 1 for the ATM call and put option and 0 elsewhere. 
#         df_TandTP can also be used to define log-contracts
# STEP 3: Executing the trade

#------------------------- !! Important !! -------------------------
# To obtain all necessary data for the following scripts,
# Run this entire script for the following scenarios:
# - Maturity = 30 and 90,
# - greek_hedge = 'gamma' and 'vega',
# - delta_type = 'delta' and 'delta_sticky' (choose delta_sticky only in combination with greek_hedge = gamma),
# A total of 6 scenarios should be ran.

library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)

#-------------------------------
#Input
#-------------------------------

#Input variables
#Option type is always 'Straddle'
Option_type = "Straddle"
#To obtain the results from the thesis, maturity can be put to 30 days or 90 days.
#We define maturity to target options with roughly the same maturity
maturity = 30
#choose hedging strategy: gamma or vega, with gamma the trade on the correlation gap andvega the trade on the correlation risk premium.
greek_hedge = "vega" 
#To use normal delta fill in 'delta', for a sticky delta use 'delta_sticky'
deltatype = 'delta'
months = c("1","2","3","4","5","6","7","8","9","10","11","12")
years = c("2008","2009","2010")

#-------------------------------
# Step 0: loading/preparing data
#-------------------------------

#the following program loads datasets: data, weights, zcb
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../3. Data loader")
source("Loading data.R")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#list of all trading dates (and their next trading day (weekends & holidays)):
df_trading_days <- data %>% distinct(quote_date) %>% mutate(next_quote_date = lead(quote_date))

#data cleaning
data_cleaned = data[data["open_interest"]>0 & data["best_bid"]>0,]
data_cleaned$midquote = (data_cleaned$best_bid+data_cleaned$best_offer)/2

#-------------------------------
#Step 0.1: create table with interpolated interest rate per quote_date with given maturity
#-------------------------------

#Important note: we use an extrapolated 1-day interest rate as a proxy for the interest received on a risk free bank account
df_interest_rate <- zcb %>%
  group_by(quote_date) %>%
  summarise(
    interest_rate = approx(days, rate, xout = 1, rule = 2)$y,
    .groups = "drop"
  )

#-------------------------------
#Step 0.2: Determining forward prices: using clean data set with ATM call and put options
#-------------------------------

#Step 0.2.1: Define helper functions to extract the European Call (EC) 
#and European Put (EP) option prices.
EP_no_warning <- function(midquote,delta){
  x = midquote[delta<0]
  return(ifelse(length(x)>0,first(x),NA))
}
EC_no_warning <- function(midquote,delta){
  x = midquote[delta>0]
  return(ifelse(length(x)>0,first(x),NA))
}

#Step 0.2.2: Calculate forward prices using put-call parity    
df_forward <- data_cleaned %>%
  left_join(df_interest_rate, by = c("quote_date")) %>%
  group_by(quote_date,security_ID,expiration,strike_price,interest_rate) %>%
  summarize(EP= EP_no_warning(midquote,delta),
            EC= EC_no_warning(midquote,delta),
            .groups = "drop") %>%
  group_by(quote_date,security_ID,expiration,interest_rate) %>%  
  filter(is.finite(EC) & is.finite(EP)) %>%  
  filter(min(abs(EC-EP)) == abs(EC-EP)) %>%    
  mutate(forward_price = strike_price +exp(interest_rate*expiration/365)*(EC-EP)) %>%
  ungroup() %>%
  select(quote_date,security_ID,expiration,forward_price,strike_price)


#in case the difference |EC-EP| is minimized over different strikes, we choose the lowest strike (cfr dispersion index CBOE)
df_forward <- df_forward %>%
  group_by(quote_date,security_ID,expiration) %>%
  filter(min(strike_price)== strike_price) %>%
  ungroup() %>%
  select(quote_date,security_ID,expiration,forward_price)

#-------------------------------
#Step 1: Create table containing prices and Greeks at T and T+1 for all liquid options
#-------------------------------

#T= quote_date
#T+1= next trading date

#We will create the table "df_TandTp" with Attributes as follows:
#quote_date
#security_ID
#interest_rate
#DOJ_weight
#expiration
#strike_price
#Kmin: first strike of a put below the forward
#Kplus: first strike of a call above the forward
#delta
#gamma
#O_T: price of option of type "option_type" (straddle, log,...) at T= quote_date
#S_T: stock price at T=quote_date
#F_T: forward price at T=quote_date
#O_Tp: price of option of type "option_type" (straddle, log,...) at the next trading date T+1
#S_Tp: stock price at T+1
#F_Tp: forward price at T+1
#PNL_theoretical: theoretical P&L when delta-hedging the option on the given record of the table for 1 day

#NOTE: we only keep those time slices that contain a liquid put at Kmin and a call at Kplus.
#Note 2: when working with log-contracts on the index, even stricter conditions on the time slices are required. Not taken up in this thesis for the backtesting of the option trades

#Computing Black-Scholes Gamma 
data_cleaned <- data_cleaned %>%
  inner_join(df_forward, by = c("quote_date","security_ID","expiration")) %>%
  left_join(df_interest_rate, by = c("quote_date")) %>%
  mutate(d1 = (log(forward_price/strike_price)+0.5*implied_volatility^2*expiration/365)/(implied_volatility*sqrt(expiration/365)),
         d2 = d1-implied_volatility*sqrt(expiration/365),
         gamma = strike_price*exp(-interest_rate*expiration/365)/(close_price^2*implied_volatility*sqrt(expiration/365))*dnorm(d2,0,1),
         vega = strike_price*exp(-interest_rate*expiration/365)*dnorm(d2,0,1)*(sqrt(expiration/365)),
         vanna = -exp(-interest_rate*expiration/365)*strike_price/close_price*dnorm(d2,0,1)*d2/implied_volatility,
         volga = vega*d1*d2/implied_volatility,
         type = ifelse(delta>0,"Call","Put"),
         expiry_date = as.numeric(quote_date)+expiration)


#Get option prices at T+1 in separate column
df_TandTp <- data_cleaned %>%
  left_join(df_trading_days, by = c("quote_date")) %>%
  left_join(data_cleaned, by =c("next_quote_date"="quote_date","expiry_date","type","security_ID","strike_price"), suffix= c(".a",".b")) %>%
  left_join(weights %>% distinct(quote_date,weights), by = "quote_date") %>%
  mutate(dt = as.numeric((next_quote_date-quote_date)/365)) %>%
  select(quote_date,security_ID,interest_rate = interest_rate.a, weight_DOJ= weights, implied_volatility = implied_volatility.a,delta = delta.a, gamma =gamma.a, vega = vega.a, vanna = vanna.a, volga = volga.a, strike_price, expiration = expiration.a, dt,
         O_T = midquote.a,
         S_T = close_price.a,
         F_T = forward_price.a,
         iv_T = implied_volatility.a,
         O_Tp = midquote.b,
         S_Tp = close_price.b,
         F_Tp = forward_price.b,
         iv_Tp = implied_volatility.b
         ) %>%
  filter(!is.na(O_Tp))

#Step 1.2: Adding theoretical_PNL of deltahedging 
PNL_theoretical_f <- function(gamma, IV, S_Tp,S_T,dt)
{
  R = (S_Tp-S_T)/S_T
  return(0.5*gamma*S_T^2*(R^2-IV^2*dt)) #converts boolean to 1 and 0 values
}

PNL_theoretical_w_vega_f <- function(gamma, vega,vanna,volga, IV, S_Tp,S_T,iv_Tp, iv_T,dt)
{
  R = (S_Tp-S_T)/S_T
  return(0.5*gamma*S_T^2*(R^2-IV^2*dt)+vega*(iv_Tp-iv_T)) #+0.5*vanna*(iv_Tp-iv_T)^2+volga*(iv_Tp-iv_T)*(S_Tp-S_T)) #converts boolean to 1 and 0 values
}

PNL_theoretical_w_vega_stickydelta_f <- function(gamma, vega,vanna,volga,iv_slope, IV, S_Tp,S_T,iv_Tp, iv_T,dt)
{
  R = (S_Tp-S_T)/S_T
  return(0.5*gamma*S_T^2*(R^2-IV^2*dt)+vega*(iv_Tp-iv_T)-vega*iv_slope*(S_Tp-S_T))#+0.5*vanna*(iv_Tp-iv_T)^2+volga*(iv_Tp-iv_T)*(S_Tp-S_T)) #converts boolean to 1 and 0 values
}



df_TandTp$PNL_theoretical <- PNL_theoretical_f(df_TandTp$gamma,df_TandTp$implied_volatility,df_TandTp$S_Tp,df_TandTp$S_T,df_TandTp$dt)
df_TandTp$PNL_theoretical_w_vega <- PNL_theoretical_w_vega_f(df_TandTp$gamma,df_TandTp$vega,df_TandTp$vanna,df_TandTp$volga,df_TandTp$implied_volatility,df_TandTp$S_Tp,df_TandTp$S_T,df_TandTp$iv_Tp,df_TandTp$iv_T,df_TandTp$dt)


#----------------------------------------------------------------------------------------------------------------------------
#Step 2: Summarize df_TandTp to get the prices and Greeks at T and T+1 for the synthetic options used in the dispersion trade
#----------------------------------------------------------------------------------------------------------------------------

# Next, we define for each underlying security_ID, the type of option with which is delta-hedged: straddle, log-contract,...
# The portfolio of options is translated into an extra column in df_main giving the composition of each option payoff chosen (most of them will be 0).
# For example: a straddle will have a 1 for the ATM call and put and 0 elsewhere
# Next, we collapse each quote_date and security_ID to one line representing the synthesized option, making use of linearity of the Greeks and theoretical P&L

#For straddle options, we use the strike so that the gamma of the resulting option is maximized. 
#when we are using the Black-Scholes gamma: this is simply the put and call closest to the forward price

df_main_int <- df_TandTp %>%
  group_by(quote_date,security_ID,expiration) %>%
  filter(sum(delta<0 & strike_price <= F_T) >=1 & sum(delta>0 & strike_price >= F_T) >=1) %>%
  mutate(Kmin = max(strike_price[delta<0 & strike_price <= F_T]),
         Kmax = min(strike_price[delta>0 & strike_price >= F_T])
        ) %>%
  ungroup() %>%
  group_by(quote_date,security_ID) %>%
  filter(abs(expiration -maturity) == min(abs(expiration -maturity))) %>%
  ungroup() %>%
  group_by(quote_date,security_ID) %>%
  filter(expiration == min(expiration)) %>%
  ungroup() 


#program computing finite difference approximation of slope volatility surface used for sticky-delta hedging
source("implied_volatility_slope_sticky_delta.R")

df_main <- df_main_int %>%
  left_join(df_iv_slope,by=c("quote_date","security_ID","expiration","strike_price")) %>%
  mutate(iv_slope = replace_na(iv_slope, 0), delta_sticky = delta+vega*iv_slope)

df_main$PNL_theoretical_w_vega_and_sticky_delta = PNL_theoretical_w_vega_stickydelta_f(df_main$gamma,df_main$vega,df_main$vanna,df_main$volga,df_main$iv_slope, df_main$implied_volatility,df_main$S_Tp,df_main$S_T,df_main$iv_Tp,df_main$iv_T,df_main$dt)
df_test <- df_main %>% filter(iv_slope==0)

synthetic_option_f <- function(security_ID,delta, strike,Kmin,Kmax,option_type)
{
  composition = case_when(
    option_type == "straddle" ~ as.numeric((strike == Kmin & delta<0) | (strike == Kmax & delta>0))
    )
  return(composition) 
}

# We also sum implied volatilities, this usually makes no sense. However, the sum: z(IV_call)+z(IV_put) is approximately z(IV_call+IV_put),
# with z = \partial \sigma_I/\partial \sigma_i


#Loading ATM implied correlations used for vega trade
df_implied_cor <-as.data.frame(read.csv2(paste0("../6. Implied correlation per moneyness/Data/",maturity,"/Implied correlation per moneyness_M",maturity," - ATM.csv"),sep=";", dec=","))
df_implied_cor <- df_implied_cor %>% mutate(quote_date = as.Date(quote_date))
df_implied_cor <- df_implied_cor %>% rename(rho_estimate = rho_iv)

df_main_collapsed <- df_main %>%
  mutate(composition = synthetic_option_f(security_ID,delta,strike_price,Kmin,Kmax,"straddle")) %>%
  group_by(quote_date, dt, security_ID, interest_rate, expiration, S_T, S_Tp, F_T, F_Tp) %>%
    summarize(delta_sticky = sum(delta_sticky*composition),
              delta = sum(delta*composition),
              gamma = sum(gamma*composition),
              vega = sum(vega*composition),
              IV_weighted = sum(implied_volatility)/2,
              O_T = sum(O_T*composition),
              O_Tp = sum(O_Tp*composition),
              PNL_theoretical = sum(PNL_theoretical*composition),
              PNL_theoretical_w_vega = sum(PNL_theoretical_w_vega*composition),
              PNL_theoretical_w_vega_and_sticky_delta = sum(PNL_theoretical_w_vega_and_sticky_delta*composition),
              .groups = "drop") %>%
  left_join(weights %>% distinct(quote_date,weights), by = "quote_date") %>%
  left_join(df_implied_cor, by = "quote_date") %>%
  select(quote_date,dt, security_ID,interest_rate, weight_DOJ= weights, delta_sticky, delta, gamma,vega, rho_estimate, IV_weighted, expiration, F_T, S_T, O_T, F_Tp, S_Tp, O_Tp, PNL_theoretical,PNL_theoretical_w_vega,PNL_theoretical_w_vega_and_sticky_delta)

#------------------------------
#INTERMEZZO: data quality check
#------------------------------
#counting number of securities available per day (should be 31: all constituents and index itself)


df_DQ <- df_main_collapsed %>%
  group_by(quote_date) %>%
  summarize(nbr_securities = n_distinct(security_ID), .groups = "drop")

ggplot(df_DQ, aes(x = quote_date, y = nbr_securities)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue") +
  labs(
    title = "Unique Security Count Over Time",
    x = "Quote Date",
    y = "Number of Unique Securities"
  ) +
  theme_minimal()

#---------------------------
#Step 3: Executing the trade
#---------------------------

# Determining the number of shares to hold for each option, stock and bank account
# result is a table with the following atttributes:

#quote_date
#security_ID
#interest_rate
#delta
#gamma
#expiration
#O_T: price of option of type "option_type" (straddle, log,...) at T= quote_date
#S_T: stock price at T=quote_date
#F_T: forward price at T=quote_date
#O_Tp: price of option of type "option_type" (straddle, log,...) at T+1
#S_Tp: stock price at T+1
#F_Tp: forward price at T+1
#shares_option
#shares_stock
#shares_bank

#for the vega hedge, we need an estimate of future correlations. We use the ATM IC.

portfolio_f <- function(greek_hedge, security_ID, gamma,delta,vega, O,S,weight_DOJ,IV,rho_estimate){
  n = length(security_ID)
  index = which(security_ID == 102456)
  alpha_index = -sign(gamma[index])/O[index]
  
  #We compute the vector signifying the shares in each option: shares_option.
  if(greek_hedge == "gamma")
  {
    shares_option= -alpha_index*first(weight_DOJ)^2*gamma[index]/gamma
    shares_option[index] = alpha_index    
  } else if (greek_hedge == "vega"){
    #partial derivative of O_I w.r.t. sigma_i, with i the vector element
    weights = S/sum(S[security_ID != 102456])
    
    z = 1/IV[index]*(weights^2*IV+weights*rho_estimate*(sum(weights[security_ID != 102456]*IV[security_ID != 102456])-weights*IV))
    partial_OI_partial_sigma_i = vega[index]*z
    
    shares_option =  -alpha_index*partial_OI_partial_sigma_i/vega
    shares_option[index] = alpha_index
  }
  #We delta hedge all stocks by hedging the index option with the index itself.
  shares_stock = -shares_option*delta
  
  shares_bank = -(sum(shares_option*O)+sum(shares_stock*S))
  return(data.frame(shares_option,shares_stock, shares_bank))
}


df_dispersion_portfolio <- df_main_collapsed %>%
  group_by(quote_date) %>%
  mutate(portfolio_f(greek_hedge,security_ID,gamma,get(deltatype),vega,O_T,S_T,weight_DOJ,IV_weighted,rho_estimate)) %>%
  ungroup()

#------------------------------------------
# Step 4: Computing P&L of hedging strategy.
#------------------------------------------

df_PNL <- df_dispersion_portfolio %>%
  group_by(quote_date) %>%
  summarize(PNL_realized = sum(shares_option*(O_Tp-O_T)+shares_stock*(S_Tp-S_T))+first(shares_bank*((1+interest_rate)^(first(dt))-1)),
            PNL_theoretical = sum(PNL_theoretical*shares_option),
            PNL_theoretical_w_vega = sum(PNL_theoretical_w_vega*shares_option),
            PNL_theoretical_w_vega_and_sticky_delta = sum(PNL_theoretical_w_vega_and_sticky_delta*shares_option),
            PNL_theoretical_scaled = sum(PNL_theoretical*shares_option)/gamma[security_ID == 102456],
            Gamma_index = gamma[security_ID == 102456],
            S_I = S_T[security_ID == 102456],
            R_I = (S_Tp[security_ID == 102456]-S_T[security_ID == 102456])/S_T[security_ID == 102456],
            O_I = O_T[security_ID == 102456],
            .groups = "drop")

#------------------------------------------
# EXTRA: Visualization P&L.
#------------------------------------------

ggplot(df_PNL, aes(x = quote_date)) +
  # Realized P&L Line
  geom_line(aes(y = cumsum(PNL_realized), color = "Realized"), size = 1) +
  # Theoretical P&L Line
  labs(
    title = "Cumulative P&L: Realized vs. Theoretical",
    x = "Quote Date",
    y = "Cumulative P&L",
    color = "Type"
  ) +
  scale_color_manual(values = c("Realized" = "steelblue", "Theoretical" = "darkorange")) +
  theme_minimal()


ggplot(df_PNL, aes(x = quote_date)) +
  # Realized P&L Line
  geom_line(aes(y = cumsum(PNL_realized), color = "Realized"), size = 1) +
  # Theoretical P&L Line
  geom_line(aes(y = cumsum(PNL_theoretical), color = "Theoretical"), size = 1, linetype = "dashed") +
  labs(
    title = "Cumulative P&L: Realized vs. Theoretical",
    x = "Quote Date",
    y = "Cumulative P&L",
    color = "Type"
  ) +
  scale_color_manual(values = c("Realized" = "steelblue", "Theoretical" = "darkorange")) +
  theme_minimal()

#------------------------------------------
# Step 5: Saving the P&L data.
#------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(paste0("Data/",maturity))
output_name <- paste0("Option_dispersion_trade_",deltatype,"_",greek_hedge,"_",maturity,".csv")
write.table(df_PNL, output_name, row.names = FALSE,sep=";",dec=",")

#------------------------------------------
#Insights in strategy
#------------------------------------------

# measuring Gamma_I/O_I
df_Gamma_over_O <- df_main_collapsed %>%
  filter(security_ID == 102456) %>%
  mutate(ratio = gamma/O_T) %>%
  select(quote_date, ratio)

assign(
  paste0("df_Gamma_over_O_", maturity),
  df_Gamma_over_O
)

#mean(df_Gamma_over_O_30$ratio)
#mean(df_Gamma_over_O_90$ratio)

ggplot(get(paste0("df_Gamma_over_O_", maturity)), aes(x = quote_date)) +
  # Realized P&L Line
  geom_line(aes(y = ratio, color = "Realized"), size = 1) +
  # Theoretical P&L Line
  labs(
    title = "Cumulative P&L: Realized vs. Theoretical",
    x = "Quote Date",
    y = "Cumulative P&L",
    color = "Type"
  ) +
  theme_minimal()

#value invested in type of asset

df_values_invested <- df_dispersion_portfolio %>%
  group_by(quote_date) %>%
  summarize(stock_worth = sum(shares_stock*S_T*if_else(security_ID == 102456,0,1)),
            index_worth = sum(shares_stock*S_T*if_else(security_ID == 102456,1,0)),
            options_worth = sum(shares_option*O_T*if_else(security_ID == 102456,0,1)),
            cash_worth = first(shares_bank),
            .groups = "drop")

df_long <- df_values_invested %>%
  pivot_longer(
    cols = c(stock_worth, index_worth, options_worth, cash_worth),
    names_to = "asset_class",
    values_to = "value"
  )

# Create plot portfolio value composition over time
ggplot(df_long, aes(x = quote_date, y = value, fill = asset_class)) +
  geom_area(alpha = 0.8, size = 0.5, colour = "white") +
  scale_fill_brewer(palette = "Set2", labels = c("Cash", "Index", "Options", "Stocks")) +
  labs(
    title = "Portfolio Value Composition Over Time",
    subtitle = "Breakdown of stock, index, options, and cash holdings",
    x = "Date",
    y = "Total Worth",
    fill = "Asset Class"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

cat("Individual stocks:", mean(df_values_invested$stock_worth)*100,"\n", 
    "Index:", mean(df_values_invested$index_worth)*100, "\n", 
    "Individual options:", mean(df_values_invested$options_worth)*100, "\n", 
    "Cash position:", mean(df_values_invested$cash_worth)*100, "\n")

model <- lm(PNL_realized ~ PNL_theoretical, data=df_PNL)
summary(model)
ggplot(df_PNL, aes(x = PNL_theoretical, y = PNL_realized)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "CAPM: Strategy Excess Returns vs. Market Excess Returns",
    x = "Theoretical Excess Return (RMe)",
    y = "Strategy Excess Return (Re)"
  ) +
  theme_minimal()

#------------------------------------------
# Linear regression of the sticky-delta rule in Section 6.2.1
#------------------------------------------

df_sticky_delta_regime <- df_main %>%
  mutate(composition = synthetic_option_f(security_ID,delta,strike_price,Kmin,Kmax,"straddle")) %>%
  filter(composition == 1) %>%
  mutate(dsigma = iv_Tp-iv_T, x = iv_slope*(S_Tp-S_T)) %>%
  select(dsigma,x)

sticky_delta_model <- lm(dsigma ~ x, data=df_sticky_delta_regime)
summary(sticky_delta_model)
