#------------------------- General Purpose -------------------------
# This file computes the model-free implied correlation (MFIC) as is explained in 
# section 2.3.2 of the thesis.
# To obtain the output necessary for other files run this script twice,
# Using
#   All months in 2008, 2009 and 2010, and
#   maturity = 30 and
#   maturity = 90.

library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)


#------------------------- 
#Input: Change maturity between 30 and 90 days to obtain the results from the paper.
#It is possible to take less months or years into account.
#-------------------------

#Example of variables
maturity = 90 #We define maturity to target options with roughly the same maturity
months = c("1","2","3","4","5","6","7","8","9","10","11","12")
years = c("2008","2009","2010")

#------------------------- 
#Data Loader: Load the relevant option data using 'Loading Data.R'. 
#-------------------------

#the following program loads datasets: data, weights, zcb
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../3. Data loader")
source("Loading Data.R")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#------------------------- 
#Preliminary data cleaning.
#------------------------- 

# Exclude illiquid options by ensuring positive open interest (contracts exist) 
# and a non-zero bid price (active buyers in the market).
data_cleaned = data[data["open_interest"]>0 & data["best_bid"]>0,]
# Using the Midquote as a proxy for the fair market value to minimize the 
# "bid-ask bounce" bias in implied volatility calculations.
data_cleaned$midquote = (data_cleaned$best_bid+data_cleaned$best_offer)/2

#------------------------- 
#Step 0.1: creation of a table with interpolated interest rate per quote_date for the given maturity
#------------------------- 

df_interest_rate <- zcb %>%
  group_by(quote_date) %>%
  summarise(
    interest_rate = approx(days, rate, xout = maturity, rule = 2)$y,
    .groups = "drop"
  )

#-------------------------
#Step 0.2: Determining forward prices: using different clean data set with ATM call and put options.
#-------------------------
#To check
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
  ungroup()

#-------------------------
#Step 0.3: Add the ATM-strike as the strike at or just below the implied forward price
#-------------------------


df_forward <- df_forward %>%
  inner_join(data_cleaned,by=c("quote_date","security_ID","expiration")) %>%
  group_by(quote_date,security_ID,expiration,forward_price) %>%
  summarize(K0 = max(strike_price.y*ifelse(strike_price.y<=forward_price,1,0)),.groups = "drop") %>%
  ungroup() %>%
  select(quote_date, security_ID, expiration, K0,forward_price)


#-------------------------
#Step 1: Create main table (df_main_int)
#-------------------------

#Attributes in the dataframe are as follows:
#quote_date
#interest_rate
#security_ID
#strike_price
#expiration
#call_price
#put_price
#forward


#with expiration only the slice before and after the given maturity


# Step 1.1: Further clean the option dataset by keeping only OTM options,
# filtering maturities, and retaining only option slices with more than two strikes.
data_cleaned = data_cleaned[(data_cleaned["delta"]< 0.5 & data_cleaned["delta"]>-0.5),]
data_cleaned = data_cleaned[data_cleaned["expiration"]>=7 & data_cleaned["expiration"]<=maturity +200,]
data_cleaned <- data_cleaned %>%
  group_by(security_ID, quote_date, expiration) %>%
  filter(n() > 2) %>%
  ungroup()



#Step 1.2: only keep nearest expiration slices and add interest rates and forward prices
df_main_int <- data_cleaned %>%
  left_join(df_interest_rate, by = c("quote_date")) %>%
  left_join(df_forward %>% select(quote_date, security_ID, expiration, forward_price,K0), by = c("quote_date","security_ID","expiration")) %>%
  #Filter out option slices where the forward price diverges from the stock price by 5% -> poor data quality
  group_by(security_ID, quote_date, expiration) %>%
  filter(all(abs(forward_price / K0 - 1) < 0.1)) %>% 
  ungroup() %>%

  group_by(security_ID, quote_date) %>%
  filter(
    expiration == max(expiration[expiration <= maturity]) | 
    expiration == min(expiration[expiration >= maturity])
  ) %>%
  ungroup()

#Step 1.3: Pivot the option prices into two columns EC and EP
dummy <- data.frame(id=c(1,2))
df_main_int <- df_main_int %>%
  cross_join(dummy) %>%
  mutate(EP = ifelse(id==1 & delta< 0,midquote,NA)) %>%
  mutate(EC = ifelse(id==2 & delta> 0,midquote,NA)) %>%
  group_by(quote_date,security_ID,expiration,strike_price,interest_rate, forward_price,K0) %>%
  summarize(
    EP = if(all(is.na(EP))) NA_real_ else max(EP, na.rm = TRUE),
    EC = if(all(is.na(EC))) NA_real_ else max(EC, na.rm = TRUE),    
    .groups = "drop")


#-------------------------
#Step 2: Compute model free implied variance (MFIV) for each security over next time window [0,maturity]
#-------------------------
#Result will be a table containing:
#quote_date
#security_ID
#T_near
#T_next
#MFIV_near
#MFIV_next
#For each combination of security and quote_date

# Step 2.1: Final data prep:
# 1. we only use strikes for which we have a call price in case K>K0 and a put in case K< K0
# 2. We require any day/maturity has at least 3 Puts and 3 Calls to keep it reliable.

df_main <- df_main_int %>%
  group_by(security_ID, quote_date,expiration) %>%
  filter(sum(!is.na(EP) & strike_price<=K0)>=3)%>%
  filter(sum(!is.na(EC) & strike_price>=K0)>=3)%>%
  ungroup() %>%
  mutate(Q = case_when(
    strike_price < K0  ~ EP,             # If strike < K0, use Put
    strike_price == K0 & !is.na(EP) & !is.na(EC) ~ (EP + EC) / 2,  # If strike is exactly K0, use average
    strike_price == K0 & !is.na(EP)  ~ EP ,  # If strike is exactly K0, use average
    strike_price == K0 & !is.na(EC)  ~ EC ,  # If strike is exactly K0, use average
    TRUE               ~ EC              # "Else" (all other cases), use Call
  )) %>%
  filter(!is.na(Q))

#To check
# Helper function calculates the model-free implied variance (MFIV).
MFIV_f <- function(K,FT,K0,Q,r,Texp)
{
  n=length(K)
  Texp_val = Texp[1]/365
  K0_val = K0[1]
  r_val = r[1]
  FT_val = FT[1]
  
  DK= rep(0,n)
  DK[1] = K[2]-K[1]
  for(i in (2:(n-1)))
  {
    DK[i]=(K[i+1]-K[i-1])/2
  }
  DK[n] = K[n]-K[n-1]
  
  return(max(0,exp(r_val*Texp_val)/Texp_val*2*sum(Q*DK/K^2) -1/Texp_val*(1-FT_val/K0_val)^2))
}

#Step 2.2: Calculate the MFIV for each security, quote date and maturity.
df_MFIV <- df_main %>%
  group_by(security_ID, quote_date,expiration) %>%
  arrange(strike_price, .by_group = TRUE) %>%
  summarize(
    MFIV={MFIV_f(strike_price,forward_price,K0,Q,interest_rate,expiration)},
    .groups = "drop"
  )

#Step 2.3: Pivot the dataframe to have separate columns for the nearest maturity and next maturity. 
df_MFIV <- df_MFIV %>%
  cross_join(dummy) %>%
  group_by(security_ID, quote_date) %>%
  summarize(
  T_near = min(expiration/365),
  T_next = max(expiration/365),
  
  MFIV_near = MFIV[expiration == min(expiration)][1],
  MFIV_next = MFIV[expiration == max(expiration)][1],
  .groups = "drop"
  )



#-------------------------
#Step 3: Pulling forward missing variances
#-------------------------
#Due to illiquid data, it is possible that not all securities have a calculated MFIV above (because too few option data were available).

#Step 3.1: We first create a table 'days_and_securities'
#containing all trading days and all security_ID's in the DOJ for that date. 
#With the following columns.
#quote_date
#security_ID


period <- data %>% mutate(quote_date = as.Date(quote_date, origin = "1899-12-31")) %>% distinct(quote_date)

#weights were loaded by the data loader
#security_ID = 102456 is the index itself.
Index <- weights %>% distinct(quote_date) %>% mutate(security_ID = 102456)
security_IDs <- weights %>% bind_rows(Index)

# Result of step 3.1:
days_and_securities <- period %>%
  inner_join(security_IDs, by = c("quote_date")) %>%
  select(quote_date,security_ID)

#To check
#Step 3.2: add the nearest and next MFIV data.
df_MFIV_full <- days_and_securities %>%
  left_join(df_MFIV, by = c("quote_date","security_ID"))

# --- INTERMEZZO: Graphical representation of data quality ---
df_heatmap <- df_MFIV_full %>%
  mutate(
    #Calculate relative interpolation error compared to twice the interpolation with a point at halve maturity/365 distance
    error_scaled =
    case_when(
      T_near != T_next  ~ 0.5*abs((T_near - maturity/365) * (T_next - maturity/365))/((maturity/365)^2/4), #interpolation error of 1th-order polynomial interpolation
      T_near == T_next ~ 0.5*abs(T_near-maturity/365)/((maturity/365)/2) #interpolation error of 0th-order polynomial interpolation
     )
    )


# Plot using the scaled error
pl_heatmap <- ggplot(df_heatmap, aes(x = quote_date, y = as.factor(security_ID), fill = error_scaled)) +
  geom_tile() +
  scale_fill_gradient(low = "#1FABD5", 
                      high = "#00407A", 
                      na.value = "#FF6B6B", 
                      name = "Scaled Error", scale = c(0,1)) +
  theme_minimal() + # Keep this as a base
  theme(
    # Forces the panel and the entire plot background to be plain white
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    # Removes the grid lines for a cleaner "tile" look
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Keeps your axis text settings
    axis.text.y = element_text(size = 5)
  )
print(pl_heatmap)


assign(paste0("pl_heatmap_", year,"_",month,"_M",maturity), pl_heatmap)
output_path <- "Figures"
image_name = paste0(output_path,"/INTERMEZZO_heatmap_",year,"_",month,"_M",maturity,".png")
ggsave(image_name, plot=pl_heatmap, height =4.5)
rm(pl_heatmap) #Clean up pl_heatmap
# --- END OF INTERMEZZO ---


# Step 3.3: Pulling variance forward and removing missing values
df_MFIV_pulled <- df_MFIV_full %>%
  arrange(security_ID, quote_date) %>%
  group_by(security_ID) %>%
  fill(MFIV_near, MFIV_next, T_near, T_next, .direction = "down") %>%
  ungroup() %>%
  filter(!is.na(MFIV_near) & !is.na(MFIV_next))


#-------------------------
#Step 4: Compute model free implied correlation (MFIC) 
#-------------------------
#To check
#Result will be a table containing:
#quote_date
#Maturity information
#MFIV information 

df_MFIV_interpolated <- df_MFIV_pulled %>%
  mutate(MFIV_maturity =case_when(
      # if both IVs are given and T_near and T_next are different (i.e. not equal to maturity), then interpolation
      !is.na(MFIV_near) & !is.na(MFIV_next) & T_near != T_next ~
          (MFIV_near*T_near*(T_next - maturity/365)/(T_next - T_near)+MFIV_next*T_next*(maturity/365-T_near)/(T_next-T_near))/(maturity/365),
      
      !is.na(MFIV_near) & !is.na(MFIV_next) & T_near == T_next ~ MFIV_near,
      # Case 2: Only Short exists
      !is.na(MFIV_near) ~ MFIV_near,
      
      # Case 3: Only Long exists
      !is.na(MFIV_next)  ~ MFIV_next,
      
      # Default: If none of the above are true, it becomes NA
      TRUE ~ NA_real_
  ))
  

#-------------------------
#Step 5: The weights comprising the index DOJ are loaded
#-------------------------

df_divisor_DJIA <- weights %>%
  mutate(quote_date = as.Date(quote_date, origin = "1899-12-31")) %>%
  select(quote_date,security_ID,close_price)

#DOJ is a price-weighted index. 
#We rescale the weights for securities with invalid MFIV.

weights <- df_MFIV_interpolated %>% 
  left_join(df_divisor_DJIA, by = c("quote_date","security_ID")) %>%
  filter(!is.na(MFIV_maturity)) %>%
  filter(security_ID != 102456) %>%
  group_by(quote_date) %>%
  
  mutate(S_tot = sum(close_price)) %>% 
  mutate(weight = close_price / S_tot) %>%
  ungroup() %>%
  select(quote_date,security_ID,weight)

#Combine the earlier result with the weights
df_main <- df_MFIV_interpolated %>% left_join(weights, by = c("quote_date","security_ID")) %>% select(security_ID, quote_date, weight, MFIV_maturity)
#-------------------------
#Step 6: Computing the model-free implied correlation
#-------------------------

df_index <- df_main %>%
  filter(security_ID == 102456) %>%
  select(quote_date, MFIV_index = MFIV_maturity)

df_components <- df_main %>%
  filter(security_ID != 102456)

df_implied_corr <- df_components %>%
  left_join(df_index, by = "quote_date") %>%
  group_by(quote_date) %>%
  summarise(
    implied_corr = {

      diag_val <- sum(weight^2 * MFIV_maturity)
      off_diag <- (sum(weight * sqrt(MFIV_maturity)))^2 - diag_val
      
      (first(MFIV_index) - diag_val) / off_diag
    },
    .groups = "drop"
  )

#-------------------------
#Step 7: Writing output
#-------------------------

target_dir <- paste0("Data/", maturity)
setwd(target_dir)
output_name <- paste0("Model_Free_Implied_Correlation_M", maturity, ".csv")
write.table(df_implied_corr, output_name, row.names = FALSE,sep=";",dec=",")
