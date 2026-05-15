#------------------------- General Purpose -------------------------
# This file computes the realized correlation (RC) as is explained in 
# section 2.3.1 of the thesis.
# To obtain the output necessary for other files run this script twice,
# Using
#   maturity = 30 and
#   maturity = 90.

library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)


#------------------------- 
#Input: Change maturity between 30 and 90 days to obtain the results from the paper.
#-------------------------
maturity = 90


#------------------------- 
#Data Loader: We only need the weights in the DOJ here.
#-------------------------
# The dataset with weights is the same for every year, so we only take the one in the '2008' folder.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../2. Raw option data/2008")
weights_name <- "weights_2008.csv"

weights = as.data.frame(read.csv2(weights_name, sep=",", header = T, dec = "."))
weights <- weights %>% mutate(quote_date = as.Date(quote_date, origin = "1899-12-30"))

#------------------------- 
#Step 1: Calculate daily returns and weights.
#-------------------------
#Calculate daily stock returns and corresponding scaled index weights
returns <- weights %>%
  group_by(security_ID) %>%
  arrange(quote_date) %>%
  mutate(return = (lead(close_price)-close_price)/close_price) %>%
  ungroup() %>%
  group_by(quote_date) %>%
  mutate(w = close_price/sum(close_price)) %>%
  ungroup() %>%
  select(quote_date,security_ID,return,w) %>%
  filter(!is.na(return)) %>% #stocks being removed from the index will have "na"
  filter(quote_date >= as.Date('2008-01-01'),quote_date <= as.Date('2010-12-31')+maturity-1)

# Create a list of unique quote dates within the analysis period (2008–2010)
# which will be used as reference dates for the realized correlation calculations.
period <- returns %>% select(quote_date) %>% filter(year(quote_date) %in% c(2008,2009,2010)) %>% distinct(quote_date)

#------------------------- 
#Step 2: Create rolling future return windows by linking each quote date.
#-------------------------

# Construct rolling maturity return windows
# Remove stocks with incomplete observations to ensure a balanced sample.
df_returns_joined <- returns %>%
  mutate(end_date = quote_date + maturity-1) %>%
  inner_join(returns, by = join_by(security_ID,x$quote_date <= y$quote_date,x$end_date >= y$quote_date),suffix = c(".a", ".b")) %>%
  group_by(quote_date.a, security_ID) %>%
  #we filter away those stocks that are removed from the DJIA index during the next #maturity days
  mutate(count = n()) %>%
  ungroup() %>%
  group_by(quote_date.a) %>%
  filter(count == max(count)) %>%
  ungroup() %>%
  select(quote_date = quote_date.a, security_ID, w = w.a, return = return.b, s = quote_date.b )

#------------------------- 
#Step 3: Calculate the realized covariances.
#-------------------------
#Realized covariance = numerator/denumerator, with:
#numerator = sum_{i != j}[w_i w_j s_i s_j R_{ij}}
#denumerator = sum_{i != j}[w_i w_j s_i s_j R_{ij}}
#with R the correlation matrix and s the sd dev of the next daily historical returns

#numerator:
numerator <- df_returns_joined %>%
  group_by(quote_date, s) %>%
  summarize(numerator_day_contribution = sum(w*return)^2-sum((w*return)^2),.groups = "drop") %>%
  group_by(quote_date) %>%
  summarize(numerator = sum(numerator_day_contribution),.groups = "drop")

#denumerator:
denumerator <- df_returns_joined %>%
  group_by(quote_date, security_ID,w) %>%
  summarize(wsigma_ID = first(w)*sqrt(sum(return^2)),.groups = "drop") %>%
  group_by(quote_date) %>%
  summarize(denumerator = sum(wsigma_ID)^2-sum((wsigma_ID)^2),.groups = "drop")

#Realized correlation:
realized_cor <- period %>%
  left_join(numerator, by =c("quote_date")) %>%
  left_join(denumerator, by =c("quote_date")) %>%
  mutate(realized_cor = numerator/denumerator) %>%
  select(quote_date,realized_cor)

#------------------------- 
#Output
#-------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
output_name <- paste0("Data/",maturity,"/Realized_Correlation_M",maturity,".csv")
write.table(realized_cor, output_name, row.names = FALSE,sep=";",dec=",")

#------------------------- 
#Extra: Plot of the realized correlation trend
#-------------------------

ggplot(realized_cor, aes(x = quote_date, y = realized_cor)) +
   geom_line(color = "#00407A", size = 1) +  # Blue line for trend
 
   labs(
     title = "Realized Correlation Trend",
     subtitle = "Analysis for 2008-2010",
     x = "Quote Date",
     y = "Realized Correlation"
   ) +
   theme_minimal()


  