#------------------------- General Purpose -------------------------
# This file computes the implied correlation over moneyness levels and produces Figure 7, as is explained in 
# section 2.3.3 of the thesis using the implementation of Section 3.3.1.
# To obtain the output necessary for other files, run this script twice
# using,
#   maturity = 30 and
#   maturity = 90.

library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)

#------------------------- 
#Input: Change maturity to either 30 or 90 days to obtain the results from the paper.
#-------------------------

maturity = 90
months = c("1","2","3","4","5","6","7","8","9","10","11","12")
years = c("2008","2009","2010")

#------------------------- 
#Data Loader: Load the relevant option data to do the analysis on. 
#-------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../3. Data loader")
source("Loading Data.R")

#------------------------- 
#Step 1: Cleaning data set
#-------------------------

#Filter the dataset for active, traded OTM-options
data_cleaned = data[data["open_interest"]>0 & data["best_bid"]>0 & (data["delta"]< 0.5 & data["delta"]>-0.5),]
# Filter out short and long maturities
data_cleaned = data_cleaned[data_cleaned["expiration"]>=6 & data_cleaned["expiration"]<=maturity+200,]

#Sometimes, for the same strike there is both a call and a put with delta <0.5 and delta >-0.5 (like delta 0.49 and -0.49).
#This causes problems. Below, we keep the option with highest open_interest and lowest bid/ask-spread
data_cleaned <- data_cleaned  %>%
  group_by(security_ID, quote_date, expiration, strike_price) %>%
  filter(open_interest == max(open_interest)) %>%
  filter((best_offer-best_bid) == min(best_offer-best_bid)) %>%
  filter(max(delta)== delta) %>% #to remove any duplicates left
  ungroup()

# Only keep maturities with more than two strikes
data_cleaned <- data_cleaned %>%
  group_by(security_ID, quote_date, expiration) %>%
  filter(n() > 2) %>%
  ungroup()

#introduce a column 'moneyness', depicting the state of each option
data_cleaned["moneyness"] = data_cleaned["strike_price"]/data_cleaned["close_price"]


#table to do sanity check
strike_counts <- data_cleaned %>%
  group_by(security_ID, quote_date, expiration) %>%
  summarize(n_strikes = n_distinct(strike_price),.groups = "drop")

#-------------------------
#Step 2: Interpolating implied volatilities (IV) across moneyness and maturities
#-------------------------

#Step 2.1: 
# Create a new dataset with the closest expiration slices to the given maturity (M) (in our example 30 or 90 days). Both under and over the given maturity. 
# If the option data already contains a slice at the given maturity, we only keep that one. 
# Next we put the IV in separate columns

#Step 2.1.1: get detailed data for the closest maturities
data_closest_maturities_detail <- data_cleaned %>%
  group_by(security_ID, quote_date) %>%
  mutate(Tshort = max(expiration[expiration <= maturity], na.rm = TRUE),
         Tlong  = min(expiration[expiration >= maturity], na.rm = TRUE)
  ) %>%
  filter(expiration == Tshort | expiration == Tlong) %>%
  ungroup()

#Step 2.1.2: Pivot the dataset so that each row represents a unique security, date, and moneyness level, with separate columns for IVshort (T < M) and IVlong (T > M).
# In case a moneyness is defined for only one expiration, the IV will be NA for the other expiration. 

data_closest_maturities <- data_closest_maturities_detail %>%
  mutate(time_type = if_else(expiration == Tshort, "IVshort", "IVlong")) %>%
  pivot_wider(
    id_cols = c(security_ID, quote_date, moneyness, Tshort, Tlong),
    names_from = time_type,
    values_from = implied_volatility
  )

#Step 2.2
# Now we can interpolate across strikes to fill in a grid of given strikes (moneyness)

# 2.2.1 We first define the grid of moneyness between which to interpolate 
moneyness_grid = seq(0.75,1.25,0.025)
# 2.2.2  Secondly, we define the securities and quote days over which we interpolate. 
data_to_interpolate <- unique(data_closest_maturities[, c("security_ID", "quote_date")])
data_to_interpolate <- merge(data_to_interpolate, data.frame(moneyness_grid =moneyness_grid), by = NULL)

# 2.2.3 Join the market data to your grid first
data_to_interpolate <- data_to_interpolate %>%
  left_join(data_closest_maturities, by = c("security_ID", "quote_date"))

# 2.2.4 Now interpolate using the columns already present in the table
data_interpolated_int <- data_to_interpolate %>%
  group_by(security_ID, quote_date,moneyness_grid, Tshort, Tlong) %>%
  summarize(
    IVlong_interpolated = if(sum(!is.na(IVlong)) >= 2) {
      approx(x = moneyness, y = IVlong, xout = moneyness_grid[1], rule = 2)$y #[1] to make sure approx returns a single value, allowed since we group by per moneyness_grid
    } else { NA_real_ },
    
    IVshort_interpolated = if(sum(!is.na(IVshort)) >= 2) {
      approx(x = moneyness, y = IVshort, xout = moneyness_grid[1], rule = 2)$y
    } else { NA_real_ },
    .groups = "drop"
  )

#-------------------------
#Step 3: We now interpolate over the expiration to get the IV's for the given maturity (M)
#-------------------------

#We interpolate total variance: T sigma(T)^2
data_interpolated <- data_interpolated_int %>%
  mutate(
    IV_maturity = case_when(
      # if both IVs are given and Tshort and Tlong are different (i.e. not equal to 30), then interpolation
      !is.na(IVshort_interpolated) & !is.na(IVlong_interpolated) & Tshort != Tlong ~
          sqrt((IVshort_interpolated^2 * Tshort * (Tlong - maturity) / (Tlong - Tshort) +IVlong_interpolated^2  * Tlong  * (maturity - Tshort) / (Tlong - Tshort)) / maturity),
      
      !is.na(IVshort_interpolated) & !is.na(IVlong_interpolated) & Tshort == Tlong ~ IVshort_interpolated,
      # Case 2: Only Short exists
      !is.na(IVshort_interpolated) ~ IVshort_interpolated,
      
      # Case 3: Only Long exists
      !is.na(IVlong_interpolated)  ~ IVlong_interpolated,
      
      # Default: If none of the above are true, it becomes NA
      TRUE ~ NA_real_
  )
  )

#-------------------------
#Step 4: The stock price comprising the index DOJ are loaded, and the rescaled weights are computed
#-------------------------


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../2. Raw option data/2008")

#load the weights from the received option data
df_divisor_DJIA = as.data.frame(read.csv2("weights_2008.csv", sep=",", header = T, dec = "."))

df_divisor_DJIA <- df_divisor_DJIA %>% mutate(quote_date = as.Date(quote_date, origin = "1899-12-30"))

#Calculate rescaled weights
df_individual  <- data_interpolated %>%
  left_join(df_divisor_DJIA, by = c("quote_date","security_ID")) %>%
  filter(security_ID != 102456) %>%  
  group_by(quote_date, moneyness_grid) %>%
  mutate(weight_rescaled = close_price/sum(close_price)) %>% 
  ungroup()


#-------------------------
#Step 5: Computing the implied correlation for each moneyness
#-------------------------

df_numerator_sum <- df_individual %>%
  group_by(quote_date, moneyness_grid) %>%
  summarize(numerator_sum = sum((weight_rescaled*IV_maturity)^2), .groups ="drop")

df_denumerator_sum <- df_individual %>%
  group_by(quote_date, moneyness_grid) %>%
  summarize(denumerator_sum =  sum(weight_rescaled*IV_maturity)^2-sum((weight_rescaled*IV_maturity)^2),
            average_iv = sum(weight_rescaled*IV_maturity),
            .groups ="drop")

df_index_iv <- data_interpolated %>%
  filter(security_ID == 102456) %>%  
  select(quote_date, moneyness_grid, IV_maturity)


#-------------------------
# Step 6: Final table
#-------------------------

# Step 6.1: Calculate the implied correlation per level of moneyness (called "rho_iv" below)
df_implied_correlations <- df_index_iv  %>%
  left_join(df_numerator_sum, by = c("quote_date","moneyness_grid")) %>%
  left_join(df_denumerator_sum, by = c("quote_date","moneyness_grid")) %>%
  mutate(rho_iv = (IV_maturity^2-numerator_sum)/denumerator_sum)

#-------------------------
# Step 7: Exporting .csv containing implied levels of correlations and plotting the correlation skew (Figure 8).
#-------------------------

# Step 7.1
# We filter the results to remove outliers were the correlation skew is not representative
df_general_trend_plot <- df_implied_correlations %>%
  filter(moneyness_grid >= 0.80, moneyness_grid <= 1.2) %>%
  group_by(quote_date) %>%
  arrange(moneyness_grid) %>%
  # Flag and remove days where the correlation slope (change across moneyness) is 
  # unrealistically steep, indicating potential data errors.
  mutate(flag = if_else(all(diff(rho_iv) <=0.4*0.025),1,0)) %>%
  ungroup() %>%
  filter(flag == 1)

legend_breaks <- as.numeric(as.Date(c("2008-01-01", "2009-01-01", "2010-01-01")))
legend_limits <- as.numeric(as.Date(c("2008-01-01", "2010-12-31")))

# ------- The following piece of code to plot the graph was generated using Gemini (Google, 2026) -------
pl2 <- ggplot(df_general_trend_plot, 
              aes(x = moneyness_grid, 
                  y = rho_iv, 
                  group = quote_date,
                  colour = as.numeric(quote_date))) +
  geom_line(alpha = 0.6, linewidth = 0.5) +
  scale_colour_gradientn(
    name = "Date",
    colours = c("#1FABD5", "#00407A"),
    limits = legend_limits,
    breaks = legend_breaks,
    labels = function(x) as.Date(x, origin = "1970-01-01")
  ) +
  scale_x_continuous(breaks = c(0.8,0.9,1,1.1,1.2)) +
  # Fixed Labels
  labs(
    x = "Moneyness",
    y = expression("Implied correlation"), 
  ) +
  theme_bw() + 
  theme(
    aspect.ratio = 0.66,
    legend.position = "right",
    panel.grid.minor = element_blank()
  )
# ------- The previous piece of code to plot the graph was generated using Gemini (Google, 2026) -------

print(pl2)

# Step 7.2: Exporting output
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('Figures')
file_name <- paste0("Figure7_Implied_correlations_vs_moneyness_and_time_M", maturity, ".png")

ggsave(file_name, 
       plot=pl2,
       width = 6.5,
       height = 3.25,
       units = "in",   # Always specify inches
       dpi = 300       # High resolution for printing
)

#--------------------------------------------
# EXTRA: Plotting correlation skew for specific date
#--------------------------------------------
df_rho_skew_date <- df_implied_correlations %>% filter(quote_date == as.Date('2008-07-21'))

# ------- The following piece of code to plot the graph was generated using Gemini (Google, 2026) -------
pl_cor3m <- ggplot(df_rho_skew_date, aes(x = moneyness_grid, y = rho_iv)) +
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, by = 0.2), # Adds labels at 0, 0.2, 0.4, etc.
    expand = c(0, 0)              # Removes the padding at the top/bottom
  ) +  
  labs(
    x = NULL,
    y = "COR3M Index",
    color = "Legend Title",
    linetype = "Legend Title"  )+
  # 50-day Simple Moving Average
  geom_line(color = "#1FABD5", linewidth = 1) +
  
  theme_minimal()+ # Apply the base theme first
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
  )
# ------- The previous piece of code to plot the graph was generated using Gemini (Google, 2026) -------

print(pl_cor3m)
#--------------------------------------------
# Saving data for later
#--------------------------------------------


output_path <- '../Data/'
df_rhoIV_ATM <- df_implied_correlations %>% filter(moneyness_grid == 1) %>% select(quote_date, rho_iv, denumerator_sum, average_iv)
output_name <- paste0(output_path,maturity,"/Implied correlation per moneyness_M",maturity," - ATM.csv")
write.table(df_rhoIV_ATM, output_name, row.names = FALSE,sep=";",dec=",")


#--------------------------------------------
# Extra: plot of the implied correlation over time
#--------------------------------------------

# ------- The following piece of code to plot the graph was generated using Gemini (Google, 2026) -------
ggplot(df_rhoIV_ATM, aes(x = quote_date)) +
  # Realized P&L Line
  geom_line(aes(y = rho_iv, color = "Implied Correlation"), linewidth = 0.5) +
  labs(
    title = "Implied correlation over time",
    x = "Quote Date",
    y = "Implied Correlation",
    color = "Type"
  ) +
  theme_minimal()
# ------- The previous piece of code to plot the graph was generated using Gemini (Google, 2026) -------
