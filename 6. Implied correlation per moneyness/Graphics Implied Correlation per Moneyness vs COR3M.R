#------------------------- General Purpose -------------------------
# This file constructs figure 6 from the thesis, using the outputs: 
#   `Implied correlation per moneyness_M<Mat> - ATM.csv` from Implied Correlation per Moneyness.R
#   `COR3M Data` in `2. Raw option input`

#------------------------- !! Important !! -------------------------
# To obtain the figure from the thesis, run using maturity = 90.

library(lubridate)
library(ggplot2)
library(dplyr)
library(jsonlite)
library(patchwork)
#detach("package:MASS", unload = TRUE)

#------------------------- 
#Input: Maturity = 90 is used in the thesis. The COR3M index is a measure of implied correlations over 90 days.
#-------------------------

maturity = 30

#------------------------- 
#Data Loader: Load the calculated implied correlations, 
#calculated in 'Implied Correlation per Moneyness.R'.
#-------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(paste0("Data/",maturity))

df_implied_cor <- as.data.frame(read.csv2(paste0("Implied correlation per moneyness_M",maturity," - ATM.csv"),sep=";", dec=","))
df_implied_cor <- df_implied_cor %>% arrange(quote_date)
#possibly smooth with: (implied_corr+lead(implied_corr))/2
df_implied_cor <- df_implied_cor %>% mutate(correlation = rho_iv, quote_date = as.Date(quote_date)) %>% select(quote_date, correlation)

#-------------------------
#Loading COR3M (CBOE implied correlation, 3 months)
#-------------------------
#.json comes from https://www.cboe.com/us/indices/dashboard/cor3m/ by right-mouse clicking "inspect" 

# 1. Load the data
# Note: Cboe data often nests the actual values inside a "data" or "values" key
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(paste0("../2. Raw option data/COR3M Data"))
raw_json <- fromJSON("COR3M (CBOE).json")
df <- raw_json$data

df_cor3m <- df %>%
  mutate(
    quote_date  = as.Date(date),  
    close = as.numeric(close),  
    open  = as.numeric(open),    
    high  = as.numeric(high),
    low   = as.numeric(low)
  ) 

#filter on dates appearing in implied correlation dataset
df_cor3m <- df_cor3m %>%
  inner_join(df_implied_cor, by = c("quote_date")) %>%
  mutate(correlation = close/100) %>%
  select(quote_date, correlation)

#------------------------- 
#Setting the right variables to make the plot.
#-------------------------
df_implied_cor$ID = "1"
df_cor3m$ID = "2"
plot_data <- rbind(df_implied_cor,df_cor3m)
plot_data$quote_date <- as.Date(plot_data$quote_date)
plot_data <- plot_data %>% arrange(ID,quote_date)

# ------- The following piece of code to plot the graph was generated using Gemini (Google, 2026) -------
Sys.setlocale("LC_TIME", "English")
plmaturity <- ggplot(plot_data, aes(x = quote_date, y = correlation, color = ID, group = ID,linetype = ID)) +
  geom_line(linewidth = 0.5) + 
  scale_x_date(
    limits = c(as.Date("2008-01-01"), as.Date("2010-12-31")),
    breaks = seq(as.Date("2008-01-01"), as.Date("2010-12-31"), by = "6 months"),
    date_labels = "%b/%y",
    expand = c(0, 0) 
  ) +
  labs(
    
    x = NULL,
    y = NULL,
    color = "Legend Title",
    linetype = "Legend Title"
  ) +
  scale_color_manual(
    values = c("1" = "#DD8A2E", "2" = "#1FABD5"),
    labels = c("1" = "ATM implied correlation (90-Day window)", "2" = "COR3M Index")
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
  scale_linetype_manual(
    values = c("1" = "solid", "2" = "21"),
    labels = c("1" = "ATM implied correlation (90-Day window)", "2" = "COR3M Index")
  )

common_scale <- list(scale_color_manual(
  name = NULL,
  values = c("1" = "#DD8A2E", "2" = "#1FABD5"),
  labels = c("1" = "ATM implied correlation (90-Day window)", "2" = "COR3M Index")),
  scale_linetype_manual(
    name = NULL,
    values = c("1" = "solid", "2" = "21"),
    labels = c("1" = "ATM implied correlation (90-Day window)", "2" = "COR3M Index")
  )
)
# ------- The previous piece of code to plot the graph was generated using Gemini (Google, 2026) -------

#------------------------- 
#Plotting for the given maturity (M)
#-------------------------
assign(
  paste0("pl", maturity),
  plmaturity
)

print(get(paste0("pl", maturity)))

#------------------------- 
#Constructing the plot from the paper.
#-------------------------

top_row <- get(paste0("pl", maturity)) + plot_layout(guides = "collect") & common_scale & theme(legend.position = "bottom")
top_row
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("Figures")


ggsave(paste0('Figure6_ImpliedATM_vs_COR3M_M',maturity,'.png'), plot=top_row,  width = 7,
       height = 3.5)

