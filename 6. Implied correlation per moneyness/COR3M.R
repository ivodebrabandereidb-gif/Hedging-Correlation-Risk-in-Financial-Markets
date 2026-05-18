#------------------------- General Purpose -------------------------
# This file constructs figure 2 from the thesis, using the COR3M Data in '2. Raw option input'. 

library(jsonlite)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyquant)

#---------------------------------------------------
#Plotting COR3M (CBOE implied correlation, 3 months)
#---------------------------------------------------
#.json comes from https://www.cboe.com/us/indices/dashboard/cor3m/ by right-mouse clicking "inspect"

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../2. Raw option data/COR3M Data")


# 1. Load the data
raw_json <- fromJSON("COR3M (CBOE).json")
df <- raw_json$data

df_clean <- df %>%
  mutate(
    date  = as.Date(date),        
    close = as.numeric(close),    
    open  = as.numeric(open),    
    high  = as.numeric(high),
    low   = as.numeric(low)
  )

pl_cor3m <- ggplot(df_clean, aes(x = date, y = close/100)) +
 scale_x_date(
    limits = c(as.Date("2006-01-03"), as.Date("2026-07-31")),
    breaks = seq(as.Date("2006-01-03"), as.Date("2026-03-06"), by = "60 months"),
    date_labels = "%Y",
    expand = c(0, 0) 
  ) +
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, by = 0.2), 
    expand = c(0, 0)              
  ) +  
  labs(
    x = NULL,
    y = "COR3M Index",
    color = "Legend Title",
    linetype = "Legend Title"  )+
  # 50-day Moving Average
  geom_ma(ma_fun = SMA, n = 50, color = "#1FABD5", linetype = 1, linewidth = 0.5) +
  
  theme_minimal()+ 
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
  )
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("Figures")
ggsave('Fig2_COR3M.png', 
       plot=pl_cor3m,
       width = 6.5,
       height = 3.25,
       units = "in",   # Always specify inches
       dpi = 300       # High resolution for printing
       )
