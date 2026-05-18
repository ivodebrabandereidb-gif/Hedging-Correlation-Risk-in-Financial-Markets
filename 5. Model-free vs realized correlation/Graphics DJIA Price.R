library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)

#Example of variables
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

df_index <- weights %>%
  filter(quote_date >= '2008-01-01' & quote_date <= '2010-12-31') %>%
  group_by(quote_date) %>%
  summarize(price_index = sum(weights*close_price), .groups = "drop") 
  
  
pl_index <- ggplot(df_index, aes(x  = quote_date, y = price_index)) + 
  geom_line(linewidth = 0.5, linetype = 1, colour = "#1FABD5") +
  scale_x_date(
    limits = c(as.Date("2008-01-01"), as.Date("2010-12-31")),
    breaks = seq(as.Date("2008-01-01"), as.Date("2010-12-31"), by = "6 months"),
    #Format of values on x-axis
    date_labels = "%b/%y",
    #Ensure the line touches the edges of the plot
    expand = c(0, 0) 
  ) +
  labs( x = NULL)+
  scale_colour_gradientn(colours = c("#1FABD5", "#00407A")) +
   ylab(expression("Index value")) +theme_minimal() +theme(aspect.ratio=0.66,legend.position = "none")+  theme(
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),panel.grid.minor = element_blank()
  )

print(pl_index)
setwd("Figures")
ggsave('Fig3_Price_Level_DJIA.png', 
       plot=pl_index,
       width = 6.5,
       height = 3.25,
       units = "in",   # Always specify inches
       dpi = 300       # High resolution for printing
)
