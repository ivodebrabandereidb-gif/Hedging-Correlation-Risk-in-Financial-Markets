#------------------------- General Purpose -------------------------
# This file constructs figure 6 from the thesis, using the outputs: 
#   `Implied correlation per moneyness_M<Mat> - ATM.csv` from Implied Correlation per Moneyness.R
#   `Realized_Correlation_M<Mat>.csv` from Realized Correlation.R

#------------------------- !! Important !! -------------------------
# To obtain the figure run this script up to line 108 for,
#   maturity = 30 and
#   maturity = 90,
# before running the last lines.

library(lubridate)
library(ggplot2)
library(dplyr)
library(patchwork)
#detach("package:MASS", unload = TRUE)

#------------------------- 
#Input: Change maturity to either 30 or 90 days to obtain the results from the paper.
#-------------------------

maturity = 90

#------------------------- 
#Data Loader: Load the calculated implied and realized correlations, 
#calculated in 'Implied Correlation per Moneyness.R' and 'Realized Correlation.R'.
#-------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(paste0("Data/",maturity))

# Loading implied correlations
df_implied_cor <-as.data.frame(read.csv2(paste0("Implied correlation per moneyness_M",maturity," - ATM.csv"),sep=";", dec=","))
df_implied_cor <- df_implied_cor %>% arrange(quote_date)
df_implied_cor <- df_implied_cor %>% rename(correlation = rho_iv) %>% select(quote_date, correlation)

#loading realized correlations
df_realized_cor <- as.data.frame(read.csv2(paste0("../../../5. Model-free vs realized correlation/Data/",maturity,"/Realized_Correlation_M",maturity,".csv"),sep=";", dec=","))
df_realized_cor <- df_realized_cor %>% rename(correlation = realized_cor)

#------------------------- 
#Setting the right variables to make the plot.
#-------------------------
df_implied_cor$ID = "1"
df_realized_cor$ID = "2"
plot_data <- rbind(df_implied_cor,df_realized_cor)
plot_data$quote_date <- as.Date(plot_data$quote_date)
plot_data <- plot_data %>% arrange(ID,quote_date)


Sys.setlocale("LC_TIME", "English")
plmaturity <- ggplot(plot_data, aes(x = quote_date, y = correlation, color = ID, group = ID,linetype = ID)) +
  geom_line(linewidth = 0.5) + 
  scale_x_date(
    limits = c(as.Date("2008-01-01"), as.Date("2010-12-31")),
    breaks = seq(as.Date("2008-01-01"), as.Date("2010-12-31"), by = "6 months"),
    date_labels = "%b/%y",
    expand = c(0, 0) 
  ) +
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8))+
  labs(
    
    x = NULL,
    y = NULL,
    color = "Legend Title",
    linetype = "Legend Title"
  ) +
  scale_color_manual(
    values = c("1" = "#DD8A2E", "2" = "#1FABD5"),
    labels = c("1" = "ATM implied correlation", "2" = "Realized correlation")
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
  annotate("text", x = as.Date("2010-09-01"), y = 0.95, label = paste0(maturity,"-day window"))+
  scale_linetype_manual(
    values = c("1" = "solid", "2" = "21"),
    labels = c("1" = "ATM implied correlation", "2" = "Realized correlation")
  )

common_scale <- list(scale_color_manual(
  name = NULL,
  values = c("1" = "#DD8A2E", "2" = "#1FABD5"),
  labels = c("1" = "ATM implied correlation", "2" = "Realized correlation")),
  scale_linetype_manual(
    name = NULL,
    values = c("1" = "solid", "2" = "21"), 
    labels = c("1" = "ATM implied correlation", "2" = "Realized correlation")
  )
)

#------------------------- 
#Plotting for the given maturity (M)
#-------------------------

assign(
  paste0("pl", maturity),
  plmaturity
)

print(get(paste0("pl", maturity)))

#------------------------- 
#Constructing the plot from the paper, make sure to have run up to line 108 for both maturities.
#Both maturity 30 and 90 should be in memory
#-------------------------

top_row <- pl30/ pl90 + plot_layout(guides = "collect") & common_scale & theme(legend.position = "bottom")
print(top_row)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(paste0("Figures"))
ggsave('Figure6_ATM_Implied_vs_realized_correlation_30_90.png', plot=top_row,  width = 6.5,
       height = 5.25)

