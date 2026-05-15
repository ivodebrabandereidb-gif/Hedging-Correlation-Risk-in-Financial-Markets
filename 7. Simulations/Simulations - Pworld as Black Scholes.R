#------------------------- General Purpose -------------------------
# This file supports the analysis from Chapter 5 in the thesis. 

#------------------------- !! Important !! -------------------------
# To specifically obtain the figures from the thesis:
#   For figure 9 and 10 put:
#     d = 2 and 10
#     optiontype = "straddle" and "log"
#     nscenarios = 500
#     for_fig_11 = FALSE
#     run up to line 400
#     It will automatically take only the first 10 scenario's to plot figure 9.
#     After running all 4 scenario's, run lines 401 - 469 to make and save the figures.
#   For figure 11:
#     d = 10
#     optiontype = "straddle"
#     nscenarios = 10
#     for_fig_11 = TRUE
#     run all lines

library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(patchwork)
#detach("package:MASS", unload = TRUE)

#------------------------- 
#Input: Main parameters.
#-------------------------
#Nr of assets underlying the option
d = 10
#Nr of trading days in 1 year
nT = 252
#Nr of scenario's 
nscenarios = 10
#Maturity of the option (in years)
maturity= 1
#Type of hedge, can only be 'gamma'
greek_hedge = "gamma"
#The type of option, could be 'log' for a log-contract or 'straddle', for a straddle option.
optiontype = "straddle"
#Are the runs to obtain figure 11 from the thesis, put this to TRUE, 
#if it is to obtain figures 9 and 10 put it to FALSE.
#To obtain no particular figure, but for the analysis put it to TRUE.
for_fig_11 = TRUE

r=0.02
mu = 0.5
rho_P = 0.3
rho_Q = 0.6
ivP = 0.2
ivQ = 0.2

S0 = rep(200,d)

#computed parameter
dt = 1/nT
divisor = d #keeps index price around level of stocks


# Simulate stock paths in P-world


# Function that outputs list including simulated stock, variance and correlation paths useful for Q- and P-paths
# flag_antithetic used in Q-world to price options. NOT used in P-world -> otherwise correlated paths


EulerMaruyama_integrator <- function()
{
  Stocksim =   array(S0,c(d,nT+1,nscenarios))
  
  Sigma <- matrix(rho_P, nrow = d, ncol = d)
  diag(Sigma) <- 1
  L <- t(chol(Sigma))
    
  stocksim = array(S0,c(d,nT+1,nscenarios)) 
  
  for (i in (1:(nT)))
  {  
    Z = matrix(rnorm(d * nscenarios), nrow = d, ncol = nscenarios)
    
    # 2. Correlate them: L %*% Z results in the Multivariate Normal samples
    # W will be an (nsim x d) matrix to match your existing loop logic
    W = t(L %*% Z)
    
    for (sim in (1:nscenarios))
    {
      
    stocksim[,i+1,sim] = stocksim[,i,sim]*exp((r-ivP^2/2)*dt+ivP*sqrt(dt)*W[sim,])
    }
  }
  return(list(S = stocksim))
}

# Intermezzo: plotting an example
X <- EulerMaruyama_integrator()
plot(seq(0,1,1/252),X$S[1,,1])


#----------------------------
# Physical world: stock paths
#----------------------------

#SVR = Stock Variance Rho
Stocks <- EulerMaruyama_integrator()$S
Stocks_tp = Stocks[,2:(nT+1),]
#putting it in dataframe


# 1. 'Melt' the array into a data frame
# This automatically preserves numeric indices if the array doesn't have dimnames
df_s <- melt(Stocks,   varnames = c("security_id", "t", "scenario"), value.name = "Stock")
df_stp <- melt(Stocks_tp, varnames = c("security_id", "t", "scenario"), value.name = "Stock_tp")

# Insert security_id = d+1 for the index price

df_index <- df_s %>%
  group_by(scenario,t) %>%
  summarize(Stock = 1/divisor*sum(Stock),
            .groups = "drop")  %>%
  mutate(security_id = d+1) %>%
  select(scenario, t, security_id, Stock)

df <- rbind(df_s,df_index)

# Insert security_id = d+1 for the index price

df_index_tp <- df_stp %>%
  group_by(scenario,t) %>%
  summarize(Stock_tp = 1/divisor*sum(Stock_tp),
            .groups = "drop")  %>%
  mutate(security_id = d+1) %>%
  select(scenario, t, security_id, Stock_tp)

df_tp <- rbind(df_stp,df_index_tp)


#finalizing
df_simulated_marketdata <- df %>%
  left_join(df_tp, by = c("security_id", "t", "scenario")) %>%
  mutate(time = (t-1)/nT)


#--------------------------------------------------------
# Risk-neutral world: price Q-options using Black-Scholes
#--------------------------------------------------------

#determining the implied volatility of the index using lognormal approximations

iv_f <- function(security_id, Stockprices,rho){ #lognormal approximation
  iv = rep(0,d+1) #initialize vector
  iv[security_id != d+1] = ivQ
  
    w = Stockprices[security_id != d+1]/sum(Stockprices[security_id != d+1])
    dummy = sum(w^2)*exp(ivQ^2)
    
    for(i in 1:d)
    {
      j=1
      while(j<i)
      {
        dummy = dummy + 2*w[i]*w[j]*exp(ivQ^2*rho_Q*maturity) 
        j= j +1
      } 
    }
    iv[d+1] = sqrt(log(dummy)/maturity)
    
    return(data.frame(iv))
}

options(digits = 5)

df_simulated_marketdata <- df_simulated_marketdata %>%
  group_by(scenario,t) %>%
  arrange(security_id) %>%
  reframe(security_id = security_id,
          Stock = Stock,
          Stock_tp = Stock_tp,
          time = time,
          iv_f(security_id, Stock_tp,rho_Q)) %>%
  ungroup() %>%
  mutate(iv_tp = iv)

df_simulated_marketdata <- df_simulated_marketdata %>%
  group_by(scenario,t) %>%
  arrange(security_id) %>%
  reframe(security_id = security_id,
          Stock = Stock,
          Stock_tp = Stock_tp,
          time = time,
          iv_tp = iv,
          iv_f(security_id, Stock,rho_Q)) %>%
  ungroup() 



BS_price <- function(iv,S0,K,option_maturity,r, optiontype)
{
  d1 = 1/(iv*sqrt(option_maturity))*(log(S0/K)+(r+iv^2/2)*option_maturity)
  d2 = d1-iv*sqrt(option_maturity)
  price_call <- pnorm(d1)*S0-pnorm(d2)*K*exp(-r*option_maturity)
  price_put <-  pnorm(-d2)*K*exp(-r*option_maturity)-pnorm(-d1)*S0
  
  delta_call <- pnorm(d1)
  gamma_call <- dnorm(d1)/(S0*iv*sqrt(option_maturity))
  
  #straddle
  delta = 2*delta_call-1
  gamma = 2*gamma_call
  vega = K*exp(-r*option_maturity)*dnorm(d2,0,1)*(sqrt(option_maturity))
  if(optiontype == "straddle")
    {
     out = list(delta = delta, gamma = gamma, price = price_call+price_put)
    } else if (optiontype == "log"){
  out = list(
    price = exp(-r*option_maturity)*((r-iv^2/2)*option_maturity+log(S0/K)),
    delta = exp(-r*(option_maturity))/S0,
    gamma = -exp(-r*(option_maturity))/S0^2)
  }
  
  return(out)
}

df_simulated_marketdata <- df_simulated_marketdata %>%
  rowwise() %>%
  mutate(Option =  BS_price(iv,Stock,Stock,maturity,r,optiontype)$price,
         Option_tp = BS_price(iv,Stock_tp,Stock,maturity-dt,r,optiontype)$price,
         delta = BS_price(iv,Stock,Stock,maturity,r,optiontype)$delta,
         gamma = BS_price(iv,Stock,Stock,maturity,r,optiontype)$gamma) %>%
  ungroup()

# We compute option prices  O_t  = E_Q[ (S(t+T)-S(t))_+ | F_t]
#-------------------------------------------------------------

# Bob's portfolio is setup with ATM-options for which the strike at time t is given by S(t).

# Q-samples of S(t+T)| F_t


#----------------------
# Executing Bob's-trade
#----------------------

portfolio_f <- function(greek_hedge, security_id, gamma,delta,vega, Option,Stock,iv,rho_estimate)
{
  n = length(security_id)
  index = which(security_id == d+1)
  alpha_index = -sign(gamma[index])/Option[index]
  
  #We compute the vector signifying the shares in each option: shares_option
  if(greek_hedge == "gamma")
  {
    shares_option= -alpha_index*1/first(divisor)^2*gamma[index]/gamma
    shares_option[index] = alpha_index    
  } else if (greek_hedge == "vega"){
    #partial derivative of O_I w.r.t. sigma_i, with i the vector element
    z = 1/first(divisor)^2/IV[index]*(first(rho_estimate)*sum(IV)+(1-first(rho_estimate))*IV)
    partial_OI_partial_sigma_i = vega[index]*z
    
    shares_option = - partial_OI_partial_sigma_i/vega
    shares_option[index] = alpha_index
  }
  #Not correct yet: S_I = 1/D*(S1+...+Sd), the delta of the index needs to be scaled!!!
  shares_stock = -shares_option*delta
  
  shares_bank = -(sum(shares_option*Option)+sum(shares_stock*Stock))
  return(data.frame(shares_option,shares_stock, shares_bank))
}


df_trade_execution <- df_simulated_marketdata %>%
  group_by(time, scenario) %>%
  mutate(portfolio_f(greek_hedge,security_id,gamma,delta,vega,Option,Stock,iv,0)) %>%
  ungroup()

PNL_theoretical_f <- function(gamma, IV, S_Tp,S_T,dt)
{
  R = (S_Tp-S_T)/S_T
  return(0.5*gamma*S_T^2*(R^2-IV^2*dt)) #converts boolean to 1 and 0 values
}

df_trade_execution$PNL_theoretical <- PNL_theoretical_f(df_trade_execution$gamma, df_trade_execution$iv, df_trade_execution$Stock_tp,df_trade_execution$Stock,dt)

#--------------------------------
# Computing PNL of Bob's strategy
#--------------------------------

df_PNL <- df_trade_execution %>%
  group_by(time, scenario) %>%
  summarize(PNL_realized = sum(shares_option*(Option_tp-Option)+shares_stock*(Stock_tp-Stock))+first(shares_bank*(exp(r*dt)-1)),
            PNL_theoretical = sum(shares_option*PNL_theoretical),
            RM = sum((Stock_tp-Stock)/Stock*if_else(security_id==d+1,1,0)),
            #PNL_theoretical = sum(PNL_theoretical*shares_option),
            .groups = "drop") %>%
  filter(time != 1)


# cummulative PNL
df_PNL <- df_PNL %>%
  arrange(scenario, time) %>%
  group_by(scenario) %>%
  mutate(cumulative_PNL = cumsum(PNL_realized)) %>%
  ungroup()

#--------------------------------
# Plotting all scenario's ran.
#--------------------------------

ggplot(df_PNL, aes(x = time, y = cumulative_PNL, color = scenario, group = scenario)) +
  # Realized P&L Lines for all scenarios
  geom_line(alpha = 0.6, size = 0.8) + 
  labs(
    title = "Cumulative Realized P&L across Scenarios",
    subtitle = "Aggregated returns from options, stocks, and bank interest",
    x = "Time",
    y = "Cumulative P&L",
    color = "Scenario"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

#--------------------------------
# Preparing for figure 9. (only plotting up to scenario 10)
#--------------------------------

df_PNL_filtered <- df_PNL %>%
  filter(scenario <= 10)

pl_evol <- ggplot(df_PNL_filtered, aes(x  = time, y = cumulative_PNL, group = scenario)) + 
  geom_line(aes(colour = scenario), linewidth = 0.5, linetype = 1) +
  scale_colour_gradientn(colours = c("#1FABD5", "#00407A")) +
   ylab(expression("Cumulative P&L")) + xlab("time")+theme_minimal() +theme(aspect.ratio=0.66,legend.position = "none")+  theme(
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),panel.grid.minor = element_blank()
  )

assign(paste0("pl_evol_",optiontype, d),pl_evol)
print(paste0("pl_evol_",optiontype, d))

#------------------------------------------------------------
# Comparing with difference realized and implied correlations
#------------------------------------------------------------

df_diff_cov <- df_simulated_marketdata %>%
  filter(security_id != d+1) %>%
  group_by(scenario, security_id) %>%
  mutate(return = (Stock_tp-Stock)/Stock) %>%
  mutate(w = Stock/sum(Stock)) %>%
  ungroup() %>%
  filter(!is.na(return)) %>%
  group_by(time, scenario) %>%
  summarize(realized_cov_day = sum(w*return)^2-sum((w*return)^2),
            implied_cov_day = dt*ivQ^2*rho_Q*(sum(w)^2-sum(w^2)),
            denumerator_day = dt*(sum(w)^2-sum(w^2)),
            .groups = "drop") %>%
  group_by(scenario) %>%
  summarize(realized_cov = sum(realized_cov_day),
            implied_cov  = sum(implied_cov_day),
            denumerator  = sum(denumerator_day),
            .groups = "drop") %>%
  mutate(diff_impl_real_cov = (implied_cov-realized_cov)/denumerator)

df_tot_PNL <- df_PNL %>%
  filter(time == 1-dt)

df_scatter_plot <- df_tot_PNL %>%
  inner_join(df_diff_cov, by = c("scenario"))

#--------------------------------
# Preparing figure 10.
#--------------------------------

pl_cummul <- ggplot(df_scatter_plot, aes(x = diff_impl_real_cov, y = cumulative_PNL)) +
  geom_point(alpha = 0.5, color = "#00407A", size = 0.15) + 
  labs(
    x = "Average (implied - realized) covariance",
    y = "Total P&L",
  ) +scale_x_continuous(breaks = c(0.008,0.01, 0.012,0.014,0.016)) +

  geom_vline(xintercept = 0.012, color = "#DD8A2E", linewidth = 1, linetype = "dashed") +
  theme_minimal()+ 
  theme(
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),panel.grid.minor = element_blank()
  )

plot_time <- paste0("pl_evol_",optiontype, d)
plot_cumul <- paste0("pl_scatter_",optiontype, d)

assign(plot_time,pl_evol)
assign(plot_cumul,pl_cummul)

print(paste0("pl_scatter_",optiontype, d))
print(paste0("pl_evol_",optiontype, d))

get(paste0("pl_scatter_",optiontype, d))
get(paste0("pl_evol_",optiontype, d))

#------------------------- 
# Constructing figures 9 and 10 from the paper, make sure to have run up to line 397 for the following 4 scenarios.
# Log-contract (optiontype) with 2 assets (d).
# Log-contract (optiontype) with 10 assets (d).
# Straddle-contract (optiontype) with 2 assets (d).
# Straddle-contract (optiontype) with 10 assets (d).
# All 4 should be in memory.
# When all 4 are in memory, run up to 469 to obtain figure 9 and 10.
#-------------------------

if (!for_fig_11) {
  #-------------------------
  # Figure 9
  #-------------------------
  
  #Test if everything is loaded properly
  pl_evol_log2
  pl_evol_log10
  pl_evol_straddle2
  pl_evol_straddle10
  
  # Combine plots: (Top Row) / (Bottom Row)
  combined_plot <- ((pl_evol_log2) + pl_evol_log10) / 
    ((pl_evol_straddle2)+ pl_evol_straddle10) + 
    plot_annotation(tag_levels = 'a')
  
  # Display the result
  combined_plot
  
  #Save the result for figure 9
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  setwd(paste0("Figures"))
  ggsave('Figure9_log2log10straddle2straddle10_evol.png', 
         plot=combined_plot,
         width = 6.5,
         height = 5,
         units = "in",   
         dpi = 300       
  )
  
  #-------------------------
  # Figure 10
  #-------------------------
  
  #test if all 4 scenario's are in memory.
  pl_scatter_log2
  pl_scatter_log10
  pl_scatter_straddle2
  pl_scatter_straddle10
  
  # Combine plots: (Top Row) / (Bottom Row)
  combined_plot <- ((pl_scatter_log2 +scale_x_continuous(breaks = c(0.006,0.009, 0.012,0.015,0.018))) + pl_scatter_log10) / 
    ((pl_scatter_straddle2 +scale_x_continuous(breaks = c(0.006,0.009, 0.012,0.015,0.018)))+ pl_scatter_straddle10) + 
    plot_annotation(tag_levels = 'a')
  
  # Display the result
  combined_plot
  
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  setwd(paste0("Figures"))
  ggsave('Figure10_log2log10straddle2straddle10_scatter.png', 
         plot=combined_plot,
         width = 6.5,
         height = 5,
         units = "in",   
         dpi = 300       
    )
}

#-------------------------
# Analysis of returns
#-------------------------

#1. Single index model / CAPM
df_CAPM <- df_PNL %>%
  mutate(Re= PNL_realized-((1+r)^(1/365)-1), RMe = RM-((1+r)^(1/365)-1)) %>%
  select(Re,RMe)

capm_model <- lm(Re ~ RMe, data = df_CAPM)

summary(capm_model)

#-------------------------
# Prepare for figure 11.
#-------------------------

pl_capm <- ggplot(df_CAPM, aes(x = RMe*100, y =Re*100)) +
  geom_point(alpha = 0.5, color = "#00407A", size = 0.2) +
  scale_y_continuous(breaks = c(-0.75,-0.5,-0.25, 0,0.25),limits = c(-0.75, 0.25),expand = c(0, 0)
  ) +  
  labs(
    x = "Index excess return (%)",
    y = "Option disperion trade excess return (%)",
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


pl_real_vs_theor <- ggplot(df_PNL, aes(x = PNL_theoretical*100, y = PNL_realized*100)) +
  geom_point(alpha = 0.5, color = "#00407A", size = 0.2) +
  geom_abline(intercept = 0, slope = 1, color = "#DD8A2E", linetype = "21", linewidth = 0.78) +
  scale_y_continuous(breaks = c(-0.75,-0.5,-0.25, 0,0.25),limits = c(-0.75, 0.25),expand = c(0, 0))+
  scale_x_continuous(breaks = c(-0.75,-0.5,-0.25, 0,0.25),limits = c(-0.75, 0.25),expand = c(0, 0))+
  labs(
    x = "Theoretical P&L (%)",
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

pl_analysis <- (pl_real_vs_theor + pl_capm)

# Display the result
print(pl_analysis)

# Save the result
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(paste0("Figures"))
ggsave('Figure11_Simulations_Analysis.png', 
plot=pl_analysis,
width = 7,
height = 3.5,
units = "in", 
dpi = 300     
)
