# Hedging-Correlation-Risk-in-Financial-Markets
This repository is made as a central location where the code is stored to recreate the results and analyses for the thesis 'Hedging Correlation Risk in Financial Markets', by Bram Verjans and Ivo De Brabandere.
The thesis was submitted during academic year 2025-2026 to the Katholieke Universiteit Leuven to obtain the degree of Master Actuarial and Financial Engineering.

## Data policy

Due to the price associated to a dataset containing option prices, delta's etc..., for multiple assets and the index, the decision was made not to make the dataset and any intermediate output with extension `.csv` available through the GitHub repository. Output using the extension `.png` are available through the GitHub repository.
Before starting, the dataset will have to be manually put into the following folders: 
- `2. Raw option data / 2008`,
- `2. Raw option data / 2009`,
- `2. Raw option data / 2010`.
More information on the type and format of data that should be stored in the folders above, can be found in separate `ReadMe` files inside these folders. To be able to guarantee that everything runs smoothly, it is important to keep the data format, names of columns and files etc... as is.
When the correct data is stored in these folders, it will be possible to compute the intermediate output files.

## Versions

- R: "4.5.0"
- package "lubridate": "1.9.5"
- package "ggplot2": "4.0.3"
- package "dplyr": "1.2.1" 
- package "jsonlite": "2.0.0"
- package "patchwork": "1.3.2"
- package "rstudioapi": "0.18.0"
- package "tidyr": "1.3.2"
- package "tidyquant": "1.0.12"
- package "MASS": "7.3-65"
- package "scales": "1.4.0"

## Overview Chapters from the Thesis and Accompanying R-Scripts
The following table gives an overview of the chapters from the thesis and the applicable R-scripts accompanying those chapters.

| Chapter | R-Script | Comment |
|---|---|---|
| 1. Literature study | `Value-at-Risk and Expected Shortfall Calculation.R` | Performs the analysis in Section 1.2 |
| 2. Preliminaries  | `COR3M.R` | Constructs figure 2, included in Section 2.3.3 |
| 3. Empirical measurement of the correlation gap | `Graphics DJIA Price.R` | Constructs figure 3, included in Section 3.1 |
| 3. Empirical measurement of the correlation gap | `Realized Correlation.R` | Computes realized correlations as explained in Section 3.3.1 |
| 3. Empirical measurement of the correlation gap  | `Model-Free Implied Correlation.R` | Computes the model-free implied correlations as explained in Section 3.3.1 |
| 3. Empirical measurement of the correlation gap | `Implied Correlation per Moneyness.R` | Computes the implied correlations per moneyness as explained in Section 3.3.1 |
| 3. Empirical measurement of the correlation gap | `Graphics Model-Free Implied vs Realized Correlation.R` | Constructs figure 4, included in Section 3.3.2 |
| 3. Empirical measurement of the correlation gap  | `Graphics Implied per Moneyness vs Realized.R` | Constructs figure 5, included in Section 3.3.2 |
| 3. Empirical measurement of the correlation gap  | `Graphics Implied Correlation per Moneyness vs COR3M.R` | Constructs figure 6, included in Section 3.3.2 |
| 5. Option dispersion trading in a simulated world  | `Simulations - Pworld as Black Scholes.R` | The whole analysis included in Chapter 5 can be found here. |
| 6. Backtesting option dispersion trades on empirical data | `Cleaning Data and Synthesizing Option Type.R` | This file contains the computations explained in Section 6.1 |
| 6. Backtesting option dispersion trades on empirical data  | `Cumulative Gains and Analysis - Gamma Strategy.R` | This file contains the analysis in Section 6.2 |
| 6. Backtesting option dispersion trades on empirical data  | `Cumulative Gains and Analysis - Vega Strategy.R` | This file contains the analysis in Section 6.3 |

## General Workflow
This section serves as an optimal order to run all R-scripts.
When running an R-script, make sure that the necessary input is available.
A general rule is that `.csv`-files are stored in the Data folder contained in the same folder as the R-script that outputs it.
`.png`-files can be found in the Figures folder.

### 1. '1. Package installer / Package Installer.R'
#### Purpose
Installs the necessary packages for all upcoming R-files.

### 2. '3. Data loader / Loading Data.R'

#### Purpose
Loads and combines the raw option data, zero-coupon bond data, and portfolio weights for 2008–2010. 
This file is intended to be sourced by other scripts and and is not meant to be executed as a standalone.

#### Inputs
- `optdata_<YYYY>_<M>.csv`
- `zerocd_<YYYY>.csv`
- `weights_<YYYY>.csv`
The combination of these three files is later referred to as 'the option data'.

#### Outputs
- `data`
- `zcb`
- `weights`

### 3. '4. Correlation risk in portfolio management / Value-at-Risk and Expected Shortfall Calculation.R'

#### Purpose

This file is constructed to give insight in the results explained in section 1.2 of the thesis. 
Furthermore, it constructs figure 1.

#### Inputs
- /

#### Outputs
- `Fig1_VaR_ES_Portfolios.png`

### 4. '5. Model-free vs realized correlation /  Graphics DJIA Price.R'

#### Purpose

This program takes the data on the stock prices composing the index and computes the index price per date.
With this information, Figure 3 is created plotting the price of the index over time.

#### Inputs
- The option data loaded using the `Loading Data.R`.

#### Outputs
- `Fig3_Price_Level_DJIA.png`


### 5. '5. Model-free vs realized correlation / Realized Correlation.R'

#### Purpose

This file computes the realized correlation (RC) as is explained in section 2.3.1 of the thesis using the implementation of Section 3.3.1.

#### Inputs
- The weights given in `weights_<YYYY>.csv` in `2. Raw option data`.

#### Outputs
- `Realized_Correlation_M<Mat>.csv`
with `<Mat>` depicting the maturity specified.

### 6. '5. Model-free vs realized correlation / Model-Free Implied Correlation.R'

#### Purpose

This file computes the model-free implied correlation (MFIC) as is explained in section 2.3.2 of the thesis using the implementation of Section 3.3.1.

#### Inputs
- The option data loaded using the `Loading Data.R`.

#### Outputs
- `Model_Free_Implied_Correlation_M<Mat>.csv`
- `INTERMEZZO_heatmap_2010_12_M<Mat>.png`

### 7. '5. Model-free vs realized correlation / Graphics Model-Free Implied vs Realized Correlation.R'

#### Purpose

This file constructs figure 4 from the thesis.

#### Inputs
- `Model_Free_Implied_Correlation_M<Mat>.csv` from `Model-Free Implied Correlation.R`
- `Realized_Correlation_M<Mat>.csv` from `Model-Free Implied Correlation.R`

#### Outputs
- `Figure4_ModelFree_Implied_vs_Realized_correlation.png`

### 8. '6. Implied correlation per moneyness / COR3M.R'

#### Purpose

This file constructs figure 2 from the thesis.

#### Inputs
- `COR3M Data` in `2. Raw option input`

#### Outputs
- `Fig2_COR3M.png`

### 9. '6. Implied correlation per moneyness / Implied Correlation per Moneyness.R'

#### Purpose

This file computes the implied correlation over moneyness levels and produces Figure 7, as is explained in section 2.3.3 of the thesis using the implementation of Section 3.3.1.

#### Inputs
- The option data loaded using the `Loading Data.R`.

#### Outputs
- `Figure7_Implied_correlations_vs_moneyness_and_time_M<Mat>.png`
- `Implied correlation per moneyness_M<Mat> - ATM.csv`

### 10. '6. Implied correlation per moneyness / Graphics Implied per Moneyness vs Realized.R'

#### Purpose

This file constructs figure 5 from the thesis.

#### Inputs
- `Implied correlation per moneyness_M<Mat> - ATM.csv` from `Implied Correlation per Moneyness.R`
- `Realized_Correlation_M<Mat>.csv` from `Realized Correlation.R`

#### Outputs
- `Figure5_ATM_Implied_vs_realized_correlation_30_90.png`

### 11. '6. Implied correlation per moneyness / Graphics Implied Correlation per Moneyness vs COR3M.R'

#### Purpose

This file constructs figure 6 from the thesis.

#### Inputs
- `Implied correlation per moneyness_M<Mat> - ATM.csv` from `Implied Correlation per Moneyness.R`
- `COR3M Data` in `2. Raw option input`

#### Outputs
- `Figure6_ImpliedATM_vs_COR3M_M<Mat>.png`

### 12. '6. Simulations / Simulations - Pworld as Black Scholes.R'

#### Purpose

This file supports the analysis from Chapter 5 in the thesis. . 

#### Inputs
- /

#### Outputs
- `Figure8_log2log10straddle2straddle10_evol.png`
- `Figure9_log2log10straddle2straddle10_scatter.png`
- `Figure10_Simulations_Analysis.png`

### 13. '8. Hedging real data / implied_volatility_slope_sticky_delta.R'

#### Purpose


Constructs the sticky-delta implied volatility slope measure by cleaning the option dataset, removing duplicate option observations, and computing implied volatility slopes across strike prices.
This file is intended to be sourced by other scripts and is not meant to be executed as a standalone.

#### Inputs
- A dataset called `data`
- A parameter `maturity`

#### Outputs
- A dataset called `df_iv_slope`

### 14. '8. Hedging real data / Cleaning Data and Synthesizing Option Type.R'

#### Purpose

This program applies the option dispersion trades discussed in Chapter 4 to the empirical dataset.
This program is the basis for all the figures and empirical results in Chapter 6.

#### Inputs
- The option data loaded using the `Loading Data.R`.
- The sticky deltas computed by `implied_volatility_slope_sticky_delta.R`.

#### Outputs
- `Option_dispersion_trade_<delta_type>_<greek_hedge>_<Maturity>.csv`

### 15. '8. Hedging real data / Cummulative Gains and Analysis - Gamma Strategy.R'

#### Purpose

This file provides insights in the results of the trading strategy on the correlation gap in accordance to what is written in the thesis in Chapter 6.2
Furthermore, it constructs figures 11, 12 and 13, and Tables 4 and 5.

#### Inputs
- The option data loaded using the `Loading Data.R`.
- `Option_dispersion_trade_<delta_type>_<greek_hedge>_<Maturity>.csv` from `Cleaning Data and Synthesizing Option Type.R`

#### Outputs
- `Figure11_PNL_cummulative_<greek_hedge>_<delta>_30_90.png`
- `Figure12_Scatter_Realized_daily_vs_<greek_hedge>_<delta>_PL.png`
- `Figure13_realmarket_analysis_<greek_hedge>_vega_corrected_<Mat>.png`

### 16. '8. Hedging real data / Cummulative Gains and Analysis - Vega Strategy.R'

#### Purpose

This file provides insights in the results of the trading strategy on the correlation risk premium in accordance to what is written in the thesis in Chapter 6.3.
Furthermore, it constructs figure 14 and Table 6.

#### Inputs
- The option data loaded using the `Loading Data.R`.
- `Option_dispersion_trade_<delta_type>_<greek_hedge>_<Maturity>.csv` from `Cleaning Data and Synthesizing Option Type.R`

#### Outputs
- `Figure14_Realmarketdata_analysis_<greek_hedge>.png`


