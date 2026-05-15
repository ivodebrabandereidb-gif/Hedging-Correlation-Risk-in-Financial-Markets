# Hedging-Correlation-Risk-in-Financial-Markets
This repository is made as a central location where the code can be found to recreate the results for the thesis 'Hedging Correlation Risk in Financial Markets', by Bram Verjans and Ivo De Brabandere


## General Workflow
### 1. '1. Package installer / Package Installer.R'
- inputs:\
- outputs: package

### 2. '3. Data loader / Loading Data.R'

#### Purpose
Loads and combines the raw option data, zero-coupon bond data, and portfolio weights for 2008–2010. 
This file is intended to be sourced by other scripts and is not executed standalone.

#### Inputs
- `optdata_<YYYY>_<M>.csv`
- `zerocd_<YYYY>.csv`
- `weights_<YYYY>.csv`

#### Outputs
- `data`
- `zcb`
- `weights`


### 3. '4. Model-free vs realized correlation / Realized Correlation.R'

#### Purpose

This file computes the realized correlation (RC) as is explained in section 2.3.1 of the thesis.

#### Inputs
- The weights given in `weights_<YYYY>.csv` in `2. Raw option data`.

#### Outputs
- `Realized_Correlation_M<Mat>.csv`
with `<Mat>` depicting the maturity specified.

### 4. '4. Model-free vs realized correlation / Model-Free Implied Correlation.R'

#### Purpose

This file computes the model-free implied correlation (MFIC) as is shown in section 2.3.2 of the thesis.

#### Inputs
- The option data loaded using the `Loading Data.R`.

#### Outputs
- `Model_Free_Implied_Correlation_M<Mat>.csv`
- `INTERMEZZO_heatmap_2010_12_M<Mat>.png`
with `<Mat>` depicting the maturity specified.

### 5. '4. Model-free vs realized correlation / Graphics Model-Free Implied vs Realized Correlation.R'

#### Purpose

This file constructs figure 5 from the thesis.

#### Inputs
- `Model_Free_Implied_Correlation_M<Mat>.csv` from `Model-Free Implied Correlation.R`
- `Realized_Correlation_M<Mat>.csv` from `Model-Free Implied Correlation.R`

#### Outputs
- `Figure5_ModelFree_Implied_vs_Realized_correlation.png`

### 6. '5. Implied correlation per moneyness / COR3M.R'

#### Purpose

This file constructs figure 3 from the thesis.

#### Inputs
- `COR3M Data` in `2. Raw option input`

#### Outputs
- `COR3M.png`

### 7. '5. Implied correlation per moneyness / Implied Correlation per Moneyness.R'

#### Purpose

This file computes the implied correlation over moneyness levels to support section 3.3.2 of the thesis.

#### Inputs
- The option data loaded using the `Loading Data.R`.

#### Outputs
- `Figure8_Implied_correlations_vs_moneyness_and_time_M<Mat>.png`
- `Implied correlation per moneyness_M<Mat> - ATM.csv`

### 8. '5. Implied correlation per moneyness / Graphics Implied per Moneyness vs Realized.R'

#### Purpose

This file constructs figure 6 from the thesis.

#### Inputs
- `Implied correlation per moneyness_M<Mat> - ATM.csv` from `Implied Correlation per Moneyness.R`
- `Realized_Correlation_M<Mat>.csv` from `Realized Correlation.R`

#### Outputs
- `Figure6_ATM_Implied_vs_realized_correlation_30_90`

### 9. '5. Implied correlation per moneyness / Graphics Implied Correlation per Moneyness vs COR3M.R'

#### Purpose

This file constructs figure 7 from the thesis.

#### Inputs
- `Implied correlation per moneyness_M<Mat> - ATM.csv` from `Implied Correlation per Moneyness.R`
- `COR3M Data` in `2. Raw option input`

#### Outputs
- `Figure7_ImpliedATM_vs_COR3M_M<Mat>.png`

### 10. '6. Simulations / Simulations - Pworld as Black Scholes.R'

#### Purpose

This file supports the analysis from Chapter 5 in the thesis. . 

#### Inputs
- /

#### Outputs
- `Figure9_log2log10straddle2straddle10_evol.png`
- `Figure10_log2log10straddle2straddle10_scatter.png`
- `Figure11_Simulations_Analysis.png`

### 11. '7. Hedging real data / implied_volatility_slope_sticky_delta.R'

#### Purpose


Constructs the sticky-delta implied volatility slope measure by cleaning the option dataset, removing duplicate option observations, and computing implied volatility slopes across strike prices.
This file is intended to be sourced by other scripts and is not executed standalone.

#### Inputs
- /

#### Outputs
- `Figure9_log2log10straddle2straddle10_evol.png`
- `Figure10_log2log10straddle2straddle10_scatter.png`
- `Figure11_Simulations_Analysis.png`

### 11. '7. Hedging real data / Cleaning Data and Synthesizing Option Type.R'

#### Purpose

Constructs the sticky-delta implied volatility slope measure by cleaning the option dataset, removing duplicate option observations, and computing implied volatility slopes across strike prices.
This file is intended to be sourced by other scripts and is not executed standalone.

#### Inputs
- The option data loaded using the `Loading Data.R`.
- The sticky deltas computed by `implied_volatility_slope_sticky_delta`.

#### Outputs
- `Option_dispersion_trade_<delta_type>_<greek_hedge>_<Maturity>`

### 11. '7. Hedging real data / Cummulative Gains and Analysis - Gamma Strategy.R'

#### Purpose

Constructs the sticky-delta implied volatility slope measure by cleaning the option dataset, removing duplicate option observations, and computing implied volatility slopes across strike prices.
This file is intended to be sourced by other scripts and is not executed standalone.

#### Inputs
- The option data loaded using the `Loading Data.R`.
- `Implied correlation per moneyness_M<Mat> - ATM.csv` from `Implied Correlation per Moneyness.R`

#### Outputs
- `Option_dispersion_trade_<delta_type>_<greek_hedge>_<Maturity>`
