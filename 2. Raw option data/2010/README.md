# Inputs folder

Insert in this folder the Dow Jones option data from 2010.

This folder should contain 14 files.

## Files containing option data per month
It is important to have 1 file per month named `optdata_YYYY_M.csv`.
Replace `YYYY` with the desired year and `M` with the desired month.
The csv should be comma separated and uses "." as decimal separator.
Each file should contain the following columns for all trading dates from a certain month for all relevant assets.  

| security_ID | quote_date | strike_price | expiration | best_bid | best_offer | volume | open_interest | implied_volatility | delta | close_price |
|---|---|---|---|---|---|---|---|---|---|---|
| Identifier for each security | Date as an Excel serial date number | The strike price | Expiration in days | The best bid price | The best offer | Volume | The open interest | The implied volatility | The delta | The close price |


## File containing the weights

It is important that the weights file bears the name "weights_<YYYY>.csv" and contains all the weights for years 2008 - 2010 for the Dow Jones Industrial Average.
The csv should be comma separated and uses "." as decimal separator.

| quote_date | security_ID | weights | close_price |
|---|---|---|---|
| Date as an Excel serial date number  | Identifier for each security | Daily weights for each security | The price of each asset at the end of the day |


## File containing the zero coupon bond returns

It is important that the zero coupon bond returns file bears the name "zerocd_<YYYY>.csv" and contains returns on a zero coupon bond over different maturities for the year 2010.
The csv should be comma separated and uses "." as decimal separator.

| quote_date | days | rate |
|---|---|---|
| Date as an Excel serial date number  | Maturity in days | Return on ZC bond |

