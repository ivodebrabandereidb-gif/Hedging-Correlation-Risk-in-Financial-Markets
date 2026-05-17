data = NULL
zcb = NULL

base_path <- dirname(getwd())

#------------------------- 
# Load option, zero-coupon yields and divisor data.
#-------------------------

for(year in years)
{
  year_folder <- file.path(base_path, "2. Raw option data", year)
  zcb_name <- file.path(year_folder,paste0("zerocd_",year,".csv"))

  for(month in months)
  {
    data_full_path <- file.path(year_folder, paste0("optdata_", year, "_", month, ".csv"))
    print(paste0("Loading data of: ", year,"-",month))
    data = bind_rows(data,as.data.frame(read.csv2(data_full_path, sep=",", header = T, dec = ".")))
  }
  zcb = bind_rows(zcb,as.data.frame(read.csv2(zcb_name, sep=",", header = T, dec = ".")))
}

#------------------------- 
# Load weights.
#-------------------------

#weights for all years are stored in each weights file, so we only need to load one:
weights_folder <- file.path(base_path, "2. Raw option data", year) # uses the last year (2010)
weights_full_path <- file.path(weights_folder, paste0("weights_", year, ".csv"))

print(paste("Loading weights from:", weights_full_path))
weights = read.csv2(weights_full_path, sep=",", header = T, dec = ".")

data <- data %>% mutate(quote_date = as.Date(quote_date, origin = "1899-12-30"))
weights <- weights %>% mutate(quote_date = as.Date(quote_date, origin = "1899-12-30"))
zcb <- zcb %>% mutate(quote_date = as.Date(quote_date, origin = "1899-12-30"))