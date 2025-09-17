#run this code every time

#start
# 1. set working directory
setwd("C:/Users/q4q/OneDrive - Oak Ridge National Laboratory/Research/Bioavailability Project/Data")

# 2. read master spreadsheet
library(readxl)
Bioavailability_Master_Spreadsheet <- read_excel("Bioavailability Master Spreadsheet.xlsx", sheet = "Speciation Mode")

# 3. rename data and inspect it
dat<-(Bioavailability_Master_Spreadsheet)
str(dat)
dat <- as.data.frame(dat)
rownames(dat) <- (dat$Sample)

# 4. change categorical variables to characters (reorder when necessary), date to date
dat$Cluster <- as.factor(dat$Cluster)
dat$Cluster <- factor(dat$Cluster, levels = c("Main Stem", "PDF Tribs", "F Tribs", "NPP Tribs"))
dat$Date <- as.Date(dat$Date)
dat$Theme <- as.factor(dat$Theme)
dat$Regime <- as.factor(dat$Regime)
dat$Flow_state <- as.factor(dat$Flow_state)
dat$Type <- as.factor(dat$Type)
dat$Lithology <- as.factor(dat$Lithology)
dat$Development <- as.factor(dat$Development)
dat$Development <- factor(dat$Development, levels = c("High intensity", "Medium intensity", "Low intensity"))
dat$Distance_from_EFPC <- as.factor(dat$Distance_from_EFPC)
dat$Drainage_area <- as.factor(dat$Drainage_area)
dat$Drainage_area <- factor(dat$Drainage_area, levels = c("<1 km2", "1-10 km2", ">10 km2"))
str(dat)

# 5. log transform concentrations in columns 27-78 (added as new columns at end of data)
library(dplyr)
dat <- dat %>%
  mutate(across(27:73, ~ log10(.), .names = "log_{.col}"))
View(dat)
str(dat) #check

#end

#------------------------------------------
# 6. Filter for "Main Stem" cluster
dat_ms <- dat %>% filter(Cluster == "Main Stem")

# 7. Choose the variable to analyze (example: "log_Concentration_X")
# Replace "log_27" with the actual column name of your log-transformed variable
response_var <- dat_ms$log_TBL_Zn_nM_gw

# 8. Aggregate replicates by month (mean per month)
library(lubridate)
library(dplyr)

monthly_data <- dat_ms %>%
  group_by(month = floor_date(Date, "month")) %>%
  summarize(
    value_mean = mean(log_TBL_Zn_nM_gw, na.rm = TRUE),  # ❌ won't work as string
    value_sd   = sd(log_TBL_Zn_nM_gw, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(month)

# Inspect aggregated data
print(monthly_data)

#Autocorrelation plots

# Make sure you have your monthly time series
y <- monthly_data$value_mean

# 1. Plot autocorrelation function (ACF)
acf(y, main = "ACF of Monthly Mean Values", lag.max = 12)

# 2. Plot partial autocorrelation function (PACF)
pacf(y, main = "PACF of Monthly Mean Values", lag.max = 12)

#Model selection

# Make time series object from your monthly means
library(lubridate)
y_ts <- ts(monthly_data$value_mean, 
           start = c(year(min(monthly_data$month)), month(min(monthly_data$month))), 
           frequency = 12)

#Check stationarity
library(tseries)
adf.test(y_ts) #p-val>).05, series non-stationary

#Must transform it to becoming stationary
#Let's try detrending since the time-series gradually increases over time

# Suppose y_ts is your monthly time series
time_index <- 1:length(y_ts)

# Fit linear model for trend
trend_model <- lm(y_ts ~ time_index)

# Extract detrended series (residuals)
y_detrended <- resid(trend_model)

# Plot detrended series
plot(y_detrended, type = "l", main = "Detrended Series (Linear)", ylab = "Residuals")

# Check stationarity again
acf(y_detrended, main = "ACF of Detrended Series")
pacf(y_detrended, main = "PACF of Detrended Series")

adf.test(y_detrended) #p-val>).05, series non-stationary

#Let's try differencing since detrending didn't work

#first difference
y_diff1 <- diff(y_ts, differences = 1)
plot(y_diff1, type = "l", main = "First Differenced Series")
adf.test(y_diff1) #sill non-stationary

#seasonal difference with a 12-month lag is only possible if length is >24 months
length(y_ts) #11

#Since seasonal decomposition is not possible, we'll fit an ARIMA with d=1 (first difference)
library(forecast)

# Fit ARIMA automatically
best_model <- auto.arima(y_ts, d = 1, seasonal = FALSE, ic = "aic")
summary(best_model)
checkresiduals(best_model)
#model chosen: ARIMA(0,1,0) (random walk with drift)
#sigma^2 = 0.0437 → variance of the residuals (innovation noise)
#The residuals’ Ljung-Box test p-value = 0.51 → cannot reject null → residuals behave like white noise.
#model fits well

#Main takeaway:
#1. The series is dominated by random fluctuations; there’s no detectable AR or MA structure.
#2. Forecasts will basically continue the last observed value plus noise.
#3. Seasonal patterns cannot be modeled reliably with only 2 years of monthly data.























