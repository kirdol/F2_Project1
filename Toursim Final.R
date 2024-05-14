
## Package loading function:
## -- If package is not installed, install it
## -- Load the package
lib <- function(package) { if (!require(package, character.only = T)) install.packages(package); require(package, character.only = T) }

# Load necessary packages
lib("readxl")
lib("dplyr")
lib("forecast")
lib("fpp3")
lib("tseries")
lib("ggplot2")

# Set the working directory
wd <- dirname(rstudioapi::getSourceEditorContext()$path) 
setwd(wd)

# Load the dataset
data <- read_excel("Dataset_tourism.xlsx")

# German month names
german_months <- c("Januar", "Februar", "März", "April", "Mai", "Juni", 
                   "Juli", "August", "September", "Oktober", "November", "Dezember")
  
# English month abbreviations
english_months_abb <- month.abb

# Match German months to English abbreviations
data$Monat <- english_months_abb[match(data$Monat, german_months)]

# Combine Year and Month and convert to Date
data$Date <- as.Date(paste(data$Jahr, data$Monat, "1", sep="-"), "%Y-%B-%d")

# Filter data for the Canton of Vaud, if 'Vaud' is the correct name of the canton
vaud_data <- data %>%
  filter(Kanton == "Vaud") %>%
  filter(!is.na(value))

# View the first few rows of the filtered dataset
head(vaud_data)

# Check the structure of the filtered dataset
str(vaud_data)


###--------Visualization-------------


#-------Yearly data ------------------
# Aggregate data by year for total visitors
 Yearly_totals <- vaud_data %>%
  group_by(Jahr) %>%
  summarise(total_visitors_yearly= sum(value), .groups = 'drop')

# Plot
ggplot(Yearly_totals, aes(x = Jahr, y = total_visitors_yearly, group = 1)) +
  geom_line() +
  labs(title = "Total Visitors by Year", x = "Year", y = "Total Visitors")


#------- Monthly data -----------------------

# Aggregate data by Month for total visitors
monthly_totals <- vaud_data %>%
  group_by(Date) %>%
  summarise(total_visitors_monthly = sum(value), .groups = 'drop')

#Decomposed the dataset 

ts_data<- ts(monthly_totals$total_visitors_monthly, start = c(2005, 1), end = c(2023, 9),frequency = 12)  
decomposed <- stl(ts_data, s.window = "periodic")
plot(decomposed)


# Fit a linear model to observe the trend
trend_model_monthly <- lm(total_visitors_monthly ~ Date, data = monthly_totals)
summary(trend_model_monthly) 

# Add the trend line to the plot
ggplot(monthly_totals, aes(x = Date, y = total_visitors_monthly)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Trend in Monthly Visitors to Vaud",
       x = "Year and Month",
       y = "Number of Visitors") +
  theme_minimal()


##---------------Seasonality analysis-----------------
ggseasonplot(ts_data)+ggtitle("Seasonality Analysis")

#Strong Yearly Seasonality: Significant peaks at lags of 12 and 24 months 
#indicate a strong yearly pattern, likely due to seasonal factors affecting visits.

#Monthly Fluctuations: Other lags show smaller, less consistent correlations, 
#suggesting monthly visits can vary, but less predictably than the yearly pattern.

#Statistical Significance: The bars that extend beyond the blue dotted confidence lines at lags 12 and 24 are 
#statistically significant, confirming the reliability of the yearly pattern observed.
ggAcf(ts_data)+ggtitle("Autocorrelation of a monthly visits")


###------------------Cleaning and Wrangling
# Assign the original time series data (ts_data) to a new variable (ts_data_method_3)
# This might be used for applying Method 3 on the time series data
ts_data_method_3 <- ts_data


#-------------------------------------Method 1:  ts_outliers

# Detect outliers in the time series data
outliers <- tsoutliers(ts_data)

# Get the indices of outliers 
outlier_indices_to_replace <- outliers$index

# Replace outliers 
ts_data[outlier_indices_to_replace] <- outliers$replacements

#--------------------------------------Method 2: dummy variables
# Create a binary vector `r` with the same length as `ts_data`, filled with 1s.
r <- rep(1, length(ts_data))

# Set elements of `r` to 0 where the corresponding index of `ts_data` is greater than or equal to March 2020.
r[which(index(ts_data) >= "2020-Mar")] <- 0

# Update elements of `r` to 1 where the index of `ts_data` is greater than February 2021.
r[which(index(ts_data) > "2021-Feb")] <- 1
r


#--------------------------------------Method 3: tsouliers + forecasting


# Create a copy of ts_data with values from January 2015 to March-03 2020
data_2015_2020 <- window(ts_data_method_3, start = c(2015, 1), end = c(2020, 2))

# Fit ARIMA and ETS models to the 2015-2020 data and generate forecasts for 2020
fit_arima <- auto.arima(data_2015_2020)  # Automatic fitting of an ARIMA model to the data spanning 2015 to 2020.FEB.
fit_ets <- ets(data_2015_2020)  # Fitting an ETS (Exponential Smoothing State Space Model) to the same data.

forecast_2020_arima <- forecast(fit_arima, h = 12)  # Generating a forecast for 2020 using the ARIMA model.
forecast_2020_ets <- forecast(fit_ets, h = 12)  # Generating a forecast for 2020 using the ETS model.

AIC(fit_arima) #1041.518
BIC(fit_arima)#1045.342

AIC(fit_ets)#1367.589
BIC(fit_ets)#1403.751


summary(forecast_2020_arima)  # Summarizing the forecast results from the ARIMA model.
summary(forecast_2020_ets)  # Summarizing the forecast results from the ETS model.

# Extracting fitted values for 2020 from the forecast
fitted_values_2020 <- forecast_2020_arima$mean  # Extracting the mean fitted values for 2020 from the forecast object.
print(fitted_values_2020)  # Printing the extracted fitted values.


# Find the starting and ending indices for the range to be replaced
start_index <- 183
end_index <- 194

# Replace the values in the ts_data_method_3 time series from start_index to end_index with the fitted values for the year 2020.
# This is likely part of a process to update or correct the time series with specific fitted values.
ts_data_method_3[start_index:end_index] <- fitted_values_2020

# Display or print the updated time series data stored in ts_data_method_3.
ts_data_method_3


# Detect outliers in the time series data
outliers <- tsoutliers(ts_data_method_3)

# Get the indices of outliers 
outlier_indices_to_replace <- outliers$index

# Replace outliers 
ts_data_method_3[outlier_indices_to_replace] <- outliers$replacements



###--------------- Modeling -----------
#Fit Naive method
fit_naive <- naive(ts_data , h=15)
fit_naive_method3 <- naive(ts_data_method_3, h=15)
summary(fit_naive) #Residual sd: 28476.6024 
summary(fit_naive_method3) #28599.9411 

## Fit SNaive method
fit_snaive <- snaive(ts_data , h=15)
fit_snaive_method3 <- snaive(ts_data_method_3, h=15)
summary(fit_snaive) #Residual sd: Residual sd: 25572.8135---- 28074.0992 
summary(fit_snaive_method3) #16701.6654 



# Fit an ARIMA model to the time series data (ts_data) using the external regressors (xreg) stored in the variable r.
# The variable r contains dummy variables where 0 represents periods affected by COVID-19 (marked as outliers)
# and 1 represents non-COVID-19 periods.
# The auto.arima function automatically selects the best ARIMA model based on AIC/BIC criteria, incorporating the effects of the dummy variables.
arima_dummy <- auto.arima(ts_data, xreg = r)

# Forecast for the next 15 months
forecast_arima <- forecast(arima_dummy, xreg = rep(1, 15))

# Print the forecasted values, ARIMA(2,0,2)(0,1,1)[12] 
summary(forecast_arima) # residual s.d 9066.637


#---------------fit ETS method
fit_ets <- ets(ts_data)#ETS(A,N,A), residual sd: 9667.688
fit_ets_method_3 <- ets(ts_data_method_3)#ETS(M,Ad,M)  #residual sd 0.0441
summary(fit_ets)
summary(fit_ets_method_3) 


#fit Arima : Best model: ARIMA(2,0,2)(0,1,1)[12] with drift for fit_arima  , ARIMA(3,0,1)(0,1,1)[12] with drift for fit_arima_method_3
fit_arima<- auto.arima(ts_data,trace = T, stepwise = FALSE, approximation = FALSE)
fit_arima_method_3 <- auto.arima(ts_data_method_3,trace = T, stepwise = FALSE, approximation = FALSE)
summary(fit_arima)#residual sd: 9117.477
summary(fit_arima_method_3) #residual sd:8611.221
  
#-------Forecast and Validation ---------

forecast_naive <- forecast(fit_naive, h = 15)
forecast_snaive <- forecast(fit_snaive, h = 15)
forecast_ets <- forecast(fit_ets, h=15,level=95)
forecast_arima <- forecast(fit_arima, h=15, level=95)

forecast_naive_method_3<- forecast(fit_naive_method3, h = 15)
forecast_snaive_method_3 <- forecast(fit_snaive_method3, h = 15)
forecast_ets_method_3 <- forecast(fit_ets_method_3, h=15,level=95)
forecast_arima_method_3 <- forecast(fit_arima_method_3, h=15, level=95)

accuracy(forecast_naive )
accuracy(forecast_snaive)
accuracy(forecast_ets)
accuracy(forecast_arima)


accuracy(forecast_naive_method_3 )
accuracy(forecast_snaive_method_3)
accuracy(forecast_ets_method_3)
accuracy(forecast_arima_method_3)


AIC(fit_ets) #5363.613
AIC(fit_arima)#4505.204

BIC(fit_ets) #5414.855
BIC(fit_arima)#4528.733


AIC(fit_ets_method_3) # 5330.26
AIC(fit_arima_method_3) #4496.834

BIC(fit_ets_method_3) #5391.75
BIC(fit_arima_method_3) #4520.363



#--------- Choose Arima---------------


print(forecast_arima_method_3, level=95)

# Plot the forecast with confidence intervals
autoplot(forecast_arima_method_3)+ autolayer(ts_data)

