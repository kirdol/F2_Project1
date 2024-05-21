
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
lib("zoo")

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
  dplyr::filter(Kanton == "Vaud") %>%
  dplyr::filter(!is.na(value))

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

#make a copy of original ts_data
original_ts_data <- ts_data


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

# Convert the time index to a Date format
time_index <- as.Date(paste(floor(time(ts_data)), cycle(ts_data), "1", sep = "-"))

# Detect outliers in the time series data
outliers <- tsoutliers(ts_data)

# Get the indices and replacements of outliers
outlier_indices <- outliers$index
outlier_replacements <- outliers$replacements

# Find the index for March 2020 and February 2021
start_index_covid <- which(time_index == as.Date("2020-03-01")) #183
end_index_covid <- which(time_index == as.Date("2021-02-01"))   #194

# Identify outlier indices that fall within the COVID-19 range
covid_outliers <- outlier_indices[outlier_indices >= start_index_covid & outlier_indices <= end_index_covid]

# Replace the values at these indices with the proposed replacements
ts_data[covid_outliers] <- outlier_replacements[outlier_indices %in% covid_outliers]





#--------------------------------------Method 3:  forecasting covid 10


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



# Replace the values in the ts_data_method_3 time series from start_index to end_index with the fitted values for the year 2020.
# This is likely part of a process to update or correct the time series with specific fitted values.
ts_data_method_3[start_index_covid:end_index_covid] <- fitted_values_2020



###--------------- Modeling -----------
#Fit Naive method
fit_naive <- naive(ts_data , h=15)
fit_naive_method3 <- naive(ts_data_method_3, h=15)
summary(fit_naive) #Residual sd: 28743.8674 
summary(fit_naive_method3) #29711.8207  

## Fit SNaive method
fit_snaive <- snaive(ts_data , h=15)
fit_snaive_method3 <- snaive(ts_data_method_3, h=15)
summary(fit_snaive) #Residual sd: Residual sd: 29235.4113 
summary(fit_snaive_method3) # 25816.3693  


#---------------fit ETS method
fit_ets <- ets(ts_data)#ETS(A,N,A), residual sd:  11796.36
fit_ets_method_3 <- ets(ts_data_method_3)#ETS(M,A,M)   #residual sd 0.0644
summary(fit_ets)
summary(fit_ets_method_3) 


#fit Arima : Best model: ARIMA(1,0,1)(0,1,1)[12] for fit_arima  , ARIMA(1,0,3)(0,1,1)[12] for fit_arima_method_3
fit_arima<- auto.arima(ts_data,trace = T, stepwise = FALSE, approximation = FALSE) #ARIMA(1,0,1)(0,1,1)[12]
fit_arima_method_3 <- auto.arima(ts_data_method_3,trace = T, stepwise = FALSE, approximation = FALSE) #ARIMA(1,0,3)(0,1,1)[12] with drift
summary(fit_arima)#residual sd^2: 141195342
summary(fit_arima_method_3) #residual sd^2:180948346
  
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


AIC(fit_ets) #5453.164
AIC(fit_arima)#4616.894

BIC(fit_ets) #5504.405
BIC(fit_arima)# 4630.339


AIC(fit_ets_method_3) #5499.355
AIC(fit_arima_method_3) #4674.697

BIC(fit_ets_method_3) #5557.428
BIC(fit_arima_method_3) #4698.226



#--------- Choose forecast Arima---------------

# Load the openxlsx package

print(forecast_arima, level=95)
# Plot the forecast with confidence intervals
plot(forecast_arima,include = 95)+ autolayer(original_ts_data)+theme_minimal()



# Add legend outside the plot area with a smaller box
legend("topleft", legend = c("Forecast", "95% Confidence Interval", "Original Data"), col = c("blue", "gray", "black"), lty = c(1, 1, 1), bty = "o", xpd = TRUE, cex = 0.6)

write.csv(forecast_arima, file = "forecast_results_part_1.csv")
