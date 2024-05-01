
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
plot(monthly_totals)

#Decomposed the dataset 

ts_data <- ts(monthly_totals$total_visitors_monthly, start = c(2005, 1), end = c(2023, 9),frequency = 12)  
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


#------- Check for outliers
#tsouliers: 
#index: This is a vector of integers indicating the positions in your time series where outliers have been detected.
#replacements: These are the suggested replacement values for the outliers, which can be used to clean your data if you choose to remove or adjust the outliers.

outliers <- tsoutliers(ts_data)
ts_data[outliers$index] <- outliers$replacements

###--------------- Modeling -----------
#Fit Naive method
fit_naive <- naive(ts_data , h=15)
summary(ts_data) #Residual sd: 28476.6024 

## Fit SNaive method
fit_snaive <- snaive(ts_data , h=15)
summary(fit_naive) #Residual sd: Residual sd: 25572.8135


#fit ETS method
fit_ets <- ets(ts_data)#ETS(A,N,A), residual sd: 9667.688
summary(fit_ets)

#fit Arima : Best model: ARIMA(2,0,2)(0,1,1)[12] with drift   
fit_arima<- auto.arima(ts_data,trace = T, stepwise = FALSE, approximation = FALSE)
summary(fit_arima)#residual sd: 9117.477
  
#-------Forecast and Validation ---------

forecast_naive <- forecast(fit_naive, h = 15)
forecast_snaive <- forecast(fit_snaive, h = 15)
forecast_ets <- forecast(fit_ets, h=15,level=95)
forecast_arima <- forecast(fit_arima, h=15, level=95)

accuracy(forecast_naive )
accuracy(forecast_snaive)
accuracy(forecast_ets)
accuracy(forecast_arima)

AIC(fit_ets)# 5363.613
BIC(fit_ets)#5414.855

AIC(fit_arima)#4505.204
BIC(fit_arima)#4528.733

#----------------------weighted average 
# Define weights for each model (e.g., based on performance or domain knowledge)
weight_arima <- 0.9
weight_ets <- 0.1

# Combine forecasts using weighted average
combined_forecast_values <- (weight_arima * fit_arima$fitted) + (weight_ets * fit_ets$fitted)

# Create a forecast object with the combined forecast values
combined_forecast <- forecast(combined_forecast_values, h= 15)

# Plot the combined forecast
autoplot(combined_forecast)

print(summary(combined_forecast )) # residual sd:  6946.932, AIC : 5214.893, BIC: 5266.135


#Generally, you want to select the model with the lowest AIC and BIC values, as these criteria balance model fit and complexity.
#Additionally, a lower residual SD indicates a better fit of the model to the data. ---> ARIMA. 

#--------- Choose Arima---------------

# Plot the forecast with confidence intervals
autoplot(forecast_ets)+autoplot(ts_data)

