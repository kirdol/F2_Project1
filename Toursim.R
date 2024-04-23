
## Package loading function:
## -- If package is not installed, install it
## -- Load the package
lib <- function(package) { if (!require(package, character.only = T)) install.packages(package); require(package, character.only = T) }

# Load necessary packages
lib("readxl")
lib("dplyr")
lib("ggplot2")
lib("tsibble")
lib("forecast")
lib("randomForest") 
lib("xgboost")       


# Set the working directory
wd <- dirname(rstudioapi::getSourceEditorContext()$path) 
setwd(wd)

# Load the dataset
data <- read_excel("Dataset_tourism.xlsx")

# Extract the year from a Date object
#data$Year <- year(data$Jahr)

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


# Plot monthly trend
ggplot(monthly_totals, aes(x = Date, y = total_visitors_monthly, group = 1)) +
  geom_line() +
  labs(title = "Monthly Visitors Trend", x = "Month", y = "Total Visitors") 


##--------------- Monthly Trend analysis -----------

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

#The p-value for the Date coefficient is very small (3.46e-05), 
#indicating a statistically significant relationship between time and 
#total visitors. The positive coefficient (7.701) suggests an overall 
#increasing trend in the number of visitors over time.

#he multiple R-squared value is 0.07416, which means about 7.4% of 
#the variability in the total number of visitors is explained by the model. 
#This is quite low, suggesting that there are other factors affecting the 
#number of visitors that the model does not account for.

# The residuals seem to have a wide range, which might suggest some variability 
#at the model isn't capturing. This could be due to seasonal trends.


##---------------Seasonality analysis-----------------
ts_data <- ts(monthly_totals$total_visitors_monthly, frequency = 12)  
decomposed <- stl(ts_data, s.window = "periodic")
plot(decomposed)
# The second panel shows a clear seasonal pattern that repeats yearly.

#The seasonality is quite pronounced and regular, suggesting that certain times
#of the year have a strong influence on the number of visitors.


#------- Check for outliers

#tsouliers: 
#index: This is a vector of integers indicating the positions in your time series where outliers have been detected.
#replacements: These are the suggested replacement values for the outliers, which can be used to clean your data if you choose to remove or adjust the outliers.


outliers <- tsoutliers(ts_data)

ts_data_clean <- ts_data
ts_data_clean[outliers$index] <- outliers$replacements

# Replace the outliers with the suggested values
ts_data_clean <- ts_data
ts_data_clean[outliers$index] <- outliers$replacements

#Re-plot the cleaned data
#The numbers on the x-axis correspond to the time steps of the data points in the series
plot(ts_data_clean, main="Cleaned Time Series Data", ylab="Total Visitors")

#####Before replacing outliers 
plot(ts_data, main="Time Series Data with Outliers")


#-----------Model selection : ARIMA--------------

#auto.arima() function automatically selects the best fit ARIMA model
sarima_model <- auto.arima(ts_data_clean, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)

# Summary of the model
summary(sarima_model)

# Forecasting future values
forecasts <- forecast(sarima_model, h = 15)  

# Plot the forecasts
plot(forecasts)

#-----------Model selection : Exponential Smoothing--------------

# 'ZZZ' allows the function to automatically select the error, trend, and seasonality types
fit_ets <- ets(ts_data_clean, model = "ZZZ")  

summary(fit_ets)

# Forecast the next 15 periods with the ETS model
ets_forecast <- forecast(fit_ets, h = 15)

# Plot the forecast
plot(ets_forecast)

#------------accuracy ---

#SARIMA model generally performs better than the ETS 
accuracy(sarima_model)
accuracy(fit_ets)


#--------------- TO CONTINUE 

# Step 2: Generate residuals
sarima_residuals <- residuals(sarima_model)
# Compute and plot ACF for residuals
acf(sarima_residuals)
# Compute and plot PACF for residuals
pacf(sarima_residuals)




