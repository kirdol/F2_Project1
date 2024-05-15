##--------------- Toursim St.Gallen-Austria -----------


#Load necessary packages
library("readxl")
library("dplyr")
library("ggplot2")
library("tsibble")
library("forecast")
library("ggseas")
library("fpp2")
library("timeSeries")
#library("modeltime")


# Set working directory
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


##--------------- Filter Tourism dataset -----------

# Filter the dataset for tourists going to St.Gallen from Austria
sg_data <- data %>%
  filter(Kanton == "St. Gallen") %>%
  filter(Herkunftsland == "Österreich") %>%
  filter(!is.na(value))

# View first lines of dataset
head(sg_data)

# Structure of the dataset
str(sg_data) #Value as numerical variable and Jahr as character

#Create Time series, include start month
ts_sg_data <- ts(sg_data$value, start = c(2005, 1), frequency = 12)
ts_sg_data

##--------------- Visualizations: Year plot -----------
# Aggregate data to have total number of visitors from Austria
year_sg <- sg_data %>%
  group_by(Jahr) %>%
  summarise(tot_tourists_sg_au = sum(value), .groups = 'drop')

# Create time series on yearly data
ts_sg_year_data <- ts(year_sg$tot_tourists_sg_au, start = 2005, frequency = 1)
ts_sg_year_data

# Plot to visualize year data
autoplot(ts_sg_year_data)+
  ggtitle("Number of tourists per year") +
  ylab("Number of tourists")

##--------------- Visualizations: Monthly plot -----------
# Plot monthly data
autoplot(ts_sg_data)+
  ggtitle("Number of tourists per month") +
  ylab("Number of tourists")
# We can see there is almost no trend, but huge seasonality

##--------------- Monthly trend and seasonality analysis (STL) -----------

#use STL decomposition to see the trend and seasonality more clearly
decomposition_sg <- stl(ts_sg_data, s.window = "periodic")
plot(decomposition_sg, main = "STL Decomposition of Tourism time series")

#We can see even better the strong seasonality component of the tourism that characterizes this data set
#This suggests an influence of the period of the year on the number of visitors in St-Gallen
#The trend can be seen as "stable" the only outlier is the COVID period (2020-21)
#Reminders seem white noise which is good

# Create graph seasonality
ggseasonplot(ts_sg_data) + 
  ggtitle("SG-AU seasonality analysis") +
  ylab("Number of Tourists")

# There is a strong seasonality as stated in the STL Decomposition
# COVID year is clearly visible

#Subseries plot
ggsubseriesplot(ts_sg_data) +
  ggtitle("Subseries analysis") +
  ylab("Number of Tourists")


## -------------- First try: No manage of data --------------------
arima_model <- auto.arima(ts_sg_data, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
summary(arima_model) #high RMSE and sigma

ets_model <- ets(ts_sg_data, model = "ZZZ")
summary(ets_model) #Higher AIC and BIC but smaller sigma, almost same RMSE

##--------------- Outliers analysis ---------------------------------
outliers_sg <- tsoutliers(ts_sg_data)
ts_sg_data_clean <- ts_sg_data
ts_sg_data_clean[outliers_sg$index] <- outliers_sg$replacements #replace outliers with values suggested

#See the results in a plot, comparing before and after replacement of outliers
plot(ts_sg_data, main = "SG-AU time series", ylab = "Number of visitors")
plot(ts_sg_data_clean, main = "SG-AU after outlier replacement", ylab = "Number of visitors")


##--------------- COVID-19 Dummy variable ---------------------------------
#Create dummy variable to manage the COVID-19 period
start_date <- as.Date("2020-03-01")
end_date <- as.Date("2021-04-30")
sg_data$sg_covid_dummy <- 1  # Initiate dummy variable at 1
sg_data$sg_covid_dummy[sg_data$Date >= start_date & sg_data$Date <= end_date] <- 0  # Specified COVID period

sg_covid <- ts(sg_data$sg_covid_dummy, start = c(2005, 1), frequency = 12)
sg_covid

#Implement dummy variable on time series
#ts_sg_data_covid <- cbind(ts_sg_data, sg_covid)
#ts_sg_data_covid

##--------------- First Forecast analysis: Naive, S-Naive (DA RIVEDERE il plot) -----------
#Create models to forecast
Mean_sg <- meanf(ts_sg_data)
summary(Mean_sg)
checkresiduals(Mean_sg)

Naive_sg <- naive(ts_sg_data)
summary(Naive_sg)

S_Naive_sg <- snaive(ts_sg_data)
summary(S_Naive_sg) 

# Plot graphs of forecasts
# 1. Mean
forecast_mean <- forecast(Mean_sg, h = 15)
plot(forecast_mean, main = "Forecast Naive", ylab = "Number of tourists")

# 2. Naive
forecast_naive <- forecast(Naive_sg, h = 15)
plot(forecast_naive, main = "Forecast Naive", ylab = "Number of tourists")

# 3. S-Naive
forecast_snaive <- forecast(S_Naive_sg, h = 15)
plot(forecast_snaive, main = "Forecast S-Naive", ylab = "Number of tourists")


# Plot the three models together to compare
# Plot first forecast
plot(forecast_main, main = "Comparison of Forecasts", ylab = "Number of tourists")

# Add second and third forecast
lines(forecast_naive$mean, col = "red")  # Add Naive forecast
lines(forecast_snaive$mean, col = "blue")  # Add S-Naive forecast
legend("topright", legend = c("Mean", "Naive", "S-Naive"), col = c("lightblue", "red", "blue"), lty = 1)

#Naive model takes more into consideration the level of last observation
#S-Naive takes into consideration the seasonality

#mean_model<- ts_sg_data %>% model(
  #Mean = mean(ts_sg_data),
  #Naive = naive(ts_sg_data),
  #S_Naive = snaive(ts_sg_data))




##--------------- Second Forecast analysis: ARIMA -----------
# Select best arima from auto.arima function
arima_sg_model <- auto.arima(ts_sg_data_clean, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)

#Summary of the ARIMA model created
summary(arima_sg_model)

#Forecast for next 15 months (Oct 2023 - Dec 2024)
forecast_arima_sg <- forecast(arima_sg_model, h = 15)
plot(forecast_arima_sg, main = "Forecast ARIMA")

##--------------- Third Forecast analysis: ETS -----------

#First try an SES (no trend and no seasonality)
ets_model_1 <- ets(ts_sg_data_clean, model = "ANN")
summary(ets_model_1)

# Compute forecast and plot the result
forecast_ets_1 <- forecast(ets_model_1, h = 15)  
plot(forecast_ets_1, main = "Forecast using ETS(A,N,N)", ylab = "Number of tourists")

# Try to see which ETS is better to use
ets_model_best <- ets(ts_sg_data_clean, model = "ZZZ")
summary(ets_model_best) #ETS(A,N,A) is suggested as actually there is no big trend but great seasonality (additive)

# Compute forecast of ETS (A,N,A) and plot the result
forecast_ets_best <- forecast(ets_model_best, h = 15)
plot(forecast_ets_best, main = "Forecast using ETS(A,N,A)", ylab = "Number of tourists")

##--------------- Comparisons: Accuracy tests -----------
accuracy(arima_sg_model)
accuracy(ets_model_best)
AIC(arima_sg_model) # 2620
BIC(arima_sg_model) # 2634
AIC(ets_model_best) # 3341
BIC(ets_model_best) # 3392

# cross-validation

# Question: ETS or ARIMA?? a little bit confused... advices?
# Looking at the RMSE (we want the lower) ETS(A,N,A) looks slightly better than the ARIMA model
accuracy(forecast_naive)
accuracy(forecast_snaive)
accuracy(forecast_mean)
# Interesting to see that looking at the results we have:
# Naive > S-Naive > Mean forecast
# Dummy variables?

# ------------- Fit ARIMA model with the dummy variable ---------------------
arima_model <- auto.arima(ts_sg_data, xreg = sg_covid) # Fit ARIMA model with the dummy variable as an exogenous regressor
summary(arima_model)

# Forecast for the next 15 months
forecast_arima_dummy <- forecast(arima_model, xreg = rep(1, 15)) # Make a forecast for the next 15 months using the fitted ARIMA model

# Print the forecast
print(forecast_arima_dummy) # Print the forecasted values

plot(forecast_arima_dummy, main = "Forecast of number of tourists using ARIMA model")
checkresiduals(forecast_arima_dummy)

# CROSS VALIDATION?!

# ------------- ETS with dummy variable --------------------------
# No direct support to insert an exogenous variable directly into the modeling
# Need to estimate effect of exo variable on the ts before fitting ETS model
# Estimate effect of the exogenous variable (here the dummy variable) on the time series using linear regression model
lm_model_covid <- lm(ts_sg_data ~ sg_covid)
summary(lm_model_covid)
sg_covid_effect <- predict(lm_model_covid)

# Subtract effect of the exogenous variable from the original time series
ts_sg_data_adjusted_covid <- ts_sg_data - sg_covid_effect

# Fit ETS model to the residual time series
ets_model_covid <- ets(ts_sg_data_adjusted_covid, model = "ZZZ")
summary(ets_model_covid) #ETS (A,N,A)

# Forecast for the next 15 months
forecast_ets_covid <- forecast(ets_model_covid, h = 15)

# Print the forecast
print(forecast_ets_covid)

# Plot the forecast
plot(forecast_ets_covid, main = "Forecast using ETS model with dummy variable")


# ---------------- Accuracy with dummy ------------------
accuracy(forecast_arima)
accuracy(forecast_ets_covid)
# better accuracy with dummy variable rather than outliers




# ---------------- Outliers and dummy ---------------
arima_model_outliers <- auto.arima(ts_sg_data_clean, xreg = sg_covid) # Fit ARIMA model with the dummy variable as an exogenous regressor
summary(arima_model_outliers)
forecast_arima_dummy_outliers <- forecast(arima_model_outliers, xreg = rep(1, 15)) # Make a forecast for the next 15 months using the fitted ARIMA model
plot(forecast_arima_dummy_outliers)

# --------------- ETS: high alpha and dummy -------
ets_model_alpha <- ets(ts_sg_data_adjusted_covid, model = "ZZZ", alpha = 0.9)
summary(ets_model_alpha) # No great results, RMSE still higher


#-------------- TSLM ----------------------------
# Compute correlation between number of visitors and covid
correlation <- cor(ts_sg_data, sg_covid)
print(correlation)

# Create TSLM model
tslm_model <- lm(ts_sg_data ~ sg_covid)
summary(tslm_model)
accuracy(tslm_model)

## include GDP? but only yearly data and not monthly available
## create new monthly dataset: join line and interpolate yearly
## Interaction GDP and covid beta 3
