##--------------- Toursim St.Gallen-Austria -----------


#Load necessary packages
library("readxl")
library("dplyr")
library("ggplot2")
library("tsibble")
library("forecast")
library("ggseas")


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

##--------------- Visualizations: Year plot -----------
# Aggregate data to have total number of visitors from Austria
year_sg <- sg_data %>%
  group_by(Jahr) %>%
  summarise(tot_tourists_sg_au = sum(value), .groups = 'drop')

# Plot to visualize year data
ggplot(year_sg, aes(x = Jahr, y = tot_tourists_sg_au, group = 1 )) +
  geom_line()+
  labs(x = 'Year', y = 'Number of Tourists', title = 'Total visitors in St.Gallen from Austria by year')+
  theme_minimal()

##--------------- Visualizations: Monthly plot -----------
# Plot monthly data
ggplot(sg_data, aes(x = Date, y = value, group = 1)) +
  geom_line() +
  labs(x = 'Year', y = 'Number of Tourists', title = 'Total visitors in St.Gallen from Austria by month') +
  theme_minimal()

##--------------- Monthly trend and seasonality analysis (STL) -----------
#use STL decomposition to see the trend and seasonality more clearly
ts_sg_data <- ts(sg_data$value, frequency = 12)
decomposition_sg <- stl(ts_sg_data, s.window = "periodic")
plot(decomposition_sg, main = "STL Decomposition of Tourism time series")

#We can see even better the strong seasonality component of the tourism that characterizes this data set
#This suggests an influence of the period of the year on the number of visitors in St-Gallen
#The trend can be seen as "stable" the only outlier is the COVID period (2020-21)
#Reminders seem white noise which is good

#Seasonal view
seasonplot(ts_sg_data, main = "Seasonal Plot of Tourism Time Series")

##--------------- Outliers analysis ---------------------------------
outliers_sg <- tsoutliers(ts_sg_data)
ts_sg_data_clean <- ts_sg_data
ts_sg_data_clean[outliers_sg$index] <- outliers_sg$replacements #replace outliers with values suggested

#See the results in a plot, comparing before and after replacement of outliers
plot(ts_sg_data, main = "SG-AU time series", ylab = "Number of visitors")
plot(ts_sg_data_clean, main = "SG-AU after outlier replacement", ylab = "Number of visitors")

##--------------- First Forecast analysis: Naive, S-Naive (DA RIVEDERE il plot) -----------
#Create models to forecast
Mean_sg <- meanf(ts_sg_data_clean, h=15)
Naive_sg <- naive(ts_sg_data_clean, h=15)
S_Naive_sg <- snaive(ts_sg_data_clean, h= 15)

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

# Question: ETS or ARIMA?? a little bit confused... advices?
# Looking at the RMSE (we want the lower) ETS(A,N,A) looks slightly better than the ARIMA model
accuracy(forecast_naive)
accuracy(forecast_snaive)
accuracy(forecast_mean)
# Interesting to see that looking at the results we have:
# Naive > S-Naive > Mean forecast