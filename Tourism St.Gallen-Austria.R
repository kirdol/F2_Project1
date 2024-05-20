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


##--------------- Filter Tourism dataset ----------------
# Filter the dataset for tourists going to St.Gallen from Austria
# Explicitly use dplyr's filter function
sg_data <- data %>%
  dplyr::filter(Kanton == "St. Gallen") %>%
  dplyr::filter(Herkunftsland == "Österreich") %>%
  dplyr::filter(!is.na(value))


# View first lines of dataset
head(sg_data)

# Structure of the dataset
str(sg_data) #Value as numerical variable and Jahr as character

#Create Time series, include start month
ts_sg_data <- ts(sg_data$value, start = c(2005, 1), frequency = 12)
ts_sg_data

##--------------- Visualizations: Year plot ----------------
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
# Take into account that the sum of tourists for 2023 is only considering the first nine months

##--------------- Visualizations: Monthly plot ----------------------
# Plot monthly data
autoplot(ts_sg_data)+
  ggtitle("Number of tourists per month") +
  ylab("Number of tourists")
# We can see there is almost no trend, but huge seasonality

##---------- Monthly trend and seasonality analysis (STL, Subseries Plot...) -----------
#First see the autocorrelation plot to have more insights
acf(ts_sg_data, main = "Autocorrelation of monthly tourism in St.Gallen")
# Peaks around lag 12 indicates a yearly seasonality pattern

#use STL decomposition to see the trend and seasonality more clearly
decomposition_sg <- stl(ts_sg_data, s.window = "periodic")
plot(decomposition_sg, main = "STL Decomposition of Tourism time series")

#We can see even better the strong seasonality component of the tourism that characterizes this data set
#This suggests an influence of the period of the year on the number of visitors in St.Gallen
#The trend can be seen as "stable" the only "outlier" is the COVID period (2020-21)
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
# July-August-September as the peak months in terms of number of visitors from Austria
# Winter months prove to be the ones with the lowest number of tourists

## -------------- First try: No manage of data --------------------
# Mean forecast 
Mean_sg_no_manipulation <- meanf(ts_sg_data, h = 15)
summary(Mean_sg_no_manipulation)
plot(Mean_sg_no_manipulation, main = "Forecast Mean", ylab = "Number of tourists")
checkresiduals(Mean_sg_no_manipulation) # The ACF proves that there exists autocorrelation among the residuals (not good model fit)

Naive_sg_no_manipulation <- naive(ts_sg_data, h = 15)
summary(Naive_sg_no_manipulation)
plot(Naive_sg_no_manipulation, main = "Forecast Naive", ylab = "Number of tourists")
checkresiduals(Naive_sg_no_manipulation) # Slightly better but still same pattern

SNaive_sg_no_manipulation <- snaive(ts_sg_data, h = 15)
plot(SNaive_sg_no_manipulation, main = "Forecast S-Naive", ylab = "Number of tourists")
summary(SNaive_sg_no_manipulation)
checkresiduals(SNaive_sg_no_manipulation) # Even if it includes seasonality, not best result

arima_model <- auto.arima(ts_sg_data, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
summary(arima_model) # AIC = 2704.36, BIC = 2717.81 
forecast_arima_model <- forecast(arima_model, h = 15)
plot(forecast_arima_model, main = "ARIMA model without any manipulation")
checkresiduals(forecast_arima_model) # better result
accuracy(forecast_arima_model) # RMSE = 127.29

ets_model <- ets(ts_sg_data, model = "ZZZ")
summary(ets_model) #Higher AIC and BIC but smaller sigma, almost same RMSE
forecast_ets_model <- forecast(ets_model, h = 15)
plot(forecast_ets_model, main = "ETS model without any manipulation")
checkresiduals(ets_model) # Two autocorrelation values are outside the blue lines 
#(indicates significant autocorrelation of residual values which is not good even if probably negligible)

##--------------- Outliers analysis ---------------------------------
outliers_sg <- tsoutliers(ts_sg_data)
outliers_sg
ts_sg_data_clean <- ts_sg_data
ts_sg_data_clean[outliers_sg$index] <- outliers_sg$replacements #replace outliers with values suggested

#See the results in a plot, comparing before and after replacement of outliers
plot(ts_sg_data, main = "SG-AU time series", ylab = "Number of visitors")
plot(ts_sg_data_clean, main = "SG-AU after outlier replacement", ylab = "Number of visitors")

##--------------- Forecast analysis with outliers: Mean, Naive, S-Naive -----------
#Create models to forecast
Mean_sg <- meanf(ts_sg_data_clean, h=15)
summary(Mean_sg)

Naive_sg <- naive(ts_sg_data_clean, h = 15)
summary(Naive_sg)

S_Naive_sg <- snaive(ts_sg_data_clean, h = 15)
summary(S_Naive_sg) 

# Plot graphs of forecasts
# 1. Mean
forecast_mean <- Mean_sg
plot(forecast_mean, main = "Forecast Mean", ylab = "Number of tourists")
checkresiduals(forecast_mean)

# 2. Naive
forecast_naive <- Naive_sg
plot(forecast_naive, main = "Forecast Naive", ylab = "Number of tourists")
checkresiduals(forecast_naive)

# 3. S-Naive
forecast_snaive <- S_Naive_sg
plot(forecast_snaive, main = "Forecast S-Naive", ylab = "Number of tourists")
checkresiduals(forecast_snaive)

# Plot the three models together to compare
# Plot first forecast
plot(ts_sg_data, main = "Comparison of Forecasts", ylab = "Number of tourists")

# Add second and third forecast
lines(forecast_mean$mean, col = "lightblue")
lines(forecast_naive$mean, col = "red")  # Add Naive forecast
lines(forecast_snaive$mean, col = "blue")  # Add S-Naive forecast
legend("topleft", legend = c("Mean", "Naive", "S-Naive"), col = c("lightblue", "red", "blue"), lty = 1)

#Naive model takes more into consideration the level of last observation
#S-Naive takes into consideration the seasonality

##--------------- Second Forecast analysis: ARIMA -----------
# Select best arima from auto.arima function
arima_outliers_model <- auto.arima(ts_sg_data_clean, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)

#Summary of the ARIMA model created
summary(arima_outliers_model)

#Forecast for next 15 months (Oct 2023 - Dec 2024)
forecast_arima_outliers <- forecast(arima_outliers_model, h = 15)
plot(forecast_arima_outliers, main = "Forecast ARIMA")
checkresiduals(forecast_arima_outliers)

##--------------- Third Forecast analysis: ETS -----------

#First try an SES (no trend and no seasonality)
ets_model_1 <- ets(ts_sg_data_clean, model = "ANN")
summary(ets_model_1)

# Compute forecast and plot the result
forecast_ets_1 <- forecast(ets_model_1, h = 15)  
plot(forecast_ets_1, main = "Forecast using ETS(A,N,N)", ylab = "Number of tourists")

# Try to see which ETS is better to use
ets_model_outliers_best <- ets(ts_sg_data_clean, model = "ZZZ")
summary(ets_model_outliers_best) #ETS(A,N,A) is suggested as actually there is no big trend but great seasonality (additive)

# Compute forecast of ETS (A,N,A) and plot the result
forecast_ets_outliers <- forecast(ets_model_outliers_best, h = 15)
plot(forecast_ets_outliers, main = "Forecast using ETS(A,N,A)", ylab = "Number of tourists")

##--------------- Comparisons: Accuracy tests -----------
accuracy(arima_outliers_model) # RMSE = 105.1622
accuracy(ets_model_outliers_best) # RMSE = 104.669
AIC(arima_outliers_model) # 2620
BIC(arima_outliers_model) # 2634
AIC(ets_model_outliers_best) # 3341
BIC(ets_model_outliers_best) # 3392

accuracy(forecast_naive)
accuracy(forecast_snaive)
accuracy(forecast_mean)
# Interesting to see that looking at the results we have:
# Naive > S-Naive > Mean forecast

#Lower RMSE but outliers just reduce variance, better look another method

##--------------- COVID-19 Dummy variable ---------------------------------
#Create dummy variable to manage the COVID-19 period
start_date <- as.Date("2020-03-01")
end_date <- as.Date("2021-04-30") # The months were based on the time series 
sg_data$sg_covid_dummy <- 1  # Initiate dummy variable at 1
sg_data$sg_covid_dummy[sg_data$Date >= start_date & sg_data$Date <= end_date] <- 0  # Specified COVID period

sg_covid <- ts(sg_data$sg_covid_dummy, start = c(2005, 1), frequency = 12)
sg_covid

# ------------- Fit ARIMA model with the dummy variable ---------------------
arima_model_dummy <- auto.arima(ts_sg_data, xreg = sg_covid) # Fit ARIMA model with the dummy variable as an exogenous regressor
summary(arima_model_dummy) # RMSE = 110,9729
# Resulting model : ARIMA (2,0,0)(0,1,1)[12]

# Forecast for the next 15 months
forecast_arima_dummy <- forecast(arima_model_dummy, xreg = rep(1, 15)) # Make a forecast for the next 15 months using the fitted ARIMA model

# Print the forecast
print(forecast_arima_dummy) # Print the forecasted values

# Plot the forecast with the original data
plot(forecast_arima_dummy, main = "Forecast of number of tourists using ARIMA model")

# Adding a legend to the plot
legend("topleft", legend = c("Forecast", "Original Data"), 
       col = c("blue", "black"), lty = 1, bty = "n")

checkresiduals(forecast_arima_dummy)

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
summary(ets_model_covid) #ETS (A,N,A), RMSE = 111.3111 and a higher AIC and BIC compared to ARIMA model

# Forecast for the next 15 months
forecast_ets_covid <- forecast(ets_model_covid, h = 15)

# Print the forecast
print(forecast_ets_covid)

# Plot the forecast
plot(forecast_ets_covid, main = "Forecast using ETS model with dummy variable")
checkresiduals(forecast_ets_covid)
# Only one lag of the remainder is significant

# ---------------- Accuracy with dummy ------------------
accuracy(forecast_arima_dummy)
accuracy(forecast_ets_covid)
# better accuracy for ARIMA in terms of RMSE but also other values (ME, MAE...)


#-------------- Include other variables with linear regression: TSLM -----------------------
# Compute correlation between number of visitors and covid
correlation <- cor(ts_sg_data, sg_covid)
print(correlation)

# Create First TSLM model
tslm_model <- lm(ts_sg_data ~ sg_covid, data = ts_sg_data_adjusted_covid)
summary(tslm_model)

# Create future data for sg_covid (assuming no COVID restrictions for the future period)
future_sg_covid_only <- data.frame(sg_covid = rep(1, 15))

# Forecast for the next 15 months
forecast_tourists_covid <- forecast(tslm_model, newdata = future_sg_covid_only)
accuracy(forecast_tourists_covid) #RMSE really high = 188.2363


# Import dataset on GDP_CH
GDP_CH <- read_excel("GDP_CH_2029.xlsx", sheet = "Data") #This data is already forecasted until 2029
GDP_CH <- GDP_CH %>% slice(-(1:20)) #Take data from 2005 as the one of the tourism
GDP_CH <- GDP_CH %>%
  rename("Year" = "Gross domestic product (GDP) in Switzerland 2029" ) %>%
  rename("Value" = "...2")

head(GDP_CH)
str(GDP_CH)

# Create a new dataframe for monthly data
GDP_CH_monthly <- data.frame()

# Iterate over each consecutive pair of years in the GDP_CH dataframe
for (i in 1:(length(unique(GDP_CH$Year)) - 1)) {
  # Select the data for the current pair of years
  year_data_start <- subset(GDP_CH, Year == unique(GDP_CH$Year)[i])
  year_data_end <- subset(GDP_CH, Year == unique(GDP_CH$Year)[i + 1])
  
  # Extract GDP values for the start and end years
  start_value <- year_data_start$Value
  end_value <- year_data_end$Value
  
  # Calculate the monthly incremental change in GDP
  incremental_change <- (end_value - start_value) / 12
  
  # Initialize the monthly GDP value with the GDP value of the start year
  monthly_value <- start_value
  
  # Iterate over each month between the start and end years
  for (month in 1:12) {
    # Add the incremental change to the monthly GDP value
    monthly_value <- monthly_value + incremental_change
    
    # Create a dataframe row for the current month
    monthly_row <- data.frame(
      Year = unique(GDP_CH$Year)[i],
      Month = month,
      Value = monthly_value
    )
    
    # Add the row to the monthly dataframe
    GDP_CH_monthly <- rbind(GDP_CH_monthly, monthly_row)
  }
}

# Now GDP_CH_monthly contains the interpolated monthly data between consecutive years

# Transform GDP monthly data CH into a time series to then compute TSLM
ts_GDP_CH_monthly <- ts(GDP_CH_monthly$Value, start = c(2005, 1), frequency = 12)
ts_GDP_CH_monthly

# Truncate time series to match ts_sg_data
ts_GDP_CH_monthly_truncated <- window(ts_GDP_CH_monthly, end = c(2023, 9)) # Have the time series aligned in terms of time
ts_GDP_CH_monthly_truncated
# The idea: higher GDP if there are more visitors

# Create TSLM model including monthly Swiss GDP
tslm_model_GDP <- lm(ts_sg_data ~ sg_covid + ts_GDP_CH_monthly_truncated)
summary(tslm_model_GDP)
accuracy(tslm_model_GDP)

# Generate future values for the next 15 months (October 2023 to December 2024)
future_GDP <- window(ts_GDP_CH_monthly, start = c(2023, 10), end = c(2024, 12))
future_sg_covid <- rep(1, 15)  # Assuming no COVID restrictions for the future period

# Create a new dataframe for forecasting
future_data <- data.frame(
  sg_covid = future_sg_covid,
  ts_GDP_CH_monthly_truncated = as.numeric(future_GDP)
)

# Forecast for the next 15 months
forecast_tourists <- forecast(tslm_model_GDP, newdata = future_data)
forecast_tourists
accuracy(forecast_tourists) # we get a better RMSE = 181.0386

# Add a new variable of interaction between GDP and COVID pandemic as a beta 3 of the linear model
# Create a data frame with the necessary variables
ts_sg_CH_data_covid <- data.frame(
  ts_sg_data = as.numeric(ts_sg_data),
  sg_covid = as.numeric(sg_covid),
  ts_GDP_CH_monthly_truncated = as.numeric(ts_GDP_CH_monthly_truncated)
)


# Create interaction term monthly swiss GDP x COVID
ts_sg_CH_data_covid$interaction_term <- ts_sg_CH_data_covid$sg_covid * ts_sg_CH_data_covid$ts_GDP_CH_monthly_truncated
ts_sg_CH_data_covid

# Fit the TSLM model with the interaction term
tslm_model_interaction <- lm(ts_sg_data ~ sg_covid + ts_GDP_CH_monthly_truncated + interaction_term, data = ts_sg_CH_data_covid)
summary(tslm_model_interaction)

# Create a new data frame for forecasting
future_data_interaction <- data.frame(
  sg_covid = future_sg_covid,
  ts_GDP_CH_monthly_truncated = as.numeric(future_GDP)
)

# Create the interaction term for the future data
future_data_interaction$interaction_term <- future_data_interaction$sg_covid * future_data_interaction$ts_GDP_CH_monthly_truncated

# Forecast for the next 15 months
forecast_tourists_interaction <- forecast(tslm_model_interaction, newdata = future_data_interaction)
accuracy(forecast_tourists_interaction) #slightly better RMSE = 180.9881

## ------------ Conclusion: Choose ARIMA with dummy variable -----------------------------
# Considering COVID, a seasonal forecast and accuracy factors seems the best option
write.csv(forecast_arima_dummy, file = "forecast_results_part_2.csv")
