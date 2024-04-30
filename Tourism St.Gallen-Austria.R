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
