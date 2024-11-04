install.packages("ggplot2")
install.packages("lmtest")
install.packages("sandwich")
install.packages("corrplot")
install.packages("reshape2")
install.packages("car")
install.packages("psych")
library(car)
library(corrplot)
library(lmtest)
library(sandwich)
library(dplyr)
library(ggplot2)
library(reshape2)
library(psych)


# Read Data (change the path for you)
data <- read.csv("C:\\Users\\Trella\\Downloads\\Data_Gummybears_2.csv")
head(data)

sorted_data <- data[order(data$Time), ]
head(sorted_data)

data <- sorted_data

#descriptive statistics
describe(data)

# Check correlation of Price with Sales & Apply stepwise selection 
cor(data$Sales..units., data$Price....k.)
stepwise_model <- step(model_ov, direction = "both")

# Check correlation of Time factors with Wage
cor(data$Time, data$Wage..Perc..)
cor(data$Year, data$Wage..Perc..)

#  Correlation Analysis
cor_matrix <- cor(data[, c('Sales..units.', 'Ad1..GRP.', 'Ad2..No_of_banners.', 'Prom..No_of_stores.')])
corrplot(cor_matrix, method = "circle")

ggplot(data, aes(x=Time, y=Sales..units.)) + geom_line() + geom_point() +
  ggtitle("Monthly Sales Trend") + xlab("Month Number") + ylab("Sales Units")

# Boxplots for Sales and log(sales)
boxplot(data$Sales..units., main="Sales", ylab="Sales")
boxplot(log(data$Sales..units.), main="Sales (Transformed)", ylab="log(Sales)")

# View the summary of the stepwise model
summary(stepwise_model)
model_ov <- lm(log(Sales..units.) ~ Ad1..GRP. + Ad2..No_of_banners. + 
                 Prom..No_of_stores. + Wage..Perc.. + Region + Month, data = data)


# Model (1) 

# Creating a dummy Month
data$Month <- factor(data$Month, levels = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
data$Month <- relevel(data$Month, ref = 'Apr')

# Creating adummy Region
data$Region <- factor(data$Region, levels = c('UK','Singapore','Spain','US','Mexico'))
data$Region <- relevel(data$Region, ref = 'UK')

#fitting the model
model_ov <- lm(formula = log(Sales..units.) ~  Ad1..GRP. + Ad2..No_of_banners. + 
                  +        Prom..No_of_stores.  + Wage..Perc..  + Region + Month , data = data)
summary(model_ov)

# Model (2)

#creating dummies for each of the marketing activities
data$Ad1_dummy <- ifelse(data$Ad1..GRP. == 0, 0, 1)
data$Ad2_dummy <- ifelse(data$Ad2..No_of_banners. == 0, 0, 1)
data$Prom_dummy <- ifelse(data$Prom..No_of_stores. == 0, 0, 1)

#fitting the model
model_ov_dummy <- lm(formula = log(Sales..units.) ~  Ad1_dummy + Ad2_dummy + 
                       +        Prom_dummy  + Wage..Perc..  + Region + Month , data = data)
summary(model_ov_dummy)

# VIF of model (1) and (2)
vif_values_1 <- car::vif(model_ov)
print(vif_values_1)
vif_values_2 <- car::vif(model_ov_dummy)
print(vif_values_2)

# Create QQ plot for Model (1) and (2)
qqnorm(residuals(model_ov))
qqline(residuals(model_ov), col = "red")

qqnorm(residuals(model_ov_dummy))
qqline(residuals(model_ov_dummy), col = "red")

# homoscedasticity check
# Residuals VS Fitted Plot for Model (1)
plot_data <- data.frame(fitted = fitted(model_ov), residuals = residuals(model_ov))
ggplot(plot_data, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted Values") +
  ylab("Residuals") +
  ggtitle("Residuals vs Fitted Plot")

# Residuals vs Fitted Plot for Model (2)
plot_data_d <- data.frame(fitted = fitted(model_ov_dummy), residuals = residuals(model_ov_dummy))
ggplot(plot_data_d, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted Values") +
  ylab("Residuals") +
  ggtitle("Residuals vs Fitted Plot")


# Split Data into the respective regions ####
usa_data <- subset(data, Region == 'US')
uk_data <- subset(data, Region == 'UK')
mexico_data <- subset(data, Region == 'Mexico')
singapore_data <- subset(data, Region == 'Singapore')
spain_data <- subset(data, Region == 'Spain')

# Models 1.1 - 1.5 

# Regression on USA data
model_usa <- lm(formula = log(Sales..units.) ~ Ad1..GRP. + Ad2..No_of_banners. + 
                  +        Prom..No_of_stores. + Month + Wage..Perc.., data = usa_data)
summary(model_usa)


# Regression on UK data ####
model_uk <- lm(formula = log(Sales..units.) ~ Ad1..GRP. + Ad2..No_of_banners. + 
                 +        Prom..No_of_stores. + Month + Wage..Perc.., data = uk_data)
summary(model_uk)

# Regression on Mexico data ####
model_mexico <- lm(formula = log(Sales..units.) ~ Ad1..GRP. + Ad2..No_of_banners. + 
                     +        Prom..No_of_stores. + Month + Wage..Perc.., data = mexico_data)
summary(model_mexico)

# Regression on Singapore data ####
model_singapore <- lm(formula = log(Sales..units.) ~ Ad1..GRP. + Ad2..No_of_banners. + 
                        +        Prom..No_of_stores. + Month + Wage..Perc.., data = singapore_data)
summary(model_singapore)

# Regression on Spain data ####
model_spain <- lm(formula = log(Sales..units.) ~ Ad1..GRP. + Ad2..No_of_banners. + 
                    +        Prom..No_of_stores. + Month + Wage..Perc.., data = spain_data)
summary(model_spain)


# Question 1

# Calculation for UK
uk_ad1 <- (exp(0.880378+0.199621)-exp(0.880378))*12
uk_ad1

uk_ad2 <- (exp(0.880378+1.284701)-exp(0.880378))*12
uk_ad2

uk_prom <- (exp(0.880378+0.226566)-exp(0.880378))*12
uk_prom
 
#Calculation for Singapore
data$Region <- factor(data$Region, levels = c('UK','Singapore','Spain','US','Mexico'))
data$Region <- relevel(data$Region, ref = 'Singapore')

model_ov_dummy <- lm(formula = log(Sales..units.) ~  Ad1_dummy + Ad2_dummy + 
                       +        Prom_dummy  + Wage..Perc..  + Region + Month , data = data)
summary(model_ov_dummy)

sin_ad1 <- (exp(0.126428+0.199621)-exp(0.126428))*12
sin_ad1

sin_ad2 <- (exp(0.126428+1.284701)-exp(0.126428))*12
sin_ad2

sin_prom <- (exp(0.126428+0.226566)-exp(0.126428))*12
sin_prom

#Calculation for Spain
data$Region <- factor(data$Region, levels = c('UK','Singapore','Spain','US','Mexico'))
data$Region <- relevel(data$Region, ref = 'Spain')

model_ov_dummy <- lm(formula = log(Sales..units.) ~  Ad1_dummy + Ad2_dummy + 
                       +        Prom_dummy  + Wage..Perc..  + Region + Month , data = data)
summary(model_ov_dummy)

sp_ad1 <- (exp(0.914124+0.199621)-exp(0.914124))*12
sp_ad1

sp_ad2 <- (exp(0.914124+1.284701)-exp(0.914124))*12
sp_ad2

sp_prom <- (exp(0.914124+0.226566)-exp(0.914124))*12
sp_prom

#Calculation for US
data$Region <- factor(data$Region, levels = c('UK','Singapore','Spain','US','Mexico'))
data$Region <- relevel(data$Region, ref = 'US')

model_ov_dummy <- lm(formula = log(Sales..units.) ~  Ad1_dummy + Ad2_dummy + 
                       +        Prom_dummy  + Wage..Perc..  + Region + Month , data = data)
summary(model_ov_dummy)

us_ad1 <- (exp(1.965318+0.199621)-exp(1.965318))*12
us_ad1

us_ad2 <- (exp(1.965318+1.284701)-exp(1.965318))*12
us_ad2

us_prom <- (exp(1.965318+0.226566)-exp(1.965318))*12
us_prom

#Calculation for Mexico
data$Region <- factor(data$Region, levels = c('UK','Singapore','Spain','US','Mexico'))
data$Region <- relevel(data$Region, ref = 'Mexico')

model_ov_dummy <- lm(formula = log(Sales..units.) ~  Ad1_dummy + Ad2_dummy + 
                       +        Prom_dummy  + Wage..Perc..  + Region + Month , data = data)
summary(model_ov_dummy)

mex_ad1 <- (exp(1.437518+0.199621)-exp(1.437518))*12
mex_ad1

mex_ad2 <- (exp(1.437518+1.284701)-exp(1.437518))*12
mex_ad2

mex_prom <- (exp(1.437518+0.226566)-exp(1.437518))*12
mex_prom

total_ad1 = uk_ad1 + us_ad1 + sp_ad1 + mex_ad1 + sin_ad1
total_ad2 = uk_ad2 + us_ad2 + sp_ad2 + mex_ad2 + sin_ad2
total_ad3 = uk_prom + us_prom + sp_prom + mex_prom + sin_prom

# Question 2

# Profit from each unit sold is 30%
# Function to calculate weighted average for price
weighted_average_forecast <- function(data_vector) {
  weights <- seq_along(data_vector)
  sum_of_products <- sum(data_vector * weights)
  sum_of_weights <- sum(weights)
  return(sum_of_products / sum_of_weights)
}

# Weighted Average of Price
weighted_average_price_ov <- weighted_average_forecast(data$Price....k.)

avg_price=weighted_average_price_ov*1000
avg_price

#profit per unit
profit_unit=avg_price*0.3
profit_unit

#Profit for AD1
prof_AD1=profit_unit*(uk_ad1+sin_ad1+us_ad1+mex_ad1+sp_ad1)
prof_AD1
#ROI AD1 - Cost is 2000,000
roi_ad1=prof_AD1/2000000*100
roi_ad1=(prof_AD1/2000000)*100
roi_ad1

roi_ad1=(prof_AD1-2000000)/2000000*100
pr=(avg_price*(uk_ad1+sin_ad1+us_ad1+mex_ad1+sp_ad1)-2000000)/2000000

#Profit for AD2
proft_AD2=profit_unit*(uk_ad2+sin_ad2+us_ad2+mex_ad2+sp_ad2)
proft_AD2

#ROI for AD2 - Cost is 500,000
roi_ad2=(proft_AD2/500000)*100
roi_ad2


# Attempt to implement time series analysis for the Q4


require(forecast)
data$Sales..units.
Sales_train <- ts(data$Sales..units.[1:288],frequency=12,start=c(2010,1))
Sales_test <- ts(data$Sales..units.[289:300],frequency=12,start=c(2014,1))
vals <- stl(log((Sales_train)),s.window = 12) 
plot(vals, main="Sales")

vals$time.series[,"seasonal"]
vals$time.series[,"trend"]

head(vals$time.series)
vals$time.series[,1]

vals <- stl(ts, s.window = 12)
trend_component <- vals$time.series[, "trend"]
plot(trend_component, type = "l", main = "Trend Component for  Sales")

# seasonal pattern that can be extracted by using the log transformation:
length(data$Sales..units.)
vals <- stl(log(Sales_train),s.window = 12) #s.window is the the number of time units were the cycle repeats. 
plot(vals)

#model of the cycle of the time series
cycle <- colMeans(matrix(ncol=12,byrow=TRUE,data = vals$time.series[,"seasonal"]) )
plot(cycle,type="b")

#trend
trend <- vals$time.series[,"trend"]
tindex <- 1:length(trend)
trend_model <- lm(trend ~ tindex)
summary(trend_model)

#remainder
remainder_component <- ts - trend_component - seasonal_component

# Plot the remainder component
plot(remainder_component, type = "l", main = "Remainder Component for Sales", xlab = "Date", ylab = "Remainder")

# Plot the remainder component
plot(vals$time.series[, "remainder"], type = "l", main = "Remainder Component for  Sales", xlab = "Date", ylab = "Remainder")

library(aTSA)
#Dickey-Fuller Test for stationarity
Rt <- vals$time.series[,"remainder"] 
aTSA::adf.test(Rt)

#ACF and PACF
acf(x = Rt) 
pacf(x = Rt)

auto.arima(Rt)
arimafit <- arima(x = Rt, order = c(0,0,0),include.mean = FALSE) 
arimafit

# Forecasting using linear regressions instead

# Sales by Region & Year ####
regions <- unique(data$Region)

# Initialize an empty named vector to store the sales data
sales_data <- numeric(0)

# Loop through the years 2010 to 2014
for(year in 2010:2014) {
  # Loop through each region
  for(region in regions) {
    # Calculate the sum of sales for each January of the specified year and region
    sales_sum <- sum(data$Sales..units.[data$Month == "Jan" & data$Year == year & data$Region == region])
    
    # Create a unique name for this year-region combination
    name <- paste(year, region, sep="_")
    
    # Add the sales sum to the vector with the unique name
    sales_data[name] <- sales_sum
  }
}

# Ad1 by Region & Year ####
# Initialize an empty named vector to store the ad1 data
ad1_data <- numeric(0)

# Loop through the years 2010 to 2014
for(year in 2010:2014) {
  # Loop through each region
  for(region in regions) {
    # Calculate the sum of sales for each January of the specified year and region
    ad1_sum <- sum(data$Ad1..GRP.[data$Month == "Jan" & data$Year == year & data$Region == region])
    
    # Create a unique name for this year-region combination
    name <- paste(year, region, sep="_")
    
    # Add the sales sum to the vector with the unique name
    ad1_data[name] <- ad1_sum
  }
}

# Ad2 by Region & Year ####
# Initialize an empty named vector to store the sales data
ad2_data <- numeric(0)

# Loop through the years 2010 to 2014
for(year in 2010:2014) {
  # Loop through each region
  for(region in regions) {
    # Calculate the sum of sales for each January of the specified year and region
    ad2_sum <- sum(data$Ad2..No_of_banners.[data$Month == "Jan" & data$Year == year & data$Region == region])
    
    # Create a unique name for this year-region combination
    name <- paste(year, region, sep="_")
    
    # Add the sales sum to the vector with the unique name
    ad2_data[name] <- ad2_sum
  }
}

# Ad3 by Region & Year ####
# Initialize an empty named vector to store the sales data
ad3_data <- numeric(0)

# Loop through the years 2010 to 2014
for(year in 2010:2014) {
  # Loop through each region
  for(region in regions) {
    # Calculate the sum of sales for each January of the specified year and region
    ad3_sum <- sum(data$Prom..No_of_stores.[data$Month == "Jan" & data$Year == year & data$Region == region])
    
    # Create a unique name for this year-region combination
    name <- paste(year, region, sep="_")
    
    # Add the sales sum to the vector with the unique name
    ad3_data[name] <- ad3_sum
  }
}

# Wages by Region & Year ####
# Initialize an empty named vector to store the sales data
wage_data <- numeric(0)

# Loop through the years 2010 to 2014
for(year in 2010:2014) {
  # Loop through each region
  for(region in regions) {
    # Calculate the sum of sales for each January of the specified year and region
    wage_sum <- sum(data$Wage..Perc..[data$Month == "Jan" & data$Year == year & data$Region == region])
    
    # Create a unique name for this year-region combination
    name <- paste(year, region, sep="_")
    
    # Add the sales sum to the vector with the unique name
    wage_data[name] <- wage_sum
  }
}

# Function to calculate weighted average forecast
weighted_average_forecast <- function(data_vector) {
  weights <- seq_along(data_vector)
  sum_of_products <- sum(data_vector * weights)
  sum_of_weights <- sum(weights)
  return(sum_of_products / sum_of_weights)
}

# Applying the function to different datasets for US ####
sales_jan_us <- c(sales_data["2010_US"], sales_data["2011_US"], sales_data["2012_US"], sales_data["2013_US"], sales_data["2014_US"])
weighted_average_sales_us <- weighted_average_forecast(sales_jan_us)

ad1_jan_us <- c(ad1_data["2010_US"], ad1_data["2011_US"], ad1_data["2012_US"], ad1_data["2013_US"], ad1_data["2014_US"])
weighted_average_ad1_us <- weighted_average_forecast(ad1_jan_us)

ad2_jan_us <- c(ad2_data["2010_US"], ad2_data["2011_US"], ad2_data["2012_US"], ad2_data["2013_US"], ad2_data["2014_US"])
weighted_average_ad2_us <- weighted_average_forecast(ad2_jan_us)

ad3_jan_us <- c(ad3_data["2010_US"], ad3_data["2011_US"], ad3_data["2012_US"], ad3_data["2013_US"], ad3_data["2014_US"])
weighted_average_ad3_us <- weighted_average_forecast(ad3_jan_us)

wage_jan_us <- c(wage_data["2010_US"], wage_data["2011_US"], wage_data["2012_US"], wage_data["2013_US"], wage_data["2014_US"])
weighted_average_wage_us <- weighted_average_forecast(wage_jan_us)

# Create data frame for Jan 2015 for US
jan_2015_data_US <- data.frame(
  Ad1..GRP. = weighted_average_ad1_us,
  Ad2..No_of_banners. = weighted_average_ad2_us,
  Prom..No_of_stores. = weighted_average_ad3_us,
  Month = 'Jan',
  Wage..Perc.. = weighted_average_wage_us*100
)

# Forecast sales for January 2015
jan_2015_forecast_us <- predict(model_usa, newdata = jan_2015_data_US)

# Output the forecast
print(jan_2015_forecast_us)

# Applying the function to different datasets for UK region ####
sales_jan_uk <- c(sales_data["2010_UK"], sales_data["2011_UK"], sales_data["2012_UK"], sales_data["2013_UK"], sales_data["2014_UK"])
weighted_average_sales_uk <- weighted_average_forecast(sales_jan_uk)

ad1_jan_uk <- c(ad1_data["2010_UK"], ad1_data["2011_UK"], ad1_data["2012_UK"], ad1_data["2013_UK"], ad1_data["2014_UK"])
weighted_average_ad1_uk <- weighted_average_forecast(ad1_jan_uk)

ad2_jan_uk <- c(ad2_data["2010_UK"], ad2_data["2011_UK"], ad2_data["2012_UK"], ad2_data["2013_UK"], ad2_data["2014_UK"])
weighted_average_ad2_uk <- weighted_average_forecast(ad2_jan_uk)

ad3_jan_uk <- c(ad3_data["2010_UK"], ad3_data["2011_UK"], ad3_data["2012_UK"], ad3_data["2013_UK"], ad3_data["2014_UK"])
weighted_average_ad3_uk <- weighted_average_forecast(ad3_jan_uk)

wage_jan_uk <- c(wage_data["2010_UK"], wage_data["2011_UK"], wage_data["2012_UK"], wage_data["2013_UK"], wage_data["2014_UK"])
weighted_average_wage_uk <- weighted_average_forecast(wage_jan_uk)

# Create data frame for Jan 2015
jan_2015_data_UK <- data.frame(
  Ad1..GRP. = weighted_average_ad1_uk,
  Ad2..No_of_banners. = weighted_average_ad2_uk,
  Prom..No_of_stores. = weighted_average_ad3_uk,
  Month = 'Jan',
  Wage..Perc.. = weighted_average_wage_uk*100
)

# Forecast sales for January 2015
jan_2015_forecast_uk <- predict(model_uk, newdata = jan_2015_data_UK)

# Output the forecast
print(jan_2015_forecast_uk)

# Applying the function to different datasets for Mexico region
sales_jan_mexico <- c(sales_data["2010_Mexico"], sales_data["2011_Mexico"], sales_data["2012_Mexico"], sales_data["2013_Mexico"], sales_data["2014_Mexico"])
weighted_average_sales_mexico <- weighted_average_forecast(sales_jan_mexico)

ad1_jan_mexico <- c(ad1_data["2010_Mexico"], ad1_data["2011_Mexico"], ad1_data["2012_Mexico"], ad1_data["2013_Mexico"], ad1_data["2014_Mexico"])
weighted_average_ad1_mexico <- weighted_average_forecast(ad1_jan_mexico)

ad2_jan_mexico <- c(ad2_data["2010_Mexico"], ad2_data["2011_Mexico"], ad2_data["2012_Mexico"], ad2_data["2013_Mexico"], ad2_data["2014_Mexico"])
weighted_average_ad2_mexico <- weighted_average_forecast(ad2_jan_mexico)

ad3_jan_mexico <- c(ad3_data["2010_Mexico"], ad3_data["2011_Mexico"], ad3_data["2012_Mexico"], ad3_data["2013_Mexico"], ad3_data["2014_Mexico"])
weighted_average_ad3_mexico <- weighted_average_forecast(ad3_jan_mexico)

wage_jan_mexico <- c(wage_data["2010_Mexico"], wage_data["2011_Mexico"], wage_data["2012_Mexico"], wage_data["2013_Mexico"], wage_data["2014_Mexico"])
weighted_average_wage_mexico <- weighted_average_forecast(wage_jan_mexico)

# Create data frame for Jan 2015
jan_2015_data_Mexico <- data.frame(
  Ad1..GRP. = weighted_average_ad1_mexico,
  Ad2..No_of_banners. = weighted_average_ad2_mexico,
  Prom..No_of_stores. = weighted_average_ad3_mexico,
  Month = 'Jan',
  Wage..Perc.. = weighted_average_wage_mexico*100
)

# Forecast sales for January 2015
jan_2015_forecast_mexico <- predict(model_mexico, newdata = jan_2015_data_Mexico)

# Output the forecast
print(jan_2015_forecast_mexico)

# Applying the function to different datasets for Spain region
sales_jan_spain <- c(sales_data["2010_Spain"], sales_data["2011_Spain"], sales_data["2012_Spain"], sales_data["2013_Spain"], sales_data["2014_Spain"])
weighted_average_sales_spain <- weighted_average_forecast(sales_jan_spain)

ad1_jan_spain <- c(ad1_data["2010_Spain"], ad1_data["2011_Spain"], ad1_data["2012_Spain"], ad1_data["2013_Spain"], ad1_data["2014_Spain"])
weighted_average_ad1_spain <- weighted_average_forecast(ad1_jan_spain)

ad2_jan_spain <- c(ad2_data["2010_Spain"], ad2_data["2011_Spain"], ad2_data["2012_Spain"], ad2_data["2013_Spain"], ad2_data["2014_Spain"])
weighted_average_ad2_spain <- weighted_average_forecast(ad2_jan_spain)

ad3_jan_spain <- c(ad3_data["2010_Spain"], ad3_data["2011_Spain"], ad3_data["2012_Spain"], ad3_data["2013_Spain"], ad3_data["2014_Spain"])
weighted_average_ad3_spain <- weighted_average_forecast(ad3_jan_spain)

wage_jan_spain <- c(wage_data["2010_Spain"], wage_data["2011_Spain"], wage_data["2012_Spain"], wage_data["2013_Spain"], wage_data["2014_Spain"])
weighted_average_wage_spain <- weighted_average_forecast(wage_jan_spain)

# Create data frame for Jan 2015
jan_2015_data_Spain <- data.frame(
  Ad1..GRP. = weighted_average_ad1_spain,
  Ad2..No_of_banners. = weighted_average_ad2_spain,
  Prom..No_of_stores. = weighted_average_ad3_spain,
  Month = 'Jan',
  Wage..Perc.. = weighted_average_wage_spain*100
)

# Forecast sales for January 2015
jan_2015_forecast_spain <- predict(model_spain, newdata = jan_2015_data_Spain)

# Output the forecast
print(jan_2015_forecast_spain)

# Applying the function to different datasets for Singapore region
sales_jan_singapore <- c(sales_data["2010_Singapore"], sales_data["2011_Singapore"], sales_data["2012_Singapore"], sales_data["2013_Singapore"], sales_data["2014_Singapore"])
weighted_average_sales_singapore <- weighted_average_forecast(sales_jan_singapore)

ad1_jan_singapore <- c(ad1_data["2010_Singapore"], ad1_data["2011_Singapore"], ad1_data["2012_Singapore"], ad1_data["2013_Singapore"], ad1_data["2014_Singapore"])
weighted_average_ad1_singapore <- weighted_average_forecast(ad1_jan_singapore)

ad2_jan_singapore <- c(ad2_data["2010_Singapore"], ad2_data["2011_Singapore"], ad2_data["2012_Singapore"], ad2_data["2013_Singapore"], ad2_data["2014_Singapore"])
weighted_average_ad2_singapore <- weighted_average_forecast(ad2_jan_singapore)

ad3_jan_singapore <- c(ad3_data["2010_Singapore"], ad3_data["2011_Singapore"], ad3_data["2012_Singapore"], ad3_data["2013_Singapore"], ad3_data["2014_Singapore"])
weighted_average_ad3_singapore <- weighted_average_forecast(ad3_jan_singapore)

wage_jan_singapore <- c(wage_data["2010_Singapore"], wage_data["2011_Singapore"], wage_data["2012_Singapore"], wage_data["2013_Singapore"], wage_data["2014_Singapore"])
weighted_average_wage_singapore <- weighted_average_forecast(wage_jan_singapore)

# Create data frame for Jan 2015
jan_2015_data_Singapore <- data.frame(
  Ad1..GRP. = weighted_average_ad1_singapore,
  Ad2..No_of_banners. = weighted_average_ad2_singapore,
  Prom..No_of_stores. = weighted_average_ad3_singapore,
  Month = 'Jan',
  Wage..Perc.. = weighted_average_wage_singapore*100
)

# Forecast sales for January 2015
jan_2015_forecast_singapore <- predict(model_singapore, newdata = jan_2015_data_Singapore)

# Output the forecast
print(jan_2015_forecast_singapore)

jan_2015_forecast_all <- c(jan_2015_forecast_singapore, jan_2015_forecast_spain, jan_2015_forecast_mexico, jan_2015_forecast_uk, jan_2015_forecast_us)
total_forecast_2015 = sum(jan_2015_forecast_all)

# Make predictions
predictions_ov <- predict(model_ov, newdata = data)

# Calculate MAPE
actual_ov <- data$Sales..units.
mape_ov <- mean(abs((actual_ov - predictions_ov) / actual_ov)) * 100

# Print MAPE
print(mape_ov)

# Print Predictions
predictions_ov
exp(predictions_ov)

# write.csv(exp(predictions_ov), file = "C:\\Users\\Trella\\Desktop\\Applied Stats Coursework\\predictions_ov.csv", row.names = FALSE)

# Make predictions
predictions_usa <- predict(model_usa, newdata = usa_data)

# Calculate MAPE
actual_usa <- usa_data$Sales..units.
mape_usa <- mean(abs((actual_usa - predictions_usa) / actual_usa)) * 100

# Print MAPE
print(mape_usa)

# Print Predictions
exp(predictions_usa)

# write.csv(exp(predictions_usa), file = "C:\\Users\\Trella\\Desktop\\Applied Stats Coursework\\predictions_usa.csv", row.names = FALSE)
# write.csv(actual_usa, file = "C:\\Users\\Trella\\Desktop\\Applied Stats Coursework\\actuals_usa.csv", row.names = FALSE)

# Make predictions for Spain
predictions_spain <- predict(model_spain, newdata = spain_data)

# Calculate MAPE for Spain
actual_spain <- spain_data$Sales..units.
mape_spain <- mean(abs((actual_spain - predictions_spain) / actual_spain)) * 100

# Print MAPE for Spain
print(mape_spain)

# Print Predictions for Spain
exp(predictions_spain)

# Save predictions to CSV for Spain
# write.csv(exp(predictions_spain), file = "C:\\Users\\Trella\\Desktop\\Applied Stats Coursework\\predictions_spain.csv", row.names = FALSE)

# Save actuals to CSV for Spain
# write.csv(actual_spain, file = "C:\\Users\\Trella\\Desktop\\Applied Stats Coursework\\actuals_spain.csv", row.names = FALSE)

# Make predictions for Singapore
predictions_singapore <- predict(model_singapore, newdata = singapore_data)

# Calculate MAPE for Singapore
actual_singapore <- singapore_data$Sales..units.
mape_singapore <- mean(abs((actual_singapore - predictions_singapore) / actual_singapore)) * 100

# Print MAPE for Singapore
print(mape_singapore)

# Print Predictions for Singapore
exp(predictions_singapore)

# Save predictions to CSV for Singapore
# write.csv(exp(predictions_singapore), file = "C:\\Users\\Trella\\Desktop\\Applied Stats Coursework\\predictions_singapore.csv", row.names = FALSE)

# Save actuals to CSV for Singapore
# write.csv(actual_singapore, file = "C:\\Users\\Trella\\Desktop\\Applied Stats Coursework\\actuals_singapore.csv", row.names = FALSE)

# Make predictions for Mexico
predictions_mexico <- predict(model_mexico, newdata = mexico_data)

# Calculate MAPE for Mexico
actual_mexico <- mexico_data$Sales..units.
mape_mexico <- mean(abs((actual_mexico - predictions_mexico) / actual_mexico)) * 100

# Print MAPE for Mexico
print(mape_mexico)

# Print Predictions for Mexico
exp(predictions_mexico)

# Save predictions to CSV for Mexico
# write.csv(exp(predictions_mexico), file = "C:\\Users\\Trella\\Desktop\\Applied Stats Coursework\\predictions_mexico.csv", row.names = FALSE)

# Save actuals to CSV for Mexico
# write.csv(actual_mexico, file = "C:\\Users\\Trella\\Desktop\\Applied Stats Coursework\\actuals_mexico.csv", row.names = FALSE)

# Make predictions for UK
predictions_uk <- predict(model_uk, newdata = uk_data)

# Calculate MAPE for UK
actual_uk <- uk_data$Sales..units.
mape_uk <- mean(abs((actual_uk - predictions_uk) / actual_uk)) * 100

# Print MAPE for UK
print(mape_uk)

# Print Predictions for UK
exp(predictions_uk)

# Save predictions to CSV for UK
# write.csv(exp(predictions_uk), file = "C:\\Users\\Trella\\Desktop\\Applied Stats Coursework\\predictions_uk.csv", row.names = FALSE)

# Save actuals to CSV for UK
# write.csv(actual_uk, file = "C:\\Users\\Trella\\Desktop\\Applied Stats Coursework\\actuals_uk.csv", row.names = FALSE)



