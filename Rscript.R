# Part 1) import libraries 
library(tidyverse)
library(lubridate) # date modification package
library(ggpubr) # usef for split view 
library(corrplot) # used to make correlation plot
library(glmnet) # Lasso 
library(leaps) # Forward Selection 
theme_set(theme_bw()) # set the theme (theme_set is built inside ggplot2)

# Part 2) import data 
seoulBike <- read_csv("Seoul_Bikeshare_Data.csv") # import csv data using read_csv function

# Part 3) clean the seoulBike data 
seoulBike <- rename_with(seoulBike, tolower) # make all the variables lowercase for consistency
# rename variables to read them clearly
seoulBike %>%  
  rename(count = 'rented bike count') %>%  
  rename(temperature = 'temperature(°c)') %>%  
  rename(humidity = 'humidity(%)') %>%  
  rename(wind_speed = 'wind speed (m/s)') %>%  
  rename(visibility = 'visibility (10m)') %>%  
  rename(dew_point_temperature = 'dew point temperature(°c)') %>%  
  rename(solar_radiation = 'solar radiation (mj/m2)') %>%  
  rename(rainfall = 'rainfall(mm)') %>%  
  rename(snowfall = 'snowfall (cm)') %>%  
  rename(functioning_day = 'functioning day') -> seoulBike
# split date variable into year, month, day, weekday and then assign them into new variables
seoulBike$date <- as.Date(seoulBike$date, format = "%d/%m/%Y")
seoulBike$year <- year(seoulBike$date)
seoulBike$month <- month(seoulBike$date)
seoulBike$day <- mday(seoulBike$date)
seoulBike$weekday <- wday(seoulBike$date, label = TRUE)
# remove unused date variable 
seoulBike %>% select(-date) -> seoulBike
# omit rows if there is missing values in this seoulBike data 
seoulBike <- na.omit(seoulBike) 
nrow(seoulBike) # I get the same records before using na.omit(), so there is no missing value in this data

# Part 4) split seoulBike data into train(90%) data and test(10%) data
set.seed(20) # random number generation
n <- length(seoulBike$count)
Z <- sample(n, n/10) # from base package, sample takes a sample of the specified size 
test <- seoulBike[sort(Z),] # for test data (10% of the full data)
train <- seoulBike[-sort(Z),] # for train data (90% of the full data)

# Part 5) exploratory analysis on train data
# Figure 1

# Histogram of Bike Share Counts -> to see the distribution of the bike share counts 
A <- qplot(train$count, 
           geom="histogram", 
           binwidth = 300,  
           main = "Histogram of Bike Share Counts", 
           xlab = "Count",  
           fill=I("blue"), 
           col=I("black"), 
           alpha=I(.2)) 

# Bike Share Mean Counts By Month (Line)
B <- train %>% 
  group_by(month) %>% 
  summarise(count_by_month = mean(count)) %>% 
  ggplot(aes(x=month, y=count_by_month)) + 
  geom_line() + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                               "Jul","Aug", "Sep", "Oct","Nov", "Dec"))+
  xlab("Month") +
  ylab("Mean Count") +
  ggtitle("Bike Share Mean Counts By Month")

# Bike Share Counts By Seasons (Boxplot)
C <- ggplot(train, aes(x=seasons, y=count, color = seasons)) + 
  geom_boxplot()+
  scale_x_discrete(limits = c("Spring", "Summer", "Autumn", "Winter"))+
  xlab("Seasons")+
  ylab("Bike Share Counts")+
  ggtitle("Bike Share Counts By Seasons")+
  theme(legend.position = "None") # delete the legend 

# Bike Share Mean Counts by Hour
D <- train %>% 
  group_by(hour) %>% 
  summarise(count_by_hour = mean(count)) %>% 
  ggplot(aes(x=hour, y=count_by_hour)) + 
  geom_line() + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), 
                   labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12",
                              "13","14","15","16","17","18","19","20","21","22","23")) +
  xlab("hour") + 
  ylab("Mean Count") +
  ggtitle("Bike Share Mean Counts by Hour")

# Bike Share Count by Hour With Temperature Scale
E <- ggplot(train, aes(x = hour, y = count))+
  geom_point(position = position_jitter(),aes(color = temperature))+
  scale_color_gradient(low = "#0099CC", high = "#FF9966") +
  scale_x_continuous(breaks = c(0,6,12,18,23), 
                     labels = c("0","6","12","18","23")) +
  xlab("Hour")+
  ylab("Bike Share Count")+
  ggtitle("Bike Share Count by Hour \n (with Temperature Scale)")

F <- ggplot(train, aes(x=weekday, y=count, color = weekday)) + 
  geom_boxplot()+
  ggtitle("Bike Share Counts by Weekday")+ 
  theme(legend.position = "None")

ggarrange(A,B,C,D,E,F, labels = c("A", "B", "C", "D", "E", "F")) # combine above six plots in one view

# Figure 2 
train_correlation <- train
train_correlation$seasons <- as.factor(train_correlation$seasons)
train_correlation$holiday <- as.factor(train_correlation$holiday)
train_correlation$functioning_day <- as.factor(train_correlation$functioning_day)
train_cor <- data.matrix(train_correlation) # convert all variables in a df to numeric mode, bind them as a matrix
corrplot(cor(train_cor), type = "upper")

# Figure 3
# Mean Temperature by Hour
hour_a <- train %>% 
  group_by(hour) %>% 
  summarise(mean_temp_by_hour = mean(temperature)) %>%  
  ggplot(aes(x=hour, y=mean_temp_by_hour)) + 
  geom_line() + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), 
                     labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12",
                                "13","14","15","16","17","18","19","20","21","22","23")) +
  xlab("Hour") + 
  ylab("Mean Temperature") +
  ggtitle("Mean Temperature by Hour")

# Mean Visibility by Hour
hour_b <- train %>% 
  group_by(hour) %>% 
  summarise(mean_visibility_by_hour = mean(visibility)) %>%  
  ggplot(aes(x=hour, y=mean_visibility_by_hour)) + 
  geom_line() + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), 
                     labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12",
                                "13","14","15","16","17","18","19","20","21","22","23")) +
  xlab("Hour") + 
  ylab("Mean Visibility") +
  ggtitle("Mean Visibility by Hour")

# Mean Windspeed by Hour
hour_c <- train %>% 
  group_by(hour) %>% 
  summarise(mean_wind_speed = mean(wind_speed)) %>%  
  ggplot(aes(x=hour, y=mean_wind_speed)) + 
  geom_line() + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), 
                     labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12",
                                "13","14","15","16","17","18","19","20","21","22","23")) +
  xlab("Hour") + 
  ylab("Mean Wind Speed") +
  ggtitle("Mean Windspeed by Hour")

# Mean Solar Radiation by Hour
hour_d <- train %>% 
  group_by(hour) %>% 
  summarise(mean_solar_radiation = mean(solar_radiation)) %>%  
  ggplot(aes(x=hour, y=mean_solar_radiation)) + 
  geom_line() + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), 
                     labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12",
                                "13","14","15","16","17","18","19","20","21","22","23")) +
  xlab("Hour") + 
  ylab("Mean Solar Radiation") +
  ggtitle("Mean Solar Radiation by Hour")

# Mean Humidity by Hour
hour_e <- train %>% 
  group_by(hour) %>% 
  summarise(mean_humid_hourly = mean(humidity)) %>%  
  ggplot(aes(x=hour, y=mean_humid_hourly)) + 
  geom_line() + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), 
                     labels = c("0","1","2","3","4","5","6","7","8","9","10","11","12",
                                "13","14","15","16","17","18","19","20","21","22","23")) +
  xlab("Hour") + 
  ylab("Mean Humidity") +
  ggtitle("Mean Humidity by Hour")

ggarrange(D,hour_a,hour_b, hour_c, hour_d, hour_e, labels = c("a", "b", "c", "d", "e", "f"))

# Figure 4 
t1 <- ggplot(train, aes(x = temperature, y = dew_point_temperature))+
  geom_point()+
  xlab("Temperature")+
  ylab("Dew Point Temperature")+
  geom_smooth(formula = y ~ x,method = "lm", se = FALSE)+
  theme_pubclean()

t2 <- ggplot(train, aes(x = temperature, y = humidity))+
  geom_point()+
  xlab("Temperature")+
  ylab("Humidity")+
  geom_smooth(formula = y ~ x,method = "lm", se = FALSE)+
  theme_pubclean()

t3 <- ggplot(train, aes(x = temperature, y = wind_speed))+
  geom_point()+
  xlab("Temperature")+
  ylab("Wind Speed")+
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE)+
  theme_pubclean()

t4 <- ggplot(train, aes(x = temperature, y = solar_radiation))+
  geom_point()+
  xlab("Temperature")+
  ylab("Solar Radiation")+
  geom_smooth(formula = y ~ x,method = "lm", se = FALSE)+
  theme_pubclean()

t5 <- ggplot(train, aes(x = temperature, y = rainfall))+
  geom_point() +
  xlab("Temperature")+
  ylab("Rainfall")+
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE)+
  theme_pubclean()

t6 <- ggplot(train, aes(x = temperature, y = snowfall))+
  geom_point()+
  xlab("Temperature")+
  ylab("Snowfall")+
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE)+
  theme_pubclean()

t7 <- ggplot(train, aes(y = temperature, x = factor(year), colour = factor(year)))+
  geom_boxplot()+
  xlab("Year")+
  ylab("Temperature")+
  theme_pubclean()+
  theme(legend.position = "None")

t8 <- ggplot(train, aes(y = temperature, x = factor(month), color = factor(month)))+
  geom_boxplot()+
  xlab("Month")+
  ylab("Temperature")+
  theme_pubclean()+
  theme(legend.position = "None")

t9 <- train %>% 
  group_by(hour) %>%  
  summarise(temp_hour = mean(temperature)) %>% 
  ggplot(aes(y = temp_hour, x = hour))+
  geom_point(colour = "steelblue", size = 2) +
  theme_pubclean()+
  xlab("Hour")+
  ylab("Mean Temperature")

ggarrange(t1,t2,t3,t4,t5,t6,t7,t8,t9, labels = c(1,2,3,4,5,6,7,8,9))

# Figure 5 

s1 <- ggplot(train, aes(x=seasons, y=temperature, fill = seasons)) + 
  geom_boxplot()+
  xlab('Seasons')+
  ylab('Temperature')+
  scale_x_discrete(limits = c("Spring", "Summer", "Autumn", "Winter"))+
  theme_classic()+
  theme(legend.position = "None")+
  scale_fill_brewer(palette="BuPu")

s2 <- ggplot(train, aes(x=seasons, y=humidity, fill = seasons)) + 
  geom_boxplot()+
  xlab('Seasons')+
  ylab('Humidity')+
  scale_x_discrete(limits = c("Spring", "Summer", "Autumn", "Winter"))+
  theme_classic()+
  theme(legend.position = "None")+
  scale_fill_brewer(palette="BuPu")

s3 <- ggplot(train, aes(x=seasons, y=wind_speed, fill = seasons)) + 
  geom_boxplot()+
  xlab('Seasons')+
  ylab('Wind Speed')+
  scale_x_discrete(limits = c("Spring", "Summer", "Autumn", "Winter"))+
  theme_classic()+
  theme(legend.position = "None")+
  scale_fill_brewer(palette="BuPu")

s4 <- ggplot(train, aes(x=seasons, y=dew_point_temperature, fill = seasons)) + 
  geom_boxplot()+
  xlab('Seasons')+
  ylab('Dew Point Temperature')+
  scale_x_discrete(limits = c("Spring", "Summer", "Autumn", "Winter"))+
  theme_classic()+
  theme(legend.position = "None")+
  scale_fill_brewer(palette="BuPu")

s5 <- ggplot(train, aes(x=seasons, y=solar_radiation, fill = seasons)) + 
  geom_boxplot()+
  scale_y_log10()+
  xlab('Seasons')+
  ylab('Solar Radiation (Log)')+
  scale_x_discrete(limits = c("Spring", "Summer", "Autumn", "Winter"))+
  theme_classic()+
  theme(legend.position = "None")+
  scale_fill_brewer(palette="BuPu")

s6 <- ggplot(train, aes(x=seasons, y=snowfall, fill = seasons)) + 
  geom_boxplot()+
  scale_y_log10()+
  xlab('Seasons')+
  ylab('Snowfall (Log)')+
  scale_x_discrete(limits = c("Spring", "Summer", "Autumn", "Winter"))+
  theme_classic()+
  theme(legend.position = "None")+
  scale_fill_brewer(palette="BuPu")

ggarrange(s1,s2,s3,s4,s5,s6, labels = c(1,2,3,4,5,6))

# Part 6: Statistical Machine Learning 

## Method1: Lasso 
y <- train$count
x <- data.matrix(train[,-1])
y_test <- test$count 
x_test <- data.matrix(test[,-1])

set.seed(20)
#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1, family = "poisson", nfolds = 5)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

# Train: use lasso regression model to predict response value
predicted_lasso <- predict(best_model, s = best_lambda, newx = x, type = "response")

data.frame(actual=train$count, predicted=predicted_lasso) %>%  
  ggplot(aes(x=s1, y=actual)) + 
  geom_point() +
  geom_abline(intercept=0, slope=1, color = 'red')+
  labs(x='Predicted Values', y='Actual Values', title='Actual vs Prediction on Train data(Lasso)')

sqrt(mean((y - predicted_lasso)^2))

#Test: use lasso regression model to predict response value
predicted_lasso1 <- predict(best_model, s = best_lambda, newx = x_test, type = "response")

data.frame(actual=y_test, predicted =predicted_lasso1) %>%  
  ggplot(aes(x=s1, y=actual)) + 
  geom_point() +
  geom_abline(intercept=0, slope=1, color = 'red')+
  labs(x='Predicted Values', y='Actual Values', title='Actual vs Prediction on Test data(Lasso)')

sqrt(mean((y_test - predicted_lasso1)^2))

# Method 2: Poisson regression 
# Train data
poisson_train <- train # assign it to new variable
poisson_train$hour <- factor(poisson_train$hour)
poisson_train$seasons <- factor(poisson_train$seasons)
poisson_train$holiday <- factor(poisson_train$holiday)
poisson_train$functioning_day <- factor(poisson_train$functioning_day)
poisson_train$year <- factor(poisson_train$year)
poisson_train$month <- factor(poisson_train$month)

# Test data
poisson_test <- test # assign it to new variable
poisson_test$hour <- factor(poisson_test$hour)
poisson_test$seasons <- factor(poisson_test$seasons)
poisson_test$holiday <- factor(poisson_test$holiday)
poisson_test$functioning_day <- factor(poisson_test$functioning_day)
poisson_test$year <- factor(poisson_test$year)
poisson_test$month <- factor(poisson_test$month)
  
mod.pois <- glm(formula = count ~ . , data=poisson_train, family=poisson)
summary(mod.pois)

# Train Data
poisson_pred_count_train <- predict(mod.pois, newdata = poisson_train, type="response")
data.frame(actual=train$count, predicted=poisson_pred_count_train) %>%  
  ggplot(aes(x=predicted, y=actual)) + 
  geom_point() +
  geom_abline(intercept=0, slope=1, color = 'red')+
  labs(x='Predicted Values', y='Actual Values', title='Actual vs Prediction on Train data(Poisson)')

sqrt(mean((poisson_train$count - poisson_pred_count_train)^2))

# Test Data
poisson_pred_count_test <- predict(mod.pois, newdata = poisson_test, type="response")

data.frame(actual=test$count, predicted=poisson_pred_count_test) %>%  
  ggplot(aes(x=predicted, y=actual)) + 
  geom_point() +
  geom_abline(intercept=0, slope=1, color = 'red')+
  labs(x='Predicted Values', y='Actual Values', title='Actual vs Prediction on Test data(Poisson)')

sqrt(mean((poisson_test$count - poisson_pred_count_test)^2))


# Method3: Forward Stepwise Selection
forward_train <- train 
forward_test <- test
forward_train$hour <- factor(forward_train$hour)
forward_train$seasons <- factor(forward_train$seasons)
forward_train$holiday <- factor(forward_train$holiday)
forward_train$functioning_day <- factor(forward_train$functioning_day)
forward_train$year <- factor(forward_train$year)
forward_train$month <- factor(forward_train$month)

forward_test$hour <- factor(forward_test$hour)
forward_test$seasons <- factor(forward_test$seasons)
forward_test$holiday <- factor(forward_test$holiday)
forward_test$functioning_day <- factor(forward_test$functioning_day)
forward_test$year <- factor(forward_test$year)
forward_test$month <- factor(forward_test$month)

rf <- regsubsets(count ~ ., forward_train, nvmax = 19, method = "forward")
r_summary <- summary(rf)
r_summary

par(mfrow = c(1,3))
plot(r_summary$cp, xlab="Number of Variables", ylab="Cp", type = "l")
plot(r_summary$bic, xlab="Number of Variables", ylab="BIC", type ="l")
plot(r_summary$adjr2, xlab="Number of Variables", ylab="adjusted-R2", type = "l")

which.min(r_summary$cp) # lowest Cp value 
which.min(r_summary$bic) # lowest bic value
which.max(r_summary$adjr2) # highest adjusted R^2 value 

plot(rf, scale = "Cp")
plot(rf, scale = "bic")
plot(rf, scale = "adjr2")

coef(rf, 20) %>%  
  round()

fit <- lm(count ~ hour+temperature+humidity+rainfall+seasons+functioning_day+month+weekday, data = forward_train)
summary(fit)

forward_train_predict <- predict(fit, forward_train)

data.frame(actual=train$count, predicted=forward_train_predict) %>%  
  ggplot(aes(x=predicted, y=actual)) + 
  geom_point() +
  geom_abline(intercept=0, slope=1, color = 'red')+
  labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values on Training data(Forward)')

sqrt(mean((forward_train$count - forward_train_predict)^2))

forward_test_predict <- predict(fit, forward_test)
data.frame(actual=forward_test$count, predicted=forward_test_predict) %>%  
  ggplot(aes(x=predicted, y=actual)) + 
  geom_point() +
  geom_abline(intercept=0, slope=1, color = 'red')+
  labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values on Test data(Forward)')
sqrt(mean((forward_test$count - forward_test_predict)^2))

