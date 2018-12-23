# Kitu Komya
# 404491375
# Final Project R Code
# Stats 170
# 11/30/2018



###############
#      1      #
###############

library(knitr)
library(kableExtra)
library(dplyr)
library(pander)

# create descriptions for table in dataframe format
var_name <- c("HOUSTNSA", "LNU04000002", "UNRATENSA")
r_name <- c("hs", "uw", "ur")
description <- c("Housing Starts is the total # of new, privately owned housing units (in thousands), measured monthly; not seasonally adjusted; this is variable to be forecasted", "Women's Unemployment Rate (in percent) measured monthly; not seasonally adjusted & Jan 1, 1959 to Aug 1, 2017 & UNRATENSA & ur & Civilian Unemployment Rate (in percent) measured monthly; not seasonally adjusted", "Civilian Unemployment Rate (in percent) measured monthly; not seasonally adjusted")
train <- rep("Jan 1, 1959 to Aug 1, 2017", 3)
test <- c("Sept 1, 2017 to Aug 1, 2019", "", "")
datatable <- data.frame(var_name, r_name, description, train, test)

# read in data
dat <- read.table("http://www.stat.ucla.edu/~jsanchez/data/hwk6data.csv", sep = ",", header = T)

# clean variable names
names(dat) <- c("hs", "uw", "ur")

# training and testing data
dat_train <- dat[1:704, ]
dat_test <- dat$hs[705:716]



###############
#      2      #
###############

# create time series object
dat_train.ts <- ts(data = dat_train, start = c(1959, 1), end = c(2017, 8), frequency = 12)

# look at a section of the data
head(dat_train.ts)

# look at statistics of the data
start(dat_train.ts) # data starts in January 1959
end(dat_train.ts) # data ends in August 2018
frequency(dat_train.ts) # data is collected monthly



###############
#      3      #
###############

# decompose time series
dat_train.ts.decompose <- decompose(dat_train.ts)
par(mfrow = c(2, 3))

# timeplots (observed) of each variable
plot(dat_train.ts.decompose$x[ , 1], main = "Time Plot of \nHousing Starts (hs)", 
xlab = "Year", ylab = "# of new private houses (in 1000s)", col = "#F8766D", lwd = 0.8)
plot(dat_train.ts.decompose$x[ , 2], main = "Time Plot of Women's \nUnemployment Rate (uw)", 
     xlab = "Year", ylab = "Women's unemployment rate (in %)", col = "#00BA38", lwd = 0.8)
plot(dat_train.ts.decompose$x[ , 3], main = "Time Plot of Civilian \nUnemployment Rate (ur)", 
     xlab = "Year", ylab = "Civilian unemployment rate (in %)", col = "#619CFF", lwd = 0.8)

# trends of each variable
plot(dat_train.ts.decompose$trend[ , 1], main = "Trend of \nHousing Starts (hs)", 
     xlab = "Year", ylab = "# of new private houses (in 1000s)", col = "#F8766D", lwd = 2)
plot(dat_train.ts.decompose$trend[ , 2], main = "Trend of Women's \nUnemployment Rate (uw)", 
     xlab = "Year", ylab = "Women's unemployment rate (in %)", col = "#00BA38", lwd = 2)
plot(dat_train.ts.decompose$trend[ , 3], main = "Trend of Civilian \nUnemployment Rate (ur)", 
     xlab = "Year", ylab = "Civilian unemployment rate (in %)", col = "#619CFF", lwd = 2)

# create descriptions for table in dataframe format
plot_type <- c("Time Plot", "Trend")
hs <- c("seasonal and cyclical component evident; large, constant variances; no general trend; not stationary; dip before 2010", "cyclical component evident; large, somewhat varying variances; no general trend; dip before 2010")
uw <- c("seasonal and cyclical component evident; somewhat constant variances; no general trend; not stationary", "cyclical component evident; somewhat constant variances; sinusoidal trend")
ur <- c("seasonal and cyclical component evident; somewhat constant variances; no general trend; not stationary", "cyclical component evident; somewhat constant variances; sinusoidal trend")
datatable <- data.frame(plot_type, hs, uw, ur)

datatable %>%
  kable() %>%
  column_spec(2:4, width = "12em")

par(mfrow = c(2, 3))
boxplot(hs ~ cycle(dat_train.ts), data = dat_train.ts, main = "Seasonal Boxplot of \nHousing Starts (hs)", 
        xlab = "Month", ylab = "# of new private houses (in 1000s)", col = "#F8766D")
boxplot(uw ~ cycle(dat_train.ts), data = dat_train.ts, main = "Seasonal Boxplot of Women's \nUnemployment Rate (uw)", 
        xlab = "Month", ylab = "Women's unemployment rate (in %)", col = "#00BA38")
boxplot(ur ~ cycle(dat_train.ts), data = dat_train.ts, main = "Seasonal Boxplot of Civilian \nUnemployment Rate (ur)",
        xlab = "Month", ylab = "Civilian unemployment rate (in %)", col = "#619CFF")

# random parts of each variable
plot(dat_train.ts.decompose$random[ , 1], main = "Random Part of \nHousing Starts (hs)", 
     xlab = "Year", ylab = "# of new private houses (in 1000s)", col = "#F8766D", lwd = 0.8)
plot(dat_train.ts.decompose$random[ , 2], main = "Random Part of Women's \nUnemployment Rate (uw)", 
     xlab = "Year", ylab = "Women's unemployment rate (in %)", col = "#00BA38", lwd = 0.8)
plot(dat_train.ts.decompose$random[ , 3], main = "Random Part of Civilian \nUnemployment Rate (ur)", 
     xlab = "Year", ylab = "Civilian unemployment rate (in %)", col = "#619CFF", lwd = 0.8)

# create descriptions for table in dataframe format
plot_type <- c("Seasonal Box Plot", "Random Plot")
hs <- c("seasonal component evident; arches to June then dips down; larger variances in summer months", "mean stationary, but variance decreases with time")
uw <- c("relatively stationary, but a slight peak in June after which it decreases; larger variance in summer months", "mean and variance stationary")
ur <- c("relatively stationary, but slightly decreases over the year; constant variance", "mean and variance stationary")

datatable <- data.frame(plot_type, hs, uw, ur)

datatable %>%
  kable() %>%
  column_spec(2:4, width = "12em")

# unit root tests
library(tseries)

adf.test(dat_train$hs)
adf.test(dat_train$uw)
adf.test(dat_train$ur)


# cointegration tests
po.test(cbind(dat_train$hs, dat_train$uw, dat_train$ur))

# create descriptions for table in dataframe format
unit_root_hs <- c("0.2943")
unit_root_uw <- c("0.119")
unit_root_ur <- c("0.09635")
cointegration_test <- c("< 0.01")

datatable <- data.frame(unit_root_hs, unit_root_uw, unit_root_ur, cointegration_test)

datatable %>%
  kable()

# volatility for GARCH models
par(mfrow = c(2, 3))
acf(dat_train$hs - mean(dat_train$hs), main = "ACF of Adjusted \nHousing Starts (hs)")
acf(dat_train$uw - mean(dat_train$uw), main = "ACF of Adjusted \nUnemployment Rates (uw)")
acf(dat_train$ur - mean(dat_train$ur), main = "ACF of Adjusted Civilian \nUnemployment Rates (ur)")

# volatility of squared mean
acf((dat_train$hs - mean(dat_train$hs))^2, main = "ACF of Squared \nMean Adjusted of hs")
acf((dat_train$uw - mean(dat_train$uw))^2, main = "ACF of Squared \nMean Adjusted of uw")
acf((dat_train$ur - mean(dat_train$ur))^2, main = "ACF of Squared \nMean Adjusted of ur")

# check correlograms and partial correlograms
acf(dat_train)




###############
#      4      #
###############

par(mfrow = c(3, 2))

# first difference
reg.diff = diff(dat_train.ts[ , 1], lag = 1, diff = 1)
plot(reg.diff, main = "Time Plot of regular \ndifferencing of hs")
acf(reg.diff, lag = 50, main = "ACF of regular \ndifferencing of hs")

# seasonal difference
seas.diff = diff(dat_train.ts[ , 1], lag = 12, diff = 1)
plot(seas.diff, main = "Time Plot of seasonal \ndifferencing of hs")
acf(seas.diff, lag = 50, main = "ACF of seasonal \ndifferencing of hs")

# seasonal difference of the regular difference
seas.reg.diff = diff(reg.diff, lag = 12, diff = 1)
plot(seas.reg.diff, main = "Time Plot of seasonal + regular \ndifferencing of hs")
acf(seas.reg.diff, lag = 50, main = "ACF of seasonal + regular \ndifferencing of hs")

# create descriptions for table in dataframe format
differencing_type <- c("Regular", "Seasonal", "Seasonal + Regular")
time_plot <- c("relatively mean stationary but not variance stationary", "neither mean nor variance stationary", "mean and variance stationary")
ACF <- c("not white noise; seasonal/cyclical trend apparent; many significant autocorrelations", "not white noise; many significant autocorrelations", "relatively white noise; few significant autocorrelations due to chance")
datatable <- data.frame(differencing_type, time_plot, ACF)

datatable %>%
  kable() %>%
  column_spec(2:3, width = "17em")

# identification of SARIMA model
par(mfrow = c(1, 2))
acf(seas.reg.diff, lag = 50, main = "ACF of seasonal + regular \ndifferencing of hs")
pacf(seas.reg.diff, lag = 50, main = "PACF of seasonal + regular \ndifferencing of hs")

# create SARIMA model by looking at the ACFs and PACFs
model_sarima = arima(dat_train.ts[ , 1], order = c(0, 1, 1), 
                     seas = list(order = c(1, 1, 1), 12))

# check residuals
acf(resid(model_sarima), na.action = na.pass, main = "ACF of Residuals of SARIMA Model")

# Ljung-Box test for white noise residuals
Box.test(model_sarima$residuals, lag = 20, type = "Ljung")

# Ljung-Box tests
Box.test(model_sarima$residuals, lag = 20, type = "Ljung")
Box.test(model_sarima$residuals^2, lag = 20, type = "Ljung")

# volatility check of residuals for GARCH models
par(mfrow = c(1, 2))
acf(resid(model_sarima), main = "ACF of Residuals for \nSARIMA Model")

# volatility of squared mean
acf(resid(model_sarima)^2, main = "ACF of Squared Residuals \nfor SARIMA Model")

# fit a SARIMA + GARCH model
model_garch1 <- garch(model_sarima$residuals, order = c(0, 1), trace = F)
model_garch2 <- garch(model_sarima$residuals, order = c(1, 0), trace = F)
model_garch3 <- garch(model_sarima$residuals, order = c(1, 1), trace = F)
model_garch4 <- garch(model_sarima$residuals, order = c(0, 2), trace = F)

# create descriptions for table in dataframe format
GARCH_model <- c("GARCH(0, 1)", "GARCH(1, 0)", "GARCH(1, 1)", "GARCH(0, 2)")
AIC <- c(AIC(model_garch1), AIC(model_garch2), AIC(model_garch3), AIC(model_garch4))
datatable <- data.frame(GARCH_model, AIC)

# create table
datatable %>%
kable()

# Ljung-Box test on the SARIMA + GARCH residuals
Box.test(model_garch3$residuals, lag = 20, type = "Ljung")
Box.test(model_garch3$residuals^2, lag = 20, type = "Ljung")

# volatility check of SARIMA + GARCH model
par(mfrow = c(1, 2))
acf(resid(model_garch3), na.action = na.pass, main = "ACF of Residuals for \nSARIMA + GARCH Model")

# volatility of squared mean
acf(resid(model_garch3)^2, na.action = na.pass, main = "ACF of Squared Residuals \nfor SARIMA + GARCH Model")

# make predictions for next 12 time points
sarima_prediction <- predict(model_sarima, n.ahead = 12)

# create descriptions for table in dataframe format
time_ahead <- c(1:12)
sarima_prediction <- sarima_prediction$pred
datatable <- data.frame(time_ahead, sarima_prediction)

# create table
datatable %>%
  kable()

# forecasts
y.pred = ts(predict(model_sarima, n.ahead = 12, se.fit = TRUE), frequency = 12,
            start = c(2017, 9))

# confidence intervals
cil = ts((y.pred$pred - 1.96 * y.pred$se), start = c(2017, 9), frequency = 12)
ciu = ts((y.pred$pred + 1.96 * y.pred$se), start = c(2017, 9), frequency = 12)

ts.plot(cbind(dat_train.ts[ , 1], y.pred$pred, cil, ciu), lty = c(1, 2, 3, 3),
        col = c("#619CFF", "#00BA38", "#F8766D", "#F8766D"), 
        ylab = "# total new private houses (in 1000s)", main = "Forecas of Housing Starts using SARIMA + GARCH Model", xlab = "Year")

# RMSE calculation
RMSE_value <- sqrt(sum((dat_test - y.pred$pred)^2)/12)

# create descriptions for table in dataframe format
model <- c("SARIMA")
RMSE <- c(RMSE_value)
datatable <- data.frame(model, RMSE)

# create table
datatable %>%
  kable()




###############
#      5      #
###############


par(mfrow = c(1, 2))
plot.ts(dat_train.ts[ , 1], type = "l", ylab = "# of new private houses (in 1000s)",
main = "Time Plot of Housing Starts")
acf(dat_train.ts[ , 1], main = "ACF of Housing Starts")

par(mfrow = c(1, 2))
monthly <- cycle(dat_train.ts) # seasonal component

# fit two classical regression models. one with only the 2 other variables, the other with seasonality as well
model_classical <- lm(dat_train.ts[ , 1] ~ dat_train.ts[ , 2] + dat_train.ts[ , 3])
model_classical2 <- lm(dat_train.ts[ , 1] ~ dat_train.ts[ , 2] + dat_train.ts[ , 3] + factor(monthly))

# check ACFs to find better model
acf(resid(model_classical), main = "ACF of classical regression model \nwith only uw and ur")
acf(resid(model_classical2), main = "ACF of classical regression model \nwith uw, ur, and seasonality")

# fit AR(1) model onto classical regression model
model_ar <- arima(ts(rstudent(model_classical2)), order = c(1, 0, 0))
corARMA_coef <- model_ar$coef[1] # coefficient used for corARMA

# ACF of residuals of AR(1) fit onto classical regression model
acf(resid(model_ar), main = "ACF of AR(1) Model's Residuals onto Classical Regression Model")

library(nlme)

# fit GLS model using AR(1) coefficient
model_gls <- gls(dat_train.ts[ , 1] ~ dat_train.ts[ , 2] + dat_train.ts[ , 3] + factor(monthly), correlation = corARMA((corARMA_coef), p = 1))

# plot ACF of resulting model
acf(resid(model_gls), main = "ACF of GLS fit on Classical Regression Model \nusing AR(1) Model Structure")

summary(model_gls)

# forecasting future time points
df_monthly <- data.frame(Monthly = rep(1:12, 1))# create out of sample data
gls_pred <- predict(model_gls, df_monthly)[1:12]
gls_prediction <- ts(gls_pred, start = c(2017, 9), frequency = 12)

# create descriptions for table in dataframe format
time_ahead <- c(1:12)
datatable <- data.frame(time_ahead, gls_prediction)

# create table
datatable %>%
kable()

# plot GLS model forecast
ts.plot(dat_train.ts[ , 1], gls_prediction, 
col = c("#F8766D", "#00BA38"), main = "Forecast of Housing Starts using GLS Model", ylab="# of new private houses (in 1000s)", xlab = "Year")

# RMSE calculation
RMSE_value <- sqrt(sum((dat_test - gls_prediction)^2)/12)

# create descriptions for table in dataframe format
model <- c("GLS")
RMSE <- c(RMSE_value)
datatable <- data.frame(model, RMSE)

# create table
datatable %>%
  kable()




###############
#      6      #
###############

# first difference
reg.diff = diff(dat_train.ts, lag = 1, diff = 1)

# seasonal difference
seas.diff = diff(dat_train.ts, lag = 12, diff = 1)

# seasonal difference of the regular difference
seas.reg.diff = diff(reg.diff, lag = 12, diff = 1)
acf(seas.reg.diff, lag = 50)

# create descriptions for table in dataframe format
input <- c(rep("hs", 3), rep("uw", 3), rep("ur", 3))
response <- rep(c("hs", "uw", "ur"), 3)
lags <- c("1, 2", "4", "none", "2", "1, 2", "1", "2", "2, 3", "1, 2")

datatable <- data.frame(input, response, lags)

# create table
datatable %>%
kable()

# fit all possibilities
library(vars)
var_none = VAR(dat_train, p = 2, type = "none")
var_trend = VAR(dat_train, p = 2, type = "trend")
var_const = VAR(dat_train, p = 2, type = "const")
var_both = VAR(dat_train, p = 2, type = "both")

# evaluate ACF of residuals
acf(resid(var_none))
acf(resid(var_trend))
acf(resid(var_const))
acf(resid(var_both))

# find which variables are leading and which are lagging
coef(var_const)
var_const_sum <- summary(var_const)

# which coefficients are significant
names(which(var_const_sum[[2]]$hs$coefficients[ , 4] < 0.05))
names(which(var_const_sum[[2]]$uw$coefficients[ , 4] < 0.05))
names(which(var_const_sum[[2]]$ur$coefficients[ , 4] < 0.05))

# create descriptions for table in dataframe format
input <- c(rep("hs", 3), rep("uw", 3), rep("ur", 3))
response <- rep(c("hs", "uw", "ur"), 3)
significant_lags <- c("1, 2", "1, 2", "1, 2", "none", "1, 2", "1", "1, 2", "1, 2", "1, 2")

datatable <- data.frame(input, response, significant_lags)

# create table
datatable %>%
kable()

# make predictions for next 12 time points
var_prediction <- predict(var_const, n.ahead = 12)

# create descriptions for table in dataframe format
time_ahead <- c(1:12)
var_prediction <- var_prediction$fcst$hs[ , 1]
datatable <- data.frame(time_ahead, var_prediction)

# create table
datatable %>%
  kable()

# forecasts
y.pred = ts(predict(var_const, n.ahead = 12, se.fit = TRUE), frequency = 12,
            start = c(2017, 9))

y.pred <- ts(y.pred$fcst$hs[,c("fcst", "lower", "upper")],
             start = c(2017,9), frequency = 12)
ts.plot(cbind(dat_train.ts[ , 1], y.pred), lty = c(1,2,2,2), col = c("#00BA38", "#F8766D", "#619CFF", "#619CFF"), main = "Forecast of Housing Starts using VAR Model", ylab = "# new private houses (in 1000s)", xlab = "Year")

# RMSE calculation
RMSE_value <- sqrt(sum((dat_test - y.pred[ , 1])^2)/12)

# create descriptions for table in dataframe format
model <- c("VAR")
RMSE <- c(RMSE_value)
datatable <- data.frame(model, RMSE)

# create table
datatable %>%
  kable()

# impulse response functions
irf.hs <- irf(var_const, impulse = "hs", response = c("hs","uw","ur"), boot = FALSE, n.ahead = 100) 

irf.uw <- irf(var_const, impulse = "uw", response = c("hs","uw","ur"), boot = FALSE, n.ahead = 100) 

irf.ur <- irf(var_const, impulse = "ur", response = c("hs","uw","ur"), boot = FALSE, n.ahead = 100) 

# plot them
par(mfrow = c(3, 1))
plot(irf.hs)
plot(irf.uw)
plot(irf.ur) 





###############
#      7      #
###############


Time_Forecasted <- c("Sept 1, 2017", "Oct 1, 2017", "Nov 1, 2017", "Dec 1, 2017",
                     "Jan 1, 2018", "Feb 1, 2018", "March 1, 2018", "April 1, 2018",
                     "May 1, 2017", "June 1, 2017", "July 1, 2017", "Aug 1, 2017")

# all columns
SARIMA <- sarima_prediction
Classical_Regression_with_GLS <- gls_pred
VAR <- var_prediction
Actual_Data <- dat_test
Average <- apply(cbind(SARIMA, Classical_Regression_with_GLS, VAR, Actual_Data), 1, mean)

# make dataframe
datatable <- data.frame(cbind(Time_Forecasted, round(SARIMA, 2), round(Classical_Regression_with_GLS, 2), round(VAR, 2), round(Average, 2), round(Actual_Data, 2)))
names(datatable) <- c("Time Forecasted", "SARIMA", "Classical Regression + GLS", "VAR", "Average", "Actual Data")

# RMSE values
RMSE <- data.frame("RMSE", round(8.958358, 2), round(32.47106, 2), round(11.49255, 3), round(sqrt(sum((as.numeric(datatable$Average) - as.numeric(datatable$`Actual Data`))^2)/12), 2), "N/A")
names(RMSE) <- names(datatable)

# create table
datatable %>%
  kable()

# RMSE table
RMSE %>%
  kable()