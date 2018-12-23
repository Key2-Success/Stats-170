# KOMYA, KITU
# 404-491-375
# Due: 10/30/18
# Stats 170
# Homework 3


##############
#     1A     #
##############

dat <- read.table("http://www.stat.ucla.edu/~jsanchez/data/osvisit.dat", header = F) # read in data

osvisit = dat$V1[1:216] # training set
testset = dat$V1[217:228] # keep for RMSE

sqrt.osvisit.ts = ts(sqrt(osvisit), start = c(1977, 1), freq = 12) # sqrt data
plot(cbind(osvisit, sqrt.osvisit.ts), main = "Overseas visitors, raw data (top) and sqrt transform (bottom)")


##############
#     1B     #
##############

acf(sqrt.osvisit.ts, main = "Correlogram of sqrt transform") # correlogram


##############
#     1C     #
##############

par(mfrow = c(3, 2))

# first difference
reg.diff = diff(sqrt.osvisit.ts, lag = 1, diff = 1)
acf(reg.diff, lag = 50)
pacf(reg.diff, lag = 50)

# seasonal difference
seas.diff = diff(sqrt.osvisit.ts, lag = 12, diff = 1)
acf(seas.diff, lag = 50)
pacf(seas.diff, lag = 50)

# seasonal difference of the regular difference
seas.reg.diff = diff(reg.diff, lag = 12, diff = 1)
acf(seas.reg.diff, lag = 50)
pacf(seas.reg.diff, lag = 50)

dev.off()


##############
#     1D     #
##############

par(mfrow = c(2, 2))

# model 1
model1 = arima(sqrt.osvisit.ts, order = c(1, 1, 0), 
               seas = list(order = c(1, 1, 0), 12))
model1

acf(resid(model1), main = "ACF of Model1") # correlogram
pacf(resid(model1), main = "PACF of Model1") # partial correlogram

# model 2
model2 = arima(sqrt.osvisit.ts, order = c(0, 1, 1), 
               seas = list(order = c(0, 1, 1), 12))
model2

acf(resid(model2), main = "ACF of Model2") # correlogram
pacf(resid(model2), main = "PACF of Model2") # partial correlogram

dev.off()



##############
#     1E     #
##############

par(mfrow = c(2, 2))

# model 1
model.1 = arima(sqrt.osvisit.ts, order = c(1, 1, 0), 
                seas = list(order = c(0, 1, 0), 12))
model.1
acf(resid(model.1), main = "ACF of Model1")

# model 2
model.2 = arima(sqrt.osvisit.ts, order = c(1, 1, 0), 
                seas = list(order = c(1, 1, 0), 12))
model.2
acf(resid(model.2), main = "ACF of Model2")

# model 3
model.3 = arima(sqrt.osvisit.ts, order = c(0, 1, 1), 
                seas = list(order = c(0, 1, 1), 12))
model.3
acf(resid(model.3), main = "ACF of Model3")

# model 4
model.4 = arima(sqrt.osvisit.ts, order = c(1, 1, 0), 
                seas = list(order = c(0, 1, 1), 12))
model.4
acf(resid(model.4), main = "ACF of Model4")

dev.off()
par(mfrow = c(2, 2))

# model 5
model.5 = arima(sqrt.osvisit.ts, order = c(0, 1, 1), 
                seas = list(order = c(1, 1, 0), 12))
model.5
acf(resid(model.5), main = "ACF of Model5")

# model 6
model.6 = arima(sqrt.osvisit.ts, order = c(1, 1, 1), 
                seas = list(order = c(1, 1, 1), 12))
model.6
acf(resid(model.6), main = "ACF of Model6")

# model 7
model.7 = arima(sqrt.osvisit.ts, order = c(1, 1, 1), 
                seas = list(order = c(1, 1, 0), 12))
model.7
acf(resid(model.7), main = "ACF of Model7")

# model 8
model.8 = arima(sqrt.osvisit.ts, order = c(1, 1, 1), 
                seas = list(order = c(0, 1, 1), 12))
model.8
acf(resid(model.8), main = "ACF of Model8")

# use Ljung-Box test
Box.test(model.1$residuals, lag = 20, type = "Ljung")
Box.test(model.2$residuals, lag = 20, type = "Ljung")
Box.test(model.3$residuals, lag = 20, type = "Ljung")
Box.test(model.4$residuals, lag = 20, type = "Ljung")
Box.test(model.5$residuals, lag = 20, type = "Ljung")
Box.test(model.6$residuals, lag = 20, type = "Ljung")
Box.test(model.7$residuals, lag = 20, type = "Ljung")
Box.test(model.8$residuals, lag = 20, type = "Ljung")

# confidence intervals for models with "white noise"
model.2$coef - 1.96*(sqrt(diag(model.2$var.coef)))
model.2$coef + 1.96*sqrt(diag(model.2$var.coef))

model.4$coef - 1.96*sqrt(diag(model.4$var.coef))
model.4$coef + 1.96*sqrt(diag(model.4$var.coef))

model.6$coef - 1.96*sqrt(diag(model.6$var.coef))
model.6$coef + 1.96*sqrt(diag(model.6$var.coef))

model.7$coef - 1.96*sqrt(diag(model.7$var.coef))
model.7$coef + 1.96*sqrt(diag(model.7$var.coef))

model.8$coef - 1.96*sqrt(diag(model.8$var.coef))
model.8$coef + 1.96*sqrt(diag(model.8$var.coef))


##############
#     1F     #
##############

# AIC
model.2$aic
model.4$aic
model.6$aic
model.7$aic
model.8$aic

# sigma squared
model.2$sigma2
model.4$sigma2
model.6$sigma2
model.7$sigma2
model.8$sigma2

# RMSE
pred2 = predict(model.2, n.ahead = 12)
rmse2 = sqrt(((sum(testset - (pred2$pred)^2)^2)/12))
rmse2

pred4 = predict(model.4, n.ahead = 12)
rmse4 = sqrt(((sum(testset - (pred4$pred)^2)^2)/12))
rmse4

pred6 = predict(model.6, n.ahead = 12)
rmse6 = sqrt(((sum(testset - (pred6$pred)^2)^2)/12))
rmse6

pred7 = predict(model.7, n.ahead = 12)
rmse7 = sqrt(((sum(testset - (pred7$pred)^2)^2)/12))
rmse7

pred8 = predict(model.8, n.ahead = 12)
rmse8 = sqrt(((sum(testset - (pred8$pred)^2)^2)/12))
rmse8



##############
#     1H     #
##############
dev.off()

y.pred = ts(predict(model.6, n.ahead = 12, se.fit = TRUE))
y.pred

cil = ts((y.pred$pred - 1.96 * y.pred$se), start = c(1995, 1), frequency = 12)
ciu = ts((y.pred$pred + 1.96 * y.pred$se), start = c(1995, 1), frequency = 12)

cil; ciu

ts.plot(cbind(osvisit.ts, (y.pred$pred)^2, cil^2, ciu^2), lty = c(1, 2, 3, 3),
        col = c("blue", "green", "red", "red"), 
        ylab = "# of overseas visitors to New Zealand", main = "Forecasting Model 6")

# coefficients of Model 6
model.6
