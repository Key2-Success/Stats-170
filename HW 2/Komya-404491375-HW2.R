# KOMYA, KITU
# 404-491-375
# Due: 10/26/18
# Homework 2

##############
#     1a     #
##############

par(mfrow = c(2, 2))

# AR(1) with alpha = -0.9.
y = ts(arima.sim(n = 100, list(ar = c(-0.9)), innov = rnorm(100)))
y.ar = arima(y, order = c(1, 0, 0))
y.pred = ts(predict(y.ar, n.ahead = 10, se.fit = TRUE))
cil = ts((y.pred$pred - 1.96 * y.pred$se), start = 101)
ciu = ts((y.pred$pred +1.96 * y.pred$se), start = 101)
ts.plot(cbind(y, y.pred$pred, cil, ciu), lty = c(1, 2, 3, 3),
        col=c("blue", "green", "red","red"), ylab="y_t", main = "AR(1) Coefficient -0.9")

# AR(1) with alpha = -0.5.
y = ts(arima.sim(n = 100, list(ar = c(-0.5)), innov = rnorm(100)))
y.ar = arima(y, order = c(1, 0, 0))
y.pred = ts(predict(y.ar, n.ahead = 10, se.fit = TRUE))
cil = ts((y.pred$pred - 1.96 * y.pred$se), start = 101)
ciu = ts((y.pred$pred +1.96 * y.pred$se), start = 101)
ts.plot(cbind(y, y.pred$pred, cil, ciu), lty = c(1, 2, 3, 3),
        col=c("blue", "green", "red","red"), ylab="y_t", main = "AR(1) Coefficient -0.5")

# AR(1) with alpha = 0.5.
y = ts(arima.sim(n = 100, list(ar = c(0.5)), innov = rnorm(100)))
y.ar = arima(y, order = c(1, 0, 0))
y.pred = ts(predict(y.ar, n.ahead = 10, se.fit = TRUE))
cil = ts((y.pred$pred - 1.96 * y.pred$se), start = 101)
ciu = ts((y.pred$pred +1.96 * y.pred$se), start = 101)
ts.plot(cbind(y, y.pred$pred, cil, ciu), lty = c(1, 2, 3, 3),
        col=c("blue", "green", "red","red"), ylab="y_t", main = "AR(1) Coefficient 0.5")

# AR(1) with alpha = 0.9.
y = ts(arima.sim(n = 100, list(ar = c(0.9)), innov = rnorm(100)))
y.ar = arima(y, order = c(1, 0, 0))
y.pred = ts(predict(y.ar, n.ahead = 10, se.fit = TRUE))
cil = ts((y.pred$pred - 1.96 * y.pred$se), start = 101)
ciu = ts((y.pred$pred +1.96 * y.pred$se), start = 101)
ts.plot(cbind(y, y.pred$pred, cil, ciu), lty = c(1, 2, 3, 3),
        col=c("blue", "green", "red","red"), ylab="y_t", main = "AR(1) Coefficient 0.9")

dev.off()

##############
#     1b     #
##############

par(mfrow = c(3, 1))

# AR(1) alpha = 1.01
par(mfrow=c(1,3))
y1 <- w <- rnorm(100)
for (t in 2:100) y1[t] = 1.01 * y1[t - 1] + w[t]
y1 = ts(y1)
y.ar1 = arima(y1, order = c(1, 0, 0))
y.pred1 = ts(predict(y.ar1, n.ahead = 10, se.fit = TRUE))
cil1 = ts((y.pred1$pred - 1.96 * y.pred1$se), start = 101)
ciu1 = ts((y.pred1$pred + 1.96 * y.pred1$se), start = 101)
ts.plot(cbind(y1, y.pred1$pred, cil1, ciu1), lty = c(1, 2, 3, 3),
        col=c("blue", "green", "red","red"), ylab="y_t", main = "AR(1) Coefficient 1.01")

# AR(1) alpha = 1.02
y2 <- w <- rnorm(100)
for (t in 2:100) y2[t] = 1.02 * y2[t - 1] + w[t]
y2 = ts(y2)
#y.ar2 = arima(y2, order = c(1, 0, 0))
y.pred2 = ts(predict(y.ar2, n.ahead = 10, se.fit = TRUE))
cil2 = ts((y.pred2$pred - 1.96 * y.pred2$se), start = 101)
ciu2 = ts((y.pred2$pred + 1.96 * y.pred2$se), start = 101)
ts.plot(cbind(y2, y.pred2$pred, cil2, ciu2), lty = c(1, 2, 3, 3),
        col=c("blue", "green", "red","red"), ylab="y_t", main = "AR(1) Coefficient 1.02")


# AR(1) alpha 1.05
y3 <- w <- rnorm(100)
for (t in 2:100) y3[t] = 1.05 * y3[t - 1] + w[t]
y3 = ts(y3)
#y.ar3 = arima(y3, order = c(1, 0, 0))
#y.pred3 = ts(predict(y.ar3, n.ahead = 10, se.fit = TRUE))
#cil3 = ts((y.pred3$pred - 1.96 * y.pred3$se), start = 101)
#ciu3 = ts((y.pred3$pred + 1.96 * y.pred3$se), start = 101)
ts.plot(cbind(y3, y.pred3$pred, cil3, ciu3), lty = c(1, 2, 3, 3),
        col=c("blue", "green", "red","red"), ylab="y_t", main = "AR(1) Coefficient 1.05")

# roots of polynomial
Mod(polyroot(c(1, -1.01)))
Mod(polyroot(c(1, -1.02)))
Mod(polyroot(c(1, -1.05)))

dev.off()

##############
#     1c     #
##############

par(mfrow=c(1,3))

# alpha = 1.01 with differencing
y.star1= diff(y1, lag=1, diff=1) 
plot.ts(y.star1, main ="time series plot with regular differencing")
acf(y.star1)
y_fit1=arima(y1, order=c(1,1,0))
y.pred1 = ts(predict(y_fit1, n.ahead = 3, se.fit = TRUE))
cil1 = ts((y.pred1$pred - 1.96 * y.pred1$se), start = 101)
ciu1 = ts((y.pred1$pred + 1.96 * y.pred1$se), start = 101)
ts.plot(cbind(y1, y.pred1$pred, cil1, ciu1), lty = c(1, 2, 3, 3),
        col=c("blue", "green", "red","red"), ylab="y_t", main = "AR(1) Coefficient 1.01 with regular differencing")

# alpha = 1.02 with differencing
y.star2= diff(y2, lag=1, diff=1) 
plot.ts(y.star2, main="time series plot with regular differencing")
acf(y.star2)
y_fit2=arima(y2, order=c(1,1,0))
y.pred2 = ts(predict(y_fit2, n.ahead = 3, se.fit = TRUE))
cil2 = ts((y.pred2$pred - 1.96 * y.pred2$se), start = 101)
ciu2 = ts((y.pred2$pred + 1.96 * y.pred2$se), start = 101)
ts.plot(cbind(y2, y.pred2$pred, cil2, ciu2), lty = c(1, 2, 3, 3),
        col=c("blue", "green", "red","red"), ylab="y_t", main = "AR(1) Coefficient 1.02 with regular differencing")

# alpha = 1.05 with differencing
y.star3= diff(y3, lag=1, diff=1) 
plot.ts(y3, main="time series plot with regular differencing")
acf(y.star3)
y_fit3=arima(y3, order=c(1,1,0))
y.pred3 = ts(predict(y_fit3, n.ahead = 3, se.fit = TRUE))
cil3 = ts((y.pred3$pred - 1.96 * y.pred3$se), start = 101)
ciu3 = ts((y.pred3$pred + 1.96 * y.pred3$se), start = 101)
ts.plot(cbind(y3, y.pred3$pred, cil3, ciu3), lty = c(1, 2, 3, 3),
        col=c("blue", "green", "red","red"), ylab="y_t", main = "AR(1) Coefficient 1.05 with regular differencing")

# look at fits
y_fit1
y_fit2

dev.off()



##############
#     2a     #
##############

x = ts(arima.sim(n = 1000, list(ar = c(5/6, -1/6)), 
                 innov = rnorm(1000)))  # simulated data 
plot.ts(x) # time series plot


##############
#     2b     #
##############

# plots
par(mfrow = c(1, 2))
acf(x)
pacf(x)


##############
#     2c     #
##############

x_AR = arima(x, order = c(2, 0, 0)) # AR model
x_AR


##############
#     2d     #
##############

# confidence intervals
x_AR$coef[1] - 1.96 * 0.0311
x_AR$coef[1] + 1.96 * 0.0311
x_AR$coef[2] - 1.96 * 0.0311
x_AR$coef[2] + 1.96 * 0.0311


##############
#     2e     #
##############

Mod(polyroot(c(1, -5/6, 1/6))) # roots of polynomial


##############
#     2f     #
##############

# plots
par(mfrow = c(1, 2))
acf(resid(x_AR))
pacf(resid(x_AR))


##############
#     3a     #
##############

# roots of polynomial
Mod(polyroot(c(1, -3/2, 1/2))) 


##############
#     3b     #
##############

# roots of polynomial
Mod(polyroot(c(1, -5/2, -2, -1/2))) 


##############
#     3c     #
##############

x <- w <- rnorm(1000) # white noise
for (t in 3:1000) x[t] = (3/2) * x[t - 1] - (1/2) * x[t - 2] + w[t]
x = ts(x)
y = diff(x, lag = 1, diff = 1)


##############
#     3d     #
##############

# confidence intervals
y_ar = arima(x, order=c(2,1,0))

y_ar$coef[1] - 1.96 * 0.0316
y_ar$coef[1] + 1.96 * 0.0316
y_ar$coef[2] + 1.96 * 0.0316
y_ar$coef[2] - 1.96 * 0.0316


##############
#     3e     #
##############

# plots
par(mfrow = c(1, 2))
acf(resid(y_ar))
pacf(resid(y_ar))
