# Kitu Komya
# 404491375
# Homework 1

# Question 1a.

par(mfrow = c(3, 3)) # to put the next graphs

# Question 1b.
y = arima.sim(n = 100, list(ma = -0.3), innov = rnorm(100)) # generating MA(1) data. ma = -0.3 is the value of theta_1
plot.ts(y, main = "Time plot of MA(1), theta = -0.3") # time plot must be line plot
acf(y, main = "ACT of MA(1), theta = -0.3") # correlogram
pacf(y, main = "Partial ACF of MA(1), theta = -0.3") # partial correlogram

y = arima.sim(n = 100, list(ma = -0.7), innov = rnorm(100)) # generating MA(1) data. ma = -0.7 is the value of theta_1
plot.ts(y, main = "Time plot of MA(1), theta = -0.7") # time plot must be line plot
acf(y, main = "ACF of MA(1), theta = -0.7") # correlogram
pacf(y, main = "Partial ACF of MA(1), theta = -0.7") # partial correlogram

y = arima.sim(n = 100, list(ma = -0.9), innov = rnorm(100)) # generating MA(1) data. ma = -0.9 is the value of theta_1
plot.ts(y, main = "Time plot of MA(1), theta = -0.9") # time plot must be line plot
acf(y, main = "ACF of MA(1), theta = -0.9") # correlogram
pacf(y, main = "Partial ACF of MA(1), theta = -0.9") # partial correlogram
dev.off()

# Question 1g
y = arima.sim(n = 100, list(ma = -0.3), innov = rnorm(100)) # generating MA(1) data. ma = -0.3 is the value of theta_1
acf(y)$acf[2] # to see r_1

y = arima.sim(n = 100, list(ma = -0.7), innov = rnorm(100)) # generating MA(1) data. ma = -0.3 is the value of theta_1
acf(y)$acf[2] # to see r_1

y = arima.sim(n = 100, list(ma = -0.9), innov = rnorm(100)) # generating MA(1) data. ma = -0.3 is the value of theta_1
acf(y)$acf[2] # to see r_1

# Question 1h
ARMAacf(ar = c(0), ma = c(-0.3), lag.max = 10) # theoretical vals
ARMAacf(ar = c(0), ma = c(-0.7), lag.max = 10)
ARMAacf(ar = c(0), ma = c(-0.9), lag.max = 10)

# Question 4a
rooms = scan("http://www.stat.ucla.edu/~jsanchez/rooms.txt")
rooms = ts(rooms, frequency = 12) # make time series
class(rooms)
plot.ts(rooms, main = "Time Series") # plot time plot

# Question 4b
rooms1 = log(rooms)
plot.ts(rooms1, main = "Time Series") # plot log time plot
class(rooms1)

# Question 4c
cycle(rooms1)
boxplot(rooms1~cycle(rooms1), main = "Logged Rooms Monthly Boxplot", xlab = "Month") # monthly cycles

# Question 4d
rooms1.decompose = decompose(rooms1) # decompose ts
plot(rooms1.decompose) # plot it
title("Logged monthly Rooms data")

# Question 4e
y = rooms1.decompose$random # random feature
class(y) # ts object

# Question 4f
acf(y, na.action = na.pass, main = "ACF of logged rooms data") # acf plot
pacf(y, na.action = na.pass, main = "Partial ACF of logged rooms data") # acf plot

acf(y, na.action = na.pass)$acf # acf coefficient values

# Question 4g
par(mfrow = c(2, 2))
acf(rooms1.decompose$x, main = "ACF of entire ts") # acf of entire ts
acf(rooms1.decompose$trend, na.action = na.pass, main = "ACF of trend") # acf of trend
acf(rooms1.decompose$seasonal, main = "ACF of seasonal") # acf of seasonal
acf(rooms1.decompose$random, na.action = na.pass, main = "ACF of random") # acf of random
dev.off()

# Question 5a
serend = c(39, 35, 16, 18, 7, 22, 13, 18, 20, 9, -12, -11, -19, -9, -2, 16)
cagey = c(47, -26, 42, -10, 27, -8, 16, 6, -1, 25, 11, 1, 25, 7, -5, 3)

serend = ts(serend) # make ts
cagey = ts(cagey) # make ts

par(mfrow = c(2, 1))
ts.plot(serend, main = "TS plot of Serendipity Shiraz vineyard")
ts.plot(cagey, main = "TS plot of Cagey Chardonnay vineyard")

# Question 5b
acf(serend)
acf(cagey)