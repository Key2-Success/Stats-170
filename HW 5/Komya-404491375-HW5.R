# KOMYA, KITU
# 404-491-375
# Stats 170
# HW 5: due 11/16/18



#############
#    1A     #
#############

data = read.table("http://www.stat.ucla.edu/~jsanchez/data/stockmarket.dat", header = T)
head(data)
stock = ts(data) # make time series

plot.ts(stock) # plot time series


#############
#    1B     #
#############

acf(stock) # acf
acf(diff(stock)) # acf of differenced


#############
#    1C     #
#############

library(vars)
stock.var = VAR(stock, p = 3, type = "trend") # fit VAR(3) with trend because time series plots have trend

# look at significant coefficients
coef(stock.var)
stock.var.summ = summary(stock.var)
names(which(stock.var.summ[[2]]$Singapore$coefficients[ , 4] < 0.05))
names(which(stock.var.summ[[2]]$Amsterdam$coefficients[ , 4] < 0.05))
names(which(stock.var.summ[[2]]$Frankfurt$coefficients[ , 4] < 0.05))
names(which(stock.var.summ[[2]]$Japan$coefficients[ , 4] < 0.05))
names(which(stock.var.summ[[2]]$NewYork$coefficients[ , 4] < 0.05))
names(which(stock.var.summ[[2]]$London$coefficients[ , 4] < 0.05))
names(which(stock.var.summ[[2]]$HongKong$coefficients[ , 4] < 0.05))


#############
#    1D     #
#############

acf(resid(stock.var)) # look at ACF of residuals
hist(resid(stock.var)) # look at distribution of residuals


#############
#    1E     #
#############

stock.var.pred <- predict(stock.var, n.ahead = 4)
stock.var.pred

# to plot
graphics.off()
par("mar") 
par(mar=c(1,1,1,1))

# plot forecast
plot(stock.var.pred)

# confidence intervals. using a window because otherwise CI not visible
am.pred <- ts(stock.var.pred$fcst$Amsterdam[ , c("fcst", "lower", "upper")], start = c(3129, 1))
fr.pred <- ts(stock.var.pred$fcst$Frankfurt[ , c("fcst", "lower", "upper")], start = c(3129, 1))
lo.pred <- ts(stock.var.pred$fcst$London[ , c("fcst", "lower", "upper")], start = c(3129, 1))
ho.pred <- ts(stock.var.pred$fcst$HongKong[ , c("fcst", "lower", "upper")], start = c(3129, 1))
ja.pred <- ts(stock.var.pred$fcst$Japan[ , c("fcst", "lower", "upper")], start = c(3129, 1))
si.pred <- ts(stock.var.pred$fcst$Singapore[ , c("fcst", "lower", "upper")], start = c(3129, 1))
ne.pred <- ts(stock.var.pred$fcst$NewYork[ , c("fcst", "lower", "upper")], start = c(3129, 1))

par(mfrow = c(3,3))

ts.plot(cbind(window(stock[ , "Amsterdam"], start = 3100), am.pred), lty = c(1, 2, 2, 2), col = c("black", "green", "red", "red"), main = "Forecast of Amsterdam, dashed line")
ts.plot(cbind(window(stock[ , "Frankfurt"], start = 3100), fr.pred), lty = c(1, 2, 2, 2), col = c("black", "green", "red", "red"), main = "Forecast of Frankfurt, dashed line")
ts.plot(cbind(window(stock[ , "London"], start = 3100), lo.pred), lty = c(1, 2, 2, 2), col = c("black", "green", "red", "red"), main = "Forecast of London, dashed line")
ts.plot(cbind(window(stock[ , "HongKong"], start = 3100), ho.pred), lty = c(1, 2, 2, 2), col = c("black", "green", "red", "red"), main = "Forecast of Hong Kong, dashed line")
ts.plot(cbind(window(stock[ , "Japan"], start = 3100), ja.pred), lty = c(1, 2, 2, 2), col = c("black", "green", "red", "red"), main = "Forecast of Japan, dashed line")
ts.plot(cbind(window(stock[ , "Singapore"], start = 3100), si.pred), lty = c(1, 2, 2, 2), col = c("black", "green", "red", "red"), main = "Forecast of Singapore, dashed line")
ts.plot(cbind(window(stock[ , "NewYork"], start = 3100), ne.pred), lty = c(1, 2, 2, 2), col = c("black", "green", "red", "red"), main = "Forecast of NewYork, dashed line")

dev.off()


#############
#    1F     #
#############

# shock applied to USA/NY
irf1 = irf(stock.var, impulse = "NewYork", response = c("Amsterdam","Frankfurt", "London", "HongKong", "Japan", "Singapore", "NewYork"), boot = FALSE, n.ahead = 100)
plot(irf1)


#############
#    1G     #
#############

# shock applied to Japan
irf2 = irf(stock.var, impulse = "Japan", response = c("Amsterdam","Frankfurt", "London", "HongKong", "Japan", "Singapore", "NewYork"), boot = FALSE, n.ahead = 100)
plot(irf2)


#############
#    1H     #
#############

# shock applied to London
irf3 = irf(stock.var, impulse = "London", response = c("Amsterdam","Frankfurt", "London", "HongKong", "Japan", "Singapore", "NewYork"), boot = FALSE, n.ahead = 100)
plot(irf3)
