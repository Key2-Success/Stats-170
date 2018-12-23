# Kitu Komya
# 404-491-375
# Homework 6
# Due 12/7/18


##########
#   1A   #
##########

Global = scan("http://www.stat.ucla.edu/~jsanchez/data/global.dat.txt")
Global.ts = ts(Global, st = c(1856, 1), end = c(2005, 12), fr = 12)
plot(Global.ts, main = "Global temperature series", ylab = "temperature (departures from monthly means)")
boxplot(Global.ts~cycle(Global.ts))


##########
#   1B   #
##########

# decompose
global_dec <- decompose(Global.ts)
plot(global_dec)

# plot trend with seasonality
plot(global_dec$trend, main = "Global temperature series", ylab = "temperature (departures from monthly means)")
lines(global_dec$seasonal)


##########
#   1C   #
##########

acf(Global.ts)

##########
#   1D   #
##########

# only trend, no seasonality
hw_train <- HoltWinters(Global.ts, beta = T, gamma = F)
hw_train
