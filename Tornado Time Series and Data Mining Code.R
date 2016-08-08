
#Time Series R Code
install.packages("forecast", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
library("forecast") 
getwd()
StormData <- OklahomaTornado2009to2015
names(StormData)
dim(StormData)
summary(StormData)
BeginDateTime <- StormData$BEGIN_DATE_TIME
StormDataModified <- as.numeric(StormData)
InjuriesDirect <- as.numeric(StormData$INJURIES_DIRECT)
InjuriesIndirect <- as.numeric(StormData$INJURIES_INDIRECT) 
DeathsDirect <- as.numeric(StormData$DEATHS_DIRECT) 
DeathsIndirect <- as.numeric(StormData$DEATHS_INDIRECT)
DamageProperty <- as.numeric(StormData$DAMAGE_PROPERTY)
DamageCrops <- as.numeric(StormData$DAMAGE_CROPS)
Magnitude <- as.numeric(StormData$MAGNITUDE)
TornadoLength <- as.numeric(StormData$TOR_LENGTH)
TornadoWidth <- as.numeric(StormData$TOR_WIDTH)
BeginRange <- as.numeric(StormData$BEGIN_RANGE)
EndRange <- as.numeric(StormData$END_RANGE)
BeginningLatitude <- as.numeric(StormData$BEGIN_LAT)
BeginningLongitude <- as.numeric(StormData$BEGIN_LON)
EndLatitude <- as.numeric(StormData$END_LAT)
EndLongitude <- as.numeric(StormData$END_LON)
StormDataNew <- data.frame(BeginDateTime,InjuriesDirect,InjuriesIndirect,DeathsDirect,DeathsIndirect,
                           DamageProperty,TornadoLength,TornadoWidth,
                           BeginningLatitude,BeginningLongitude)
names(StormDataNew)
dim(StormDataNew)
summary(StormDataNew)
class(StormDataNew$InjuriesDirect)
cor(StormDataNew)


#TimeSeriesRCode
#Time Series Property Damage
par(mfrow=c(1,1))
summary(DamageProperty.ts)
DamageProperty.ts <- na.omit(DamageProperty.ts)
DamageProperty.ts <- ts(StormDataNew$DamageProperty, start = c(2009,1), end = c(2014, 12), freq = 12)
DamageProperty2.ts <- ts(StormDataNew$DamageProperty, start = c(2015,1), end = c(2015, 12), freq = 12)
DamageProperty.ts
DamageProperty2.ts
class(DamageProperty.ts)
start(DamageProperty.ts)
end(DamageProperty.ts)
frequency(DamageProperty.ts)
summary(DamageProperty.ts)
plot(DamageProperty.ts)
time(DamageProperty.ts)
abline(reg = lm(DamageProperty.ts ~ time(DamageProperty.ts)))
cycle(DamageProperty.ts)
aggregate(DamageProperty.ts)
aggregate(DamageProperty.ts, FUN=mean)
plot(aggregate(DamageProperty.ts))
boxplot(DamageProperty.ts-cycle(DamageProperty.ts))
DamageProperty.ts.zoom <- window(DamageProperty.ts, start = c(2009, 1), end = c(2015, 12))
plot(DamageProperty.ts.zoom, xlab = "Time", ylab = "Property Damage", ylim = c(0, 3*10^6), bty = "l")
decompose(DamageProperty.ts)
plot(decompose(DamageProperty.ts))


#Property Damage Time Windows
DamageProperty.ts.zoom20092010 <- window(DamageProperty.ts, start = c(2009,1), end = c(2010,12))
plot(DamageProperty.ts.zoom20092010, xlab = "Time", ylab = "Property Damage",
     ylim = c(0, 3*10^6), bty = "l")
decompose(DamageProperty.ts.zoom20092010)
plot(decompose(DamageProperty.ts.zoom20092010))
DamageProperty.ts.zoom20112012 <- window(DamageProperty.ts, start = c(2011,1), end = c(2012,12))
plot(DamageProperty.ts.zoom20112012, xlab = "Time", ylab = "Property Damage",
     ylim = c(0, 3*10^6), bty = "l")
decompose(DamageProperty.ts.zoom20112012)
plot(decompose(DamageProperty.ts.zoom20112012))
DamageProperty.ts.zoom20132014 <- window(DamageProperty.ts, start = c(2013,1), end = c(2014,12))
plot(DamageProperty.ts.zoom20132014, xlab = "Time", ylab = "Property Damage",
     ylim = c(0, 5*10^5), bty = "l")
decompose(DamageProperty.ts.zoom20132014)
plot(decompose(DamageProperty.ts.zoom20132014))
DamageProperty.ts.zoom <- window(DamageProperty.ts, start = c(1998,1), end = c(2000,12))
plot(DamageProperty.ts.zoom, xlab = "Time", ylab = "Property Damage",
     ylim = c(0, 3*10^6), bty = "l")
decompose(DamageProperty.ts.zoom)
plot(decompose(DamageProperty.ts.zoom))
decompose(DamageProperty.ts)
plot(decompose(DamageProperty.ts))
DamageProperty.ts.zoom2 <- window(DamageProperty.ts, start = c(1990,1), end = c(1999,12))
plot(DamageProperty.ts.zoom2, xlab = "Time", ylab = "Property Damage",
     ylim = c(0, 2500000), bty = "l")
decompose(DamageProperty.ts.zoom2)
plot(decompose(DamageProperty.ts.zoom2))
DamageProperty.ts.zoom19801989 <- window(DamageProperty.ts, start = c(1980,1), end = c(1989,12))
plot(DamageProperty.ts.zoom19801989, xlab = "Time", ylab = "Property Damage",
     ylim = c(0, 3*10^6), bty = "l")
decompose(DamageProperty.ts.zoom19801989)
plot(decompose(DamageProperty.ts.zoom19801989))
DamageProperty.ts.zoom19701979 <- window(DamageProperty.ts, start = c(1970,1), end = c(1979,12))
plot(DamageProperty.ts.zoom19701979, xlab = "Time", ylab = "Property Damage",
     ylim = c(0, 3*10^6), bty = "l")
decompose(DamageProperty.ts.zoom19701979)
plot(decompose(DamageProperty.ts.zoom19701979))
DamageProperty.ts.zoom1970 <- window(DamageProperty.ts, start = c(1970,1), end = c(1972,12))
plot(DamageProperty.ts.zoom1970, xlab = "Time", ylab = "Property Damage",
     ylim = c(0, 3*10^6), bty = "l")
decompose(DamageProperty.ts.zoom1970)
plot(decompose(DamageProperty.ts.zoom1970))
nValid <- 24
nTrain <- length(DamageProperty.ts) - nValid
TrainDamageProperty.ts <- window(DamageProperty.ts, start = c(1950,1), end = c(2013,12))
decompose(TrainDamageProperty.ts)
plot(decompose(TrainDamageProperty.ts))
DamageProperty2.lm <- tslm(TrainDamageProperty.ts ~ poly(trend, 2))
DamageProperty2.lm.pred <- forecast(DamageProperty2.lm, h = nValid, level = 0)
plot(DamageProperty2.lm.pred, ylim = c(0,3*10^6), ylab = "Property Damage", xlab = "Time", bty = "l",
     xaxt = "n", xlim = c(2010, 2015), main = "", flty = 2)
axis(1, at = seq(2010, 2015, 1), labels = format(seq(2010, 2015, 1)))
lines(DamageProperty2.lm$fitted, lwd = 2)
valid.ts <- window(DamageProperty.ts, start = c(2010,1), end = c(2015,12))
lines(valid.ts)
accuracy(DamageProperty2.lm.pred$mean, valid.ts)
DamageProperty2.lm.pred
names(DamageProperty2.lm.pred)
DamageProperty2.lm.pred$residuals
valid.ts - DamageProperty2.lm.pred$mean
hist(DamageProperty2.lm.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")


#Naive and Seasonal Naive Forecasts for Property Damage 
fixed.nValid
fixed.nTrain
fixed.nValid <- 12
fixed.nTrain <- length(DamageProperty.ts) - fixed.nValid 
summary(train.ts)
summary(valid.ts)
train.ts <- window(DamageProperty.ts, start = c(2009, 1), end = c(2009, fixed.nTrain)) 
valid.ts <- window(DamageProperty.ts, start = c(2009, fixed.nTrain + 1), end = c(2009, fixed.nTrain + fixed.nValid)) 
train.ts
valid.ts
naive.pred <- naive(train.ts, h = fixed.nValid) 
naive.pred
snaive.pred
snaive.pred <- snaive(train.ts, h = fixed.nValid) 
accuracy(naive.pred, valid.ts) 
accuracy(snaive.pred, valid.ts)
plot(train.ts, ylim = c(0, 3*10^6), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2017), main = "") 
axis(1, at = seq(2009, 2017, 1), labels = format(seq(2009,2017, 1))) 
lines(snaive.pred) 
lines(valid.ts)
naive.pred2 <- naive(train.ts, h = 2*fixed.nValid) 
naive.pred2
snaive.pred2 <- snaive(train.ts, h = 2*fixed.nValid)
snaive.pred2


#Moving Average Forecast for Property Damage
nValid <- 12
length(nValid)
nTrain
nTrain <- length(DamageProperty.ts) - nValid 
train.ts <- window(DamageProperty.ts, start = c(2009, 1), end = c(2009, nTrain)) 
valid.ts <- window(DamageProperty.ts, start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid)) 
ma.trailing <- rollmean(train.ts, k = 12, align = "right") 
last.ma <- tail(ma.trailing, 1) 
ma.trailing.pred <- ts(rep(last.ma, nValid), start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid), freq = 12) 
plot(train.ts, ylim = c(0, 3*10^6), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2017), main = "") 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009,2018, 1))) 
lines(ma.trailing, lwd = 2, col = "blue") 
lines(ma.trailing.pred, lwd = 2, col = "red", lty = 2) 
lines(valid.ts)
ma.trailing.pred
ma.trailing.pred$residuals
accuracy(ma.trailing.pred,valid.ts)


#Differencing and Exponential Smoothing for Property Damage
mean(DamageProperty.ts)
nValid <- 12 
nTrain <- length(DamageProperty.ts) - nValid 
nValid
nTrain
train.ts <- window(DamageProperty.ts, start = c(2009, 1), end = c(2009, nTrain + 1)) 
valid.ts <- window(DamageProperty.ts, start = c(2009, nTrain + 2), end = c(2009, nTrain + 1 + nValid)) 
ses <- ets(train.ts, model = "ZZZ", alpha = 0.2) 
ses.pred <- forecast(ses, h = nValid) 
ses.pred 
plot(ses.pred, ylim = c(0, 3*10^6), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2)
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(ses.pred$fitted, lwd = 2, col = "blue") 
lines(valid.ts)
accuracy(ses.pred$mean, valid.ts)
ses.pred[1:90]
ses.pred
names(ses.pred)
ses.pred$residuals
valid.ts - ses.pred$mean
hist(ses.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")
ses.opt <- ets(train.ts, model = "ZZZ") 
ses.opt.pred <- forecast(ses.opt, h = nValid, level = 0) 
accuracy(ses.pred, valid.ts) 
accuracy(ses.opt.pred, valid.ts) 
ses.opt


#Holt Winters Function with CV for Property Damage
nValid <- 12 
nTrain <- length(DamageProperty.ts) - nValid 
train.ts <- window(DamageProperty.ts, start = c(2009, 1), end = c(2009, nTrain)) 
hw <- ets(train.ts, model = "ZZZ", restrict = FALSE)
hw.pred <- forecast(hw, h = nValid) 
plot(hw.pred, ylim = c(0, 3*10^6), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(hw.pred$fitted, lwd = 2, col = "blue") 
lines(valid.ts)
hw.pred
hw
hw.pred$residuals
hist(hw.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")
hw$states[nrow(hw$states), ]
ets(train.ts)
accuracy(hw.pred, valid.ts)


#Linear Regression Model for Property Damage
DamageProperty.ts
nValid <- 12 
nTrain <- length(DamageProperty.ts) - nValid 
nTrain
train.ts <- window(DamageProperty.ts, start = c(2009, 1), end = c(2009, nTrain)) 
train.ts
trend <- DamageProperty.ts
train.lm <- tslm(train.ts ~ trend)
plot(train.ts, xlab = "Time", ylab = "Property Damage", ylim = c(0, 3*10^6), bty="l")
lines(train.lm$fitted, lwd=2)
summary(train.lm)
train.lm.pred <- forecast(train.lm, h=nValid)
plot(train.lm.pred, ylim = c(0, 3*10^6), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1)))
lines(train.lm.pred$fitted, lwd = 2, col = "blue") 
lines(valid.ts)
train.lm.pred
nValid
valid.ts
train.lm.pred2
accuracy(train.lm.pred, valid.ts)
train.lm.pred$residuals
hist(train.lm.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")


#Additive Seasonality for Property Damage
train.lm.season <- tslm(train.ts ~ season)
summary(train.lm.season)
train.lm.season$data
train.lm.season.pred <- forecast(train.lm.season, h = nValid) 
plot(train.lm.season.pred, ylim = c(0, 3*10^7), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(train.lm.season.pred$fitted, lwd = 2, col = "blue") 
lines(train.lm.season.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(train.lm.season.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(valid.ts)
accuracy(train.lm.season.pred, valid.ts)
train.lm.season.pred$residuals
hist(train.lm.season.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")
train.lm.season.pred 
plot(train.lm.season.pred)
DamageProperty.ts
mean(DamageProperty.ts)


#Multiplicative Seasonality for Property Damage
train.exp.season <- tslm(train.ts ~ season, lambda = 1)
summary(train.exp.season)
train.exp.season$data
train.exp.season.pred <- forecast(train.exp.season, h = nValid, level = 1) 
plot(train.exp.season.pred, ylim = c(0, 3*10^6), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
plot(train.exp.season.pred, ylim = c(0, 3*10^6), ylab = "Residuals", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(train.exp.season.pred$residuals, lwd = 2, col = "yellow")
lines(train.exp.season.pred$fitted, lwd = 2, col = "blue") 
lines(train.exp.season.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(train.exp.season.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(valid.ts)
accuracy(train.exp.season.pred, valid.ts)
train.exp.season.pred


#Trend and Seasonality for Property Damage
nValid <- 12 
nTrain <- length(DamageProperty.ts) - nValid
train.ts <- window(DamageProperty.ts, start = c(2009, 1), end = c(2009, nTrain)) 
trend <- DamageProperty.ts
train.lm.trend.season <- tslm(train.ts ~ trend + season)
summary(train.lm.trend.season)
train.lm.trend.season$data
train.lm.trend.season.pred <- forecast(train.lm.trend.season, h = nValid) 
plot(train.lm.trend.season.pred, ylim = c(0, 3*10^7), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
plot(train.lm.trend.season.pred, ylim = c(0, 3*10^7), ylab = "Residuals", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(train.lm.trend.season.pred$residuals, lwd = 2, col = "yellow")
lines(train.lm.trend.season.pred$fitted, lwd = 2, col = "blue") 
lines(train.lm.trend.season.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(train.lm.trend.season.pred$mean, lwd = 2, col = "blue", lty = 3) 
lines(valid.ts)
accuracy(train.lm.trend.season.pred, valid.ts)
train.lm.trend.season$residuals
hist(train.lm.trend.season.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")


#Autocorrelation for Property Damage
DamageProperty.24.ts <- window(DamageProperty.ts, start = c(2009, 1), end = c(2009, 12)) 
Acf(DamageProperty.24.ts, lag.max = 12, main = "")



#ARIMA for Property Damage
nValid <- 12
nTrain <- length(DamageProperty.ts) - nValid
train.ts <- window(DamageProperty.ts, start = c(2009, 1), end = c(2009, nTrain)) 
valid.ts <- window(DamageProperty.ts, start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid + 1)) 
trend <- train.ts
par(mfrow = c(1,1))
nValid


#regular with no seasonality
train.arimaS <- Arima(train.ts,order=c(1,0,0))
summary(train.arimaS)
train.arimaS
train.arima.predS <- forecast(train.arimaS, h = nValid)
train.arima.predS
plot(train.arima.predS, ylim = c(0, 3*10^7), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(train.arima.predS$fitted, lwd = 2, col = "blue") 
lines(DamageProperty.ts, lwd = 2, col = "red")
accuracy(train.arima.predS,valid.ts)
#default 95% confidence interval
predict(train.arima.predS, valid.ts, interval = "confidence")
?predict


#Model 1
nValid
train.ts
train.arimaS1 <- Arima(train.ts,order=c(1,1,0), seasonal=c(1,1,0))
summary(train.arimaS1)
train.arimaS1
train.arima.predS1 <- forecast(train.arimaS1, h = nValid)
train.arima.predS1
valid.ts
train.arima.confS1 <- predict(train.arima.predS1, valid.ts, interval = "confidence")
train.arima.confS1
summary(train.arima.predS1)
plot(train.arima.confS1, ylim = c(0, 3*10^6), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(train.arima.confS1$fitted, lwd = 2, col = "blue") 
lines(DamageProperty.ts, lwd = 2, col = "red")
accuracy(train.arima.confS1,valid.ts)
train.arima.confS1
train.arima.predS1
DamageProperty2.ts
valid.ts


#Model 2
train.arimaS2 <- Arima(train.ts,order=c(1,1,0), seasonal=c(1,1,1))
summary(train.arimaS2)
train.arimaS2
train.arima.predS2 <- forecast(train.arimaS2, h = nValid)
train.arima.predS2
#default 95% confidence interval
train.arima.confS2 <- predict(train.arima.predS2, valid.ts, interval = "confidence")
train.arima.confS2
plot(train.arima.confS2, ylim = c(0, 3*10^6), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(train.arima.confS2$fitted, lwd = 2, col = "blue") 
lines(DamageProperty.ts, lwd = 2, col = "red")
accuracy(train.arima.confS2,valid.ts)
valid.ts
mean(valid.ts)


#Model 3
mean(train.ts)
mean(valid.ts)
mean(DamageProperty.ts)
mean(DamageProperty2.ts)
train.arimaS3 <- Arima(train.ts,order=c(1,1,0), seasonal=c(1,1,2))
summary(train.arimaS3)
train.arimaS3
train.arima.predS3 <- forecast(train.arimaS3, h = nValid)
train.arima.predS3
train.arima.confS3 <- predict(train.arima.predS3, valid.ts, interval = "confidence")
train.arima.confS3
plot(train.arima.predS3, ylim = c(0, 3*10^6), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(train.arima.predS3$fitted, lwd = 2, col = "blue") 
lines(DamageProperty.ts, lwd = 2, col = "red")
accuracy(train.arima.predS3,valid.ts)
valid.ts
mean(valid.ts)


#Model 4 
train.arimaS4 <- Arima(train.ts,order=c(1,1,0), seasonal=c(1,1,3))
summary(train.arimaS4)
train.arimaS4
train.arima.predS4 <- forecast(train.arimaS4, h = nValid)
train.arima.predS4
sd(train.arima.predS4)
sd(valid.ts)
sd(DamageProperty.ts)
mean(DamageProperty.ts)
plot(train.arima.predS4, ylim = c(0, 3*10^6), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(train.arima.predS4$fitted, lwd = 2, col = "blue") 
lines(DamageProperty.ts, lwd = 2, col = "red")
accuracy(train.arima.predS4, valid.ts)


#Model 5 
train.arimaS5 <- Arima(train.ts,order=c(1,1,0), seasonal=c(0,1,3))
summary(train.arimaS5)
train.arimaS5
train.arima.predS5 <- forecast(train.arimaS5, h = nValid)
train.arima.predS5
sd(train.arima.predS5)
sd(valid.ts)
sd(DamageProperty.ts)
mean(DamageProperty.ts)
plot(train.arima.predS5, ylim = c(0, 3*10^6), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(train.arima.predS5$fitted, lwd = 2, col = "blue") 
lines(DamageProperty.ts, lwd = 2, col = "red")
accuracy(train.arima.predS5, valid.ts)


#Model 6 
train.arimaS6 <- Arima(train.ts,order=c(1,1,0), seasonal=c(0,1,2))                                                       
summary(train.arimaS6)
train.arimaS6
train.arima.predS6 <- forecast(train.arimaS6, h = nValid)
train.arima.predS6
sd(train.arima.predS6)
sd(valid.ts)
sd(DamageProperty.ts)
mean(DamageProperty.ts)
plot(train.arima.predS6, ylim = c(0, 3*10^6), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(train.arima.predS6$fitted, lwd = 2, col = "blue") 
lines(DamageProperty.ts, lwd = 2, col = "red")
accuracy(train.arima.predS6, valid.ts)


#Model 7 
train.arimaS7 <- Arima(train.ts,order=c(1,1,0), seasonal=c(0,1,1))
summary(train.arimaS7)
train.arimaS7
train.arima.predS7 <- forecast(train.arimaS7, h = nValid)
train.arima.predS7
sd(train.arima.predS7)
sd(valid.ts)
sd(DamageProperty.ts)
mean(DamageProperty.ts)
plot(train.arima.predS7, ylim = c(0, 3*10^6), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(train.arima.predS7$fitted, lwd = 2, col = "blue") 
lines(DamageProperty.ts, lwd = 2, col = "red")
accuracy(train.arima.predS7, valid.ts)


#Model 8 
train.arimaS8 <- Arima(train.ts,order=c(1,1,0), seasonal=c(0,1,0))
summary(train.arimaS8)
train.arimaS8
train.arima.predS8 <- forecast(train.arimaS8, h = nValid)
train.arima.predS8
sd(train.arima.predS8)
sd(valid.ts)
sd(DamageProperty.ts)
mean(DamageProperty.ts)
plot(train.arima.predS8, ylim = c(0, 3*10^6), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(train.arima.predS8$fitted, lwd = 2, col = "blue") 
lines(DamageProperty.ts, lwd = 2, col = "red")
accuracy(train.arima.predS8, valid.ts)
valid.ts
train.arimaS8P <- Arima(DamageProperty.ts,order=c(1,1,0), seasonal=c(1,0,2))
summary(train.arimaS8P)
train.arimaS8P
train.arima.predS8P <- forecast(train.arimaS8P, h = nValid)
train.arima.predS8P
sd(train.arima.predS8P)
sd(valid.ts)
sd(DamageProperty.ts)
mean(DamageProperty.ts)
plot(train.arima.predS8P, ylim = c(0, 3*10^5), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(train.arima.predS8P$fitted, lwd = 2, col = "blue") 
lines(DamageProperty.ts, lwd = 2, col = "red")
lines(valid.ts)
lines(DamageProperty2.ts, lwd = 2, col = "green")
accuracy(train.arima.predS8P, DamageProperty2.ts)


#Model 9
train.arimaS9 <- Arima(train.ts,order=c(1,1,0), seasonal=c(1,0,1))
summary(train.arimaS9)
train.arimaS9
train.arima.predS9 <- forecast(train.arimaS9, h = nValid)
train.arima.predS9
sd(train.arima.predS9)
sd(valid.ts)
sd(DamageProperty.ts)
mean(DamageProperty.ts)
plot(train.arima.predS9, ylim = c(0, 3*10^6), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(train.arima.predS9$fitted, lwd = 2, col = "blue") 
lines(DamageProperty.ts, lwd = 2, col = "red")
lines(valid.ts)
accuracy(train.arima.predS9, valid.ts)

train.arimaS9P <- Arima(DamageProperty.ts,order=c(1,1,0), seasonal=c(1,0,1))
summary(train.arimaS9P)
train.arimaS9P
train.arima.predS9P <- forecast(train.arimaS9P, h = nValid)
train.arima.predS9P
sd(train.arima.predS9P)
sd(valid.ts)
sd(DamageProperty.ts)
mean(DamageProperty.ts)
plot(train.arima.predS9P, ylim = c(0, 3*10^5), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2014,2016), main = "", flty = 2) 
axis(1, at = seq(2014, 2016, 1), labels = format(seq(2014, 2016, 1))) 
lines(train.arima.predS9P$fitted, lwd = 2, col = "blue") 
lines(DamageProperty.ts, lwd = 2, col = "red")
lines(valid.ts)
lines(DamageProperty2.ts, lwd = 2, col = "green")
train.arima.predS9P
DamageProperty2.ts
accuracy(train.arima.predS9P, DamageProperty2.ts)


#Model 10
train.arimaS10 <- Arima(train.ts,order=c(1,1,0), seasonal=c(1,0,2))
summary(train.arimaS10)
train.arimaS10
train.arima.predS10 <- forecast(train.arimaS10, h = nValid)
train.arima.predS10
sd(train.arima.predS10)
sd(valid.ts)
sd(DamageProperty.ts)
mean(DamageProperty.ts)
plot(train.arima.predS10, ylim = c(0, 3*10^6), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(train.arima.predS10$fitted, lwd = 2, col = "blue") 
lines(DamageProperty.ts, lwd = 2, col = "red")
lines(valid.ts)
accuracy(train.arima.predS10, valid.ts)


#ARIMA Forecast for Property Damage
nValid
train.ts
train.arimaS1P <- Arima(DamageProperty.ts,order=c(1,1,0), seasonal=c(1,1,0))
summary(train.arimaS1P)
train.arimaS1P
train.arima.predS1P <- forecast(train.arimaS1P, h = nValid, level=95)
train.arima.predS1P
valid.ts
#default 95% confidence interval
train.arima.confS1P <- predict(train.arima.predS1P, valid.ts, interval = "confidence")
train.arima.confS1P
summary(train.arima.predS1P)
plot(train.arima.confS1P, ylim = c(0, 6*10^5), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(train.arima.confS1P$fitted, lwd = 2, col = "blue") 
lines(DamageProperty.ts, lwd = 2, col = "red")
accuracy(train.arima.confS1P,valid.ts)
train.arima.confS1P
train.arima.predS1P




#Neural Network for Property Damage
library(GMDH)
neuralnetwork <- fcast(train.ts, method = "GMDH", input = 12, layer = 2, f.number = 5, level = 95)
neuralnetwork
accuracy(neuralnetwork, valid.ts)
set.seed(201) 
nValid <- 12 
nTrain <- length(DamageProperty.ts) - nValid
nTrain
train.ts <- window(DamageProperty.ts, start = c(2009, 1), end = c(2009, nTrain)) 
train.ts
valid.ts
dim(train.ts)
train.ts
length(train.ts)
length(valid.ts)
PropertyDamage.nnetar <- nnetar(train.ts) 
valid.ts
summary(PropertyDamage.nnetar$model[[1]]) 
PropertyDamage.nnetar.pred <- forecast(PropertyDamage.nnetar, h = nValid) 
PropertyDamage.nnetar.pred
accuracy(PropertyDamage.nnetar.pred, valid.ts) 
plot(train.ts, ylim = c(0, 3*10^6), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), lty = 1) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(PropertyDamage.nnetar.pred$fitted, lwd = 2, col = "blue") 
lines(PropertyDamage.nnetar.pred$mean, lwd = 2, col = "blue", lty = 2) 
lines(valid.ts)
PropertyDamage.nnetar.pred
valid.ts
PropertyDamage2.nnetar <- nnetar(DamageProperty.ts, repeats = 20, p = 11, P = 1, size = 7) 
summary(PropertyDamage2.nnetar$model[[1]]) 
PropertyDamage2.nnetar.pred <- forecast(PropertyDamage2.nnetar, h = nValid) 
accuracy(PropertyDamage.nnetar.pred, DamageProperty.ts) 
plot(DamageProperty.ts, ylim = c(0, 3*10^6), ylab = "Property Damage", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2010,2020), lty = 1) 
axis(1, at = seq(2010, 2020, 1), labels = format(seq(2010, 2020, 1))) 
lines(PropertyDamage2.nnetar.pred$fitted, lwd = 2, col = "blue") 
lines(PropertyDamage2.nnetar.pred$mean, lwd = 2, col = "red", lty = 2) 
lines(valid.ts)
PropertyDamage2.nnetar.pred
PropertyDamage2.nnetar.pred$residuals 
hist(train.res2.arim.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")


#Time Series Tornado Length
TornadoLength.ts <- ts(StormDataNew$TornadoLength, start = c(2009,1), end = c(2014,12), freq = 12)
TornadoLength2.ts <- ts(StormDataNew$TornadoLength, start = c(2015,1), end = c(2015,12), freq = 12)
class(TornadoLength.ts)
start(TornadoLength.ts)
end(TornadoLength.ts)
frequency(TornadoLength.ts)
summary(TornadoLength.ts)
plot(TornadoLength.ts)
time(TornadoLength.ts)
cycle(TornadoLength.ts)
aggregate(TornadoLength.ts)
aggregate(TornadoLength.ts, FUN=mean)
plot(aggregate(TornadoLength.ts))
boxplot(TornadoLength.ts-cycle(TornadoLength.ts))
TornadoLength.ts.zoom <- window(TornadoLength.ts, start = c(1997, 1), end = c(2000, 12))
plot(TornadoLength.ts.zoom, xlab = "Time", ylab = "Tornado Length", ylim = c(0, 100), bty = "l")
decompose(TornadoLength.ts)
plot(decompose(TornadoLength.ts))
TornadoLength.ts.20092010 <- window(TornadoLength.ts, start = c(2009, 1), end = c(2010, 12))
plot(TornadoLength.ts.20092010, xlab = "Time", ylab = "Tornado Length", ylim = c(0, 21), bty = "l")
decompose(TornadoLength.ts.20092010)
plot(decompose(TornadoLength.ts.20092010))
TornadoLength.ts.20112012 <- window(TornadoLength.ts, start = c(2011, 1), end = c(2012, 12))
plot(TornadoLength.ts.20112012, xlab = "Time", ylab = "Tornado Length", ylim = c(0, 21), bty = "l")
decompose(TornadoLength.ts.20112012)
plot(decompose(TornadoLength.ts.20112012))
TornadoLength.ts.20132014 <- window(TornadoLength.ts, start = c(2013, 1), end = c(2014, 12))
plot(TornadoLength.ts.20132014, xlab = "Time", ylab = "Tornado Length", ylim = c(0, 21), bty = "l")
decompose(TornadoLength.ts.20132014)
plot(decompose(TornadoLength.ts.20132014))


#Naive and Seasonal Naive Forecasts for Tornado Length
fixed.nValid <- 12
fixed.nTrain <- length(TornadoLength.ts) - fixed.nValid 
train.ts <- window(TornadoLength.ts, start = c(2009, 1), end = c(2009, fixed.nTrain)) 
valid.ts <- window(TornadoLength.ts, start = c(2009, fixed.nTrain + 1), end = c(2009, fixed.nTrain + fixed.nValid)) 
naive.pred <- naive(train.ts, h = fixed.nValid) 
naive.pred
snaive.pred
snaive.pred <- snaive(train.ts, h = fixed.nValid) 
accuracy(naive.pred, valid.ts) 
accuracy(snaive.pred, valid.ts)
plot(naive.pred, ylim = c(0,25), ylab = "Tornado Length", xlab = "Time", bty = "l",
     xaxt = "n", xlim = c(2009, 2016), main = "", flty = 2)
plot(snaive.pred, ylim = c(0,25), ylab = "Tornado Length", xlab = "Time", bty = "l",
     xaxt = "n", xlim = c(2009, 2016), main = "", flty = 2)
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1)))
lines(naive.pred$fitted, lwd = 2)
lines(snaive.pred$fitted, lwd = 2)
valid.ts <- window(TornadoLength.ts, start = c(2014,1), end = c(2015,12))
lines(valid.ts)
accuracy(snaive.pred, valid.ts)


#Trailing Moving Average Forecast for Tornado Length
nValid <- 12 
nTrain <- length(TornadoLength.ts) - nValid 
train.ts <- window(TornadoLength.ts, start = c(2009, 1), end = c(2009, nTrain)) 
valid.ts <- window(TornadoLength.ts, start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid)) 
ma.trailing <- rollmean(train.ts, k = 12, align = "right") 
last.ma <- tail(ma.trailing, 1) 
ma.trailing.pred <- ts(rep(last.ma, nValid), start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid), freq = 12) 
plot(train.ts, ylim = c(0, 21), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "") 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009,2016, 1))) 
lines(ma.trailing, lwd = 2, col = "blue") 
lines(ma.trailing.pred, lwd = 2, col = "blue", lty = 2) 
lines(valid.ts)
ma.trailing.pred
accuracy(ma.trailing.pred, valid.ts)


#Exponential Smoothing for Tornado Length
nValid <- 12 
nTrain <- length(TornadoLength.ts) - nValid 
train.ts <- window(TornadoLength.ts, start = c(2009, 1), end = c(2009, nTrain + 1)) 
valid.ts <- window(TornadoLength.ts, start = c(2009, nTrain + 2), end = c(2009, nTrain + 1 + nValid)) 
ses <- ets(train.ts, model = "ZZZ", alpha = 0.2) 
ses.pred <- forecast(ses, h = nValid) 
ses.pred 
plot(ses.pred, ylim = c(0, 30), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2)
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(ses.pred$fitted, lwd = 2, col = "blue") 
lines(valid.ts)
accuracy(ses.pred, valid.ts)
ses.pred[1:90]
names(ses.pred)
ses.pred$residuals
valid.ts - ses.pred$mean
hist(ses.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")
accuracy(ses.pred, valid.ts) 


#Holt Winters Function with CV for Tornado Length
nValid <- 12
nTrain <- length(TornadoLength.ts) - nValid 
trainTLhw.ts <- window(TornadoLength.ts, start = c(2009, 1), end = c(2009, nTrain)) 
validTLhw.ts <- window(TornadoLength.ts, start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid)) 
hw <- ets(trainTLhw.ts, model = "ANM", restrict = FALSE)
hw.pred <- forecast(hw, h = nValid) 
plot(hw.pred, ylim = c(0, 25), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(hw.pred$fitted, lwd = 2, col = "blue") 
lines(validTLhw.ts)
hw.pred
hw
hw$states[nrow(hw$states), ]
ets(trainTLhw.ts)
accuracy(hw.pred, validTLhw.ts)
validTLhw.ts

#Linear Regression Model for Tornado Length
trend <- TornadoLength.ts
nValid <- 12
nTrain <- length(TornadoLength.ts) - nValid 
trainTL.ts <- window(TornadoLength.ts, start = c(2009, 1), end = c(2009,nTrain))                                                                    
validTL.ts <- window(TornadoLength.ts, start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid)) 
trainTL.lm <- tslm(trainTL.ts ~ trend)
plot(trainTL.ts, xlab = "Time", ylab = "Tornado Length", ylim = c(0, 30), bty="l")
lines(trainTL.lm$fitted, lwd=2)
summary(trainTL.lm)
trainTL.lm.pred <- forecast(trainTL.lm, h=nValid)
trainTL.lm.pred
plot(trainTL.lm.pred, ylim = c(0, 30), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1)))
lines(trainTL.lm.pred$fitted, lwd = 2, col = "blue") 
lines(validTL.ts)
accuracy(trainTL.lm.pred, valid.ts)
trainTL.lm.pred$residuals
hist(trainTL.lm.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")
trainTL.lm.pred
trainTLF.ts <- window(TornadoLength.ts, start = c(1950, 1), end = c(2015, 12)) 
trainTLF.lm <- tslm(trainTLF.ts ~ trend)
plot(trainTLF.ts, xlab = "Time", ylab = "Tornado Length", ylim = c(0, 100), bty="l")
lines(trainTLF.lm$fitted, lwd=2)
summary(trainTLF.lm)
trainTLF.lm.pred <- forecast(trainTLF.lm, h=nValid, level=0)
plot(trainTLF.lm.pred, ylim = c(0, 100), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2015,2020), main = "", flty = 2) 
axis(1, at = seq(2015, 2020, 1), labels = format(seq(2015, 2020, 1)))
lines(trainTLF.lm.pred$fitted, lwd = 2, col = "blue") 
lines(validTL.ts)
accuracy(trainTLF.lm.pred, validTL.ts)
trainTL.lm.pred$residuals
hist(trainTL.lm.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")
trainTLF.lm.pred


#Exponential Trend for Tornado Length
nValid <- 72 
nTrain <- length(TornadoLength.ts) - nValid 
trainTL.ts <- window(TornadoLength.ts, start = c(1950, 1), end = c(1950, nTrain)) 
validTL.ts <- window(TornadoLength.ts, start = c(1950, nTrain + 1), end = c(1950, nTrain + nValid)) 
trainTL.lm.expo.trend <- tslm(trainTL.ts ~ trend, lambda = 1) 
trainTL.lm.expo.trend.pred <- forecast(trainTL.lm.expo.trend, h = nValid, level = 0) 
accuracy(trainTL.lm.expo.trend.pred, validTL.ts)
trainTL.lm.linear.trend <- tslm(trainTL.ts ~ trend, lambda = 1) 
trainTL.lm.linear.trend.pred <- forecast(trainTL.lm.linear.trend, h = nValid, level = 0) 
accuracy(trainTL.lm.linear.trend.pred, validTL.ts)
plot(trainTL.lm.expo.trend.pred, ylim = c(0, 50), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1950,2015), main = "", flty = 2) 
axis(1, at = seq(1950, 2015, 1), labels = format(seq(1950, 2015, 1))) 
lines(trainTL.lm.expo.trend.pred$fitted, lwd = 2, col = "blue") 
lines(trainTL.lm.linear.trend.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainTL.lm.linear.trend.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validTL.ts)
trainTL.lm.poly.trend <- tslm(trainTL.ts ~ poly(trend, 2, raw = TRUE))
summary(trainTL.lm.poly.trend)
trainTL.lm.poly.trend$data
trainTL.lm.poly.trend.pred <- forecast(trainTL.lm.poly.trend, h = nValid, level = 0) 
plot(trainTL.lm.poly.trend.pred, ylim = c(0, 50), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1950,2015), main = "", flty = 2) 
axis(1, at = seq(1950, 2015, 1), labels = format(seq(1950, 2015, 1))) 
lines(trainTL.lm.poly.trend.pred$fitted, lwd = 2, col = "blue") 
lines(trainTL.lm.poly.trend.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainTL.lm.poly.trend.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validTL.ts)
accuracy(trainTL.lm.poly.trend.pred, valid.ts)


#Seasonality for Tornado Length
nValid <- 12 
nTrain <- length(TornadoLength.ts) - nValid 
trainTL.ts <- window(TornadoLength.ts, start = c(2009, 1), end = c(2009, nTrain)) 
validTL.ts <- window(TornadoLength.ts, start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid)) 
trainTL.lm.season <- tslm(trainTL.ts ~ season)
summary(trainTL.lm.season)
trainTL.lm.season$data
trainTL.lm.season.pred <- forecast(trainTL.lm.season, h = nValid) 
plot(trainTL.lm.season.pred, ylim = c(0, 25), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(trainTL.lm.season.pred$fitted, lwd = 2, col = "blue") 
lines(trainTL.lm.season.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainTL.lm.season.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validTL.ts)
accuracy(trainTL.lm.season.pred, validTL.ts)


#Trend and Seasonality for Tornado Length
nValid <- 12
nTrain <- length(TornadoLength.ts) - nValid 
trainTL.ts <- window(TornadoLength.ts, start = c(2009, 1), end = c(2009, nTrain)) 
validTL.ts <- window(TornadoLength.ts, start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid)) 
trainTL.lm.trend.season <- tslm(trainTL.ts ~ trend + season)
summary(trainTL.lm.trend.season)
trainTL.lm.trend.season$data
trainTL.lm.trend.season.pred <- forecast(trainTL.lm.trend.season, h = nValid) 
plot(trainTL.lm.trend.season.pred, ylim = c(0, 25), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTL.lm.trend.season.pred$fitted, lwd = 2, col = "blue") 
lines(trainTL.lm.trend.season.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainTL.lm.trend.season.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validTL.ts)
accuracy(trainTL.lm.trend.season.pred, validTL.ts)


#Model with Quadratic, Sine and Cosine Terms for Tornado Length
trainTL.quad.sin.cos <- tslm(trainTL.ts ~ trend + I(trend^2) + I(sin(2*pi*trend/12)) + I(cos(2*pi*trend/12)))
summary(trainTL.quad.sin.cos)
trainTL.quad.sin.cos$data
trainTL.quad.sin.cos.pred <- forecast(trainTL.quad.sin.cos, h = nValid) 
plot(trainTL.quad.sin.cos.pred, ylim = c(0, 30), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(trainTL.quad.sin.cos.pred$fitted, lwd = 2, col = "blue") 
lines(trainTL.quad.sin.cos.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainTL.quad.sin.cos.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validTL.ts)
accuracy(trainTL.quad.sin.cos.pred, validTL.ts)


#Autocorrelation for Tornado Length
TornadoLength.24.ts <- window(TornadoLength.ts, start = c(2009, 1), end = c(2014, 12)) 
Acf(TornadoLength.24.ts, lag.max = 12, main = "")


#ARIMA for Tornado Length
nValid <- 12
nTrain <- length(TornadoLength.ts) - nValid 
trainTL.ts <- window(TornadoLength.ts, start = c(2009, 1), end = c(2009, nTrain)) 
validTL.ts <- window(TornadoLength.ts, start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid))


#Model 1 
trainTL.arima <- Arima(trainTL.ts, order = c(1,0,0)) 
plot(trainTL.arima, ylim = c(0, 30), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "") 
axis(1, at = seq(2009, 2015, 1), labels = format(seq(2009, 2015, 1))) 
lines(trainTL.arima$fitted, lwd = 2, col = "blue")
trainTL.arima.pred <- forecast(trainTL.arima, h = nValid) 
plot(trainTL.arima.pred, ylim = c(0,25), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTL.arima.pred$fitted, lwd = 2, col = "blue") 
lines(trainTL.arima.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainTL.arima.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validTL.ts)
accuracy(trainTL.arima.pred, validTL.ts)
trainTL.arima.pred
validTL.ts


#Model 2 
trainTL.arima <- Arima(trainTL.ts, order = c(0,1,3), seasonal = c(0,1,3)) 
plot(trainTL.arima, ylim = c(0, 25), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "") 
axis(1, at = seq(2009, 2015, 1), labels = format(seq(2009, 2015, 1))) 
lines(trainTL.arima$fitted, lwd = 2, col = "blue")
trainTL.arima.pred <- forecast(trainTL.arima, h = nValid) 
plot(trainTL.arima.pred, ylim = c(0,25), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTL.arima.pred$fitted, lwd = 2, col = "blue") 
lines(trainTL.arima.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainTL.arima.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validTL.ts)
accuracy(trainTL.arima.pred, validTL.ts)
trainTL.arima.pred
validTL.ts
mean(TornadoLength.ts)
accuracy(trainTL.arima.pred, validTL.ts)
TornadoLength.ts
mean(TornadoLength.ts)
trainTLF.arima <- Arima(TornadoLength.ts, order = c(0,1,3), seasonal = c(0,1,3)) 
trainTLF.arima.pred <- forecast(trainTLF.arima, h = nValid) 
plot(trainTLF.arima.pred, ylim = c(0,25), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTLF.arima.pred$fitted, lwd = 2, col = "blue") 
lines(trainTLF.arima.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainTLF.arima.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validTL.ts)
accuracy(trainTLF.arima.pred, validTL.ts)
trainTLF.arima.pred
validTL.ts
mean(TornadoLength.ts)
accuracy(trainTL.arima.pred, validTL.ts)


#Model 3 
trainTL3.arima <- Arima(trainTL.ts, order = c(0,1,2), seasonal = c(0,1,2)) 
lines(trainTL3.arima$fitted, lwd = 2, col = "blue")
trainTL3.arima.pred <- forecast(trainTL3.arima, h = nValid) 
plot(trainTL3.arima.pred, ylim = c(0,25), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTL3.arima.pred$fitted, lwd = 2, col = "blue") 
lines(trainTL3.arima.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validTL.ts)
accuracy(trainTL3.arima.pred, validTL.ts)
trainTL3.arima.pred
validTL.ts


#Model 4 
trainTL4.arima <- Arima(trainTL.ts, order = c(1,1,0), seasonal = c(1,1,0)) 
lines(trainTL4.arima$fitted, lwd = 2, col = "blue")
trainTL4.arima.pred <- forecast(trainTL4.arima, h = nValid) 
plot(trainTL4.arima.pred, ylim = c(0,25), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTL4.arima.pred$fitted, lwd = 2, col = "blue") 
lines(trainTL4.arima.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validTL.ts)
accuracy(trainTL4.arima.pred, validTL.ts)
trainTL4.arima.pred
validTL.ts


#Model 5 
trainTL5.arima <- Arima(trainTL.ts, order = c(1,1,0), seasonal = c(1,1,1)) 
lines(trainTL5.arima$fitted, lwd = 2, col = "blue")
trainTL5.arima.pred <- forecast(trainTL5.arima, h = nValid) 
plot(trainTL5.arima.pred, ylim = c(0,25), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTL5.arima.pred$fitted, lwd = 2, col = "blue") 
lines(trainTL5.arima.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validTL.ts)
accuracy(trainTL5.arima.pred, validTL.ts)
trainTL5.arima.pred
validTL.ts


#Model 6 
trainTL6.arima <- Arima(trainTL.ts, order = c(1,1,0), seasonal = c(1,1,2)) 
lines(trainTL6.arima$fitted, lwd = 2, col = "blue")
trainTL6.arima.pred <- forecast(trainTL6.arima, h = nValid) 
plot(trainTL6.arima.pred, ylim = c(0,25), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTL6.arima.pred$fitted, lwd = 2, col = "blue") 
lines(validTL.ts)
accuracy(trainTL6.arima.pred, validTL.ts)
trainTL6.arima.pred
validTL.ts


#Model 7 
trainTL7.arima <- Arima(trainTL.ts, order = c(1,1,2), seasonal = c(1,1,3)) 
lines(trainTL7.arima$fitted, lwd = 2, col = "blue")
trainTL7.arima.pred <- forecast(trainTL7.arima, h = nValid) 
plot(trainTL7.arima.pred, ylim = c(0,25), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTL7.arima.pred$fitted, lwd = 2, col = "blue") 
lines(validTL.ts)
accuracy(trainTL7.arima.pred, validTL.ts)
trainTL7.arima.pred
validTL.ts


#Model 8 
trainTL8.arima <- Arima(trainTL.ts, order = c(1,1,2), seasonal = c(1,1,1)) 
lines(trainTL8.arima$fitted, lwd = 2, col = "blue")
trainTL8.arima.pred <- forecast(trainTL8.arima, h = nValid) 
plot(trainTL8.arima.pred, ylim = c(0,25), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTL8.arima.pred$fitted, lwd = 2, col = "blue") 
lines(validTL.ts)
accuracy(trainTL8.arima.pred, validTL.ts)
trainTL8.arima.pred
validTL.ts


#Model 9 
trainTL9.arima <- Arima(trainTL.ts, order = c(1,1,2), seasonal = c(1,0,0)) 
lines(trainTL9.arima$fitted, lwd = 2, col = "blue")
trainTL9.arima.pred <- forecast(trainTL9.arima, h = nValid) 
plot(trainTL9.arima.pred, ylim = c(0,25), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTL9.arima.pred$fitted, lwd = 2, col = "blue") 
lines(validTL.ts)
accuracy(trainTL9.arima.pred, validTL.ts)
trainTL9.arima.pred
validTL.ts


#Model 10
trainTL10a.arima <- Arima(trainTL.ts, order = c(1,1,3), seasonal = c(1,0,0)) 
lines(trainTL10a.arima$fitted, lwd = 2, col = "blue")
trainTL10a.arima.pred <- forecast(trainTL10a.arima, h = nValid) 
plot(trainTL10a.arima.pred, ylim = c(0,25), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTL10a.arima.pred$fitted, lwd = 2, col = "blue") 
lines(validTL.ts)
accuracy(trainTL10a.arima.pred, validTL.ts)
trainTL10.arima.pred
validTL.ts


#Model 11 
trainTL10.arima <- Arima(trainTL.ts, order = c(1,0,2), seasonal = c(1,0,0)) 
lines(trainTL10.arima$fitted, lwd = 2, col = "blue")
trainTL10.arima.pred <- forecast(trainTL10.arima, h = nValid) 
plot(trainTL10.arima.pred, ylim = c(0,25), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTL10.arima.pred$fitted, lwd = 2, col = "blue") 
lines(validTL.ts)
accuracy(trainTL10.arima.pred, validTL.ts)
trainTL10.arima.pred
validTL.ts
trainTL10F.arima <- Arima(TornadoLength.ts, order = c(1,0,2), seasonal = c(1,0,0)) 
lines(trainTL10F.arima$fitted, lwd = 2, col = "blue")
trainTL10F.arima.pred <- forecast(trainTL10F.arima, h = nValid) 
plot(trainTL10F.arima.pred, ylim = c(0,25), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTL10F.arima.pred$fitted, lwd = 2, col = "blue") 
lines(TornadoLength2.ts, lwd = 2, col = "green")
accuracy(trainTL10F.arima.pred, TornadoLength2.ts)
trainTL10F.arima.pred
TornadoLength2.ts


#Model 12 
trainTL12.arima <- Arima(trainTL.ts, order = c(1,0,2), seasonal = c(1,0,2)) 
lines(trainTL12.arima$fitted, lwd = 2, col = "blue")
trainTL12.arima.pred <- forecast(trainTL12.arima, h = nValid) 
plot(trainTL12.arima.pred, ylim = c(0,25), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTL12.arima.pred$fitted, lwd = 2, col = "blue") 
lines(validTL.ts)
accuracy(trainTL12.arima.pred, validTL.ts)
trainTL12.arima.pred
validTL.ts


#Model 13 
trainTL13.arima <- Arima(trainTL.ts, order = c(1,1,0), seasonal = c(1,0,1)) 
lines(trainTL13.arima$fitted, lwd = 2, col = "blue")
trainTL13.arima.pred <- forecast(trainTL13.arima, h = nValid) 
plot(trainTL13.arima.pred, ylim = c(0,25), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTL13.arima.pred$fitted, lwd = 2, col = "blue") 
lines(validTL.ts)
accuracy(trainTL13.arima.pred, validTL.ts)
trainTL13.arima.pred
validTL.ts


#Model 14 
trainTL14.arima <- Arima(trainTL.ts, order = c(1,1,0), seasonal = c(1,0,2)) 
lines(trainTL14.arima$fitted, lwd = 2, col = "blue")
trainTL14.arima.pred <- forecast(trainTL14.arima, h = nValid) 
plot(trainTL14.arima.pred, ylim = c(0,25), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTL14.arima.pred$fitted, lwd = 2, col = "blue") 
lines(validTL.ts)
accuracy(trainTL14.arima.pred, validTL.ts)
trainTL14.arima.pred
validTL.ts


#Neural Network for forecasting Tornado Length
set.seed(201) 
length(trainTL.ts)
nValid <- 12 
nTrain <- 60 
trainTL.ts <- window(TornadoLength.ts, start = c(2009, 1), end = c(2009, nTrain)) 
validTL.ts <- window(TornadoLength.ts, start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid)) 
TornadoLength.nnetar <- nnetar(trainTL.ts, repeats = 20, p = 11, P = 1, size = 7) 
summary(TornadoLength.nnetar$model[[1]]) 
TornadoLength.nnetar.pred <- forecast(TornadoLength.nnetar, h = nValid)  
plot(trainTL.ts, ylim = c(0, 25), ylab = "Tornado Length", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), lty = 1) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(TornadoLength.nnetar.pred$fitted, lwd = 2, col = "blue") 
lines(TornadoLength.nnetar.pred$mean, lwd = 2, col = "blue", lty = 2) 
lines(validTL.ts)
TornadoLength.nnetar.pred
accuracy(TornadoLength.nnetar.pred, validTL.ts)
TornadoLength.nnetar.pred$residuals
hist(TornadoLength.nnetar.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")


#Time Series Tornado Width
TornadoWidth.ts <- ts(StormDataNew$TornadoWidth, start = c(2009,1), end = c(2014,12), freq = 12)
TornadoWidth2.ts <- ts(StormDataNew$TornadoWidth, start = c(2015,1), end = c(2015,12), freq = 12)
class(TornadoWidth.ts)
start(TornadoWidth.ts)
end(TornadoWidth.ts)
frequency(TornadoWidth.ts)
summary(TornadoWidth.ts)
plot(TornadoWidth.ts)
time(TornadoWidth.ts)
cycle(TornadoWidth.ts)
aggregate(TornadoWidth.ts)
aggregate(TornadoWidth.ts, FUN=mean)
plot(aggregate(TornadoWidth.ts))
boxplot(TornadoWidth.ts-cycle(TornadoWidth.ts))
TornadoWidth.ts.zoom <- window(TornadoWidth.ts, start = c(1997, 1), end = c(2000, 12))
plot(TornadoWidth.ts.zoom, xlab = "Time", ylab = "Tornado Width", ylim = c(0, 1500), bty = "l")
decompose(TornadoWidth.ts)
plot(decompose(TornadoWidth.ts))
TornadoWidth.ts.20092010 <- window(TornadoWidth.ts, start = c(2009, 1), end = c(2010, 12))
plot(TornadoWidth.ts.20092010, xlab = "Time", ylab = "Tornado Width", ylim = c(0, 1000), bty = "l")
decompose(TornadoWidth.ts.20092010)
plot(decompose(TornadoWidth.ts.20092010))
TornadoWidth.ts.20112012 <- window(TornadoWidth.ts, start = c(2011, 1), end = c(2012, 12))
plot(TornadoWidth.ts.20112012, xlab = "Time", ylab = "Tornado Width", ylim = c(0, 1000), bty = "l")
decompose(TornadoWidth.ts.20112012)
plot(decompose(TornadoWidth.ts.20112012))
TornadoWidth.ts.20132014 <- window(TornadoWidth.ts, start = c(2013, 1), end = c(2014, 12))
plot(TornadoWidth.ts.20132014, xlab = "Time", ylab = "Tornado Width", ylim = c(0, 1500), bty = "l")
decompose(TornadoWidth.ts.20132014)
plot(decompose(TornadoWidth.ts.20132014))
TornadoWidth.ts.19801989 <- window(TornadoWidth.ts, start = c(1980, 1), end = c(1989, 12))
plot(TornadoWidth.ts.19801989, xlab = "Time", ylab = "Tornado Width", ylim = c(0, 2000), bty = "l")
decompose(TornadoWidth.ts.19801989)
plot(decompose(TornadoWidth.ts.19801989))
TornadoWidth.ts.19901999 <- window(TornadoWidth.ts, start = c(1990, 1), end = c(1999, 12))
plot(TornadoWidth.ts.19901999, xlab = "Time", ylab = "Tornado Width", ylim = c(0, 1500), bty = "l")
decompose(TornadoWidth.ts.19901999)
plot(decompose(TornadoWidth.ts.19901999))
TornadoWidth.ts.20002015 <- window(TornadoWidth.ts, start = c(2000, 1), end = c(2015, 12))
plot(TornadoWidth.ts.20002015, xlab = "Time", ylab = "Tornado Width", ylim = c(0, 2000), bty = "l")
decompose(TornadoWidth.ts.20002015)
plot(decompose(TornadoWidth.ts.20002015))


#Quadratic Model for Tornado Width
nValid <- 72
nTrain <- length(TornadoWidth.ts) - nValid
TrainTornadoWidth.ts <- window(TornadoWidth.ts, start = c(1950,1), end = c(2010,12))
TornadoWidth.lm <- tslm(TrainTornadoWidth.ts ~ poly(trend, 2))
TornadoWidth.lm.pred <- forecast(TornadoWidth.lm, h = nValid, level = 0)
plot(TornadoWidth.lm.pred , ylim = c(0,1500), ylab = "Tornado Width", xlab = "Time", bty = "l",
     xaxt = "n", xlim = c(1950, 2015), main = "", flty = 2)
axis(1, at = seq(1950, 2015, 1), labels = format(seq(1950, 2015, 1)))
lines(TornadoWidth.lm$fitted, lwd = 2)
validTW.ts <- window(TornadoWidth.ts, start = c(2011,1), end = c(2015,12))
lines(validTW.ts)
accuracy(TornadoWidth.lm.pred$mean, validTW.ts)
TornadoWidth.lm.pred
names(TornadoWidth.lm.pred)
TornadoWidth.lm.pred$residuals
validTW.ts - TornadoWidth.lm.pred$mean
hist(TornadoWidth.lm.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")


#Naive and Seasonal Naive Forecasts for Tornado Width
mean(TornadoWidth.ts)
fixed.nValid <- 12
fixed.nTrain <- length(TornadoWidth.ts) - fixed.nValid 
trainTW.ts <- window(TornadoWidth.ts, start = c(2009, 1), end = c(2009, fixed.nTrain)) 
validTW.ts <- window(TornadoWidth.ts, start = c(2009, fixed.nTrain + 1), end = c(2009, fixed.nTrain + fixed.nValid)) 
naiveTW.pred <- naive(trainTW.ts, h = fixed.nValid) 
naiveTW.pred
snaiveTW.pred <- snaive(trainTW.ts, h = fixed.nValid) 
snaiveTW.pred
accuracy(naiveTW.pred, validTW.ts) 
accuracy(snaiveTW.pred, validTW.ts)
plot(naiveTW.pred, ylim = c(0,1500), ylab = "Tornado Width", xlab = "Time", bty = "l",
     xaxt = "n", xlim = c(2009, 2016), main = "", flty = 2)
plot(snaiveTW.pred, ylim = c(0,1500), ylab = "Tornado Width", xlab = "Time", bty = "l",
     xaxt = "n", xlim = c(2009, 2018), main = "", flty = 2)
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1)))
lines(naiveTW.pred$fitted, lwd = 2)
lines(snaiveTW.pred$fitted, lwd = 2)
validTW.ts <- window(TornadoWidth.ts, start = c(2014,1), end = c(2015,12))
lines(validTW.ts)
accuracy(snaiveTW.pred, validTW.ts)


#Moving Average Forecast for Tornado Width
nValid <- 12
nTrain <- length(TornadoWidth.ts) - nValid 
trainTW.ts <- window(TornadoWidth.ts, start = c(2009, 1), end = c(2009, nTrain)) 
validTW.ts <- window(TornadoWidth.ts, start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid)) 
ma.trailing <- rollmean(trainTW.ts, k = 12, align = "right") 
last.ma <- tail(ma.trailing, 1) 
ma.trailing.pred <- ts(rep(last.ma, nValid), start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid), freq = 12) 
plot(trainTW.ts, ylim = c(0, 1500), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "") 
axis(1, at = seq(2009, 2015, 1), labels = format(seq(2009,2015, 1))) 
lines(ma.trailing, lwd = 2, col = "blue") 
lines(ma.trailing.pred, lwd = 2, col = "red", lty = 2) 
lines(validTW.ts)
ma.trailing.pred
accuracy(ma.trailing.pred, validTW.ts)


#Exponential Smoothing for Tornado Width
mean(TornadoWidth.ts)
summary(TornadoWidth.ts)
nValid <- 12
nTrain <- length(TornadoWidth.ts) - nValid 
trainTW.ts <- window(TornadoWidth.ts, start = c(2009, 1), end = c(2009, nTrain + 1)) 
validTW.ts <- window(TornadoWidth.ts, start = c(2009, nTrain + 2), end = c(2009, nTrain + 1 + nValid)) 
ses <- ets(trainTW.ts, model = "ZZZ", alpha = 0.2) 
ses.pred <- forecast(ses, h = nValid) 
ses.pred 
plot(ses.pred, ylim = c(0, 2000), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2)
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(ses.pred$fitted, lwd = 2, col = "blue") 
lines(validTW.ts)
accuracy(ses.pred, validTW.ts)
ses.pred[1:90]
names(ses.pred)
ses.pred$residuals
validTW.ts - ses.pred$mean
hist(ses.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")
ses.opt <- ets(trainTW.ts, model = "ANN") 
ses.opt.pred <- forecast(ses.opt, h = nValid, level = 0) 
accuracy(ses.pred, validTW.ts)
accuracy(ses.opt.pred, validTW.ts)
ses.opt
sesF <- ets(TornadoWidth.ts, model = "ZZZ", alpha = 0.2) 
ses.predF <- forecast(ses, h = nValid, level = 95) 
ses.predF 
plot(ses.predF, ylim = c(0, 2000), ylab = "Tornado Width (Twice-Differenced)", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2015,2018), main = "", flty = 2)
axis(1, at = seq(2015, 2018, 1), labels = format(seq(2015, 2018, 1))) 
lines(ses.predF$fitted, lwd = 2, col = "blue") 
lines(validTW.ts)


#Holt Winters Function with CV for Tornado Width
nValid <- 12 
nTrain <- length(TornadoWidth.ts) - nValid 
trainTW.ts <- window(TornadoWidth.ts, start = c(2009, 1), end = c(2009, nTrain)) 
validTW.ts <- window(TornadoWidth.ts, start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid)) 
hw <- ets(trainTW.ts, model = "ZZZ", restrict = FALSE)
hw.pred <- forecast(hw, h = nValid) 
plot(hw.pred, ylim = c(0, 2000), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(hw.pred$fitted, lwd = 2, col = "blue") 
lines(validTW.ts)
hw.pred
hw
hw$states[nrow(hw$states), ]
ets(trainTW.ts)
accuracy(hw.pred, validTW.ts)


#Linear Regression Model for Tornado Width
trend <- TornadoWidth.ts
nValid <- 12 
nTrain <- length(TornadoWidth.ts) - nValid 
trainTW.ts <- window(TornadoWidth.ts, start = c(2009, 1), end = c(2009, nTrain)) 
validTW.ts <- window(TornadoWidth.ts, start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid)) 
trainTW.lm <- tslm(trainTW.ts ~ trend)
plot(trainTW.ts, xlab = "Time", ylab = "Tornado Width", ylim = c(0, 2000), bty="l")
lines(trainTW.lm$fitted, lwd=2)
summary(trainTW.lm)
trainTW.lm.pred <- forecast(trainTW.lm, h=nValid)
plot(trainTW.lm.pred, ylim = c(0, 2000), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1)))
lines(trainTW.lm.pred$fitted, lwd = 2, col = "blue") 
lines(validTW.ts)
accuracy(trainTW.lm.pred, validTW.ts)
trainTW.lm.pred2 <- forecast(trainTW.lm, h=2*nValid, level=0)
plot(trainTW.lm.pred2, ylim = c(0, 1500), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1950,2021), main = "", flty = 2) 
axis(1, at = seq(1950, 2021, 1), labels = format(seq(1950, 2021, 1)))
lines(trainTW.lm.pred2$fitted, lwd = 2, col = "blue") 
lines(validTW.ts)
trainTW.lm.pred2
accuracy(trainTW.lm.pred2, validTW.ts)


#Exponential Trend for Tornado Width
nValid <- 24 
nTrain <- length(TornadoWidth.ts) - nValid 
trainTW.ts <- window(TornadoWidth.ts, start = c(1950, 1), end = c(1950, nTrain)) 
validTW.ts <- window(TornadoWidth.ts, start = c(1950, nTrain + 1), end = c(1950, nTrain + nValid)) 
trainTW.lm.expo.trend <- tslm(trainTW.ts ~ trend, lambda = 1) 
trainTW.lm.expo.trend.pred <- forecast(trainTW.lm.expo.trend, h = nValid, level = 0) 
accuracy(trainTW.lm.expo.trend.pred, validTW.ts)
trainTW.lm.linear.trend <- tslm(trainTW.ts ~ trend, lambda = 1) 
trainTW.lm.linear.trend.pred <- forecast(trainTW.lm.linear.trend, h = nValid, level = 0) 
accuracy(trainTW.lm.linear.trend.pred, validTW.ts)
plot(trainTW.lm.expo.trend.pred, ylim = c(0, 2000), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1950,2015), main = "", flty = 2) 
axis(1, at = seq(1950, 2015, 1), labels = format(seq(1950, 2015, 1))) 
lines(trainTW.lm.expo.trend.pred$fitted, lwd = 2, col = "blue") 
lines(trainTW.lm.linear.trend.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainTW.lm.expo.trend.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validTW.ts)
trainTW.lm.poly.trend <- tslm(trainTW.ts ~ poly(trend, 3, raw = TRUE))
summary(trainTW.lm.poly.trend)
trainTW.lm.poly.trend$data
trainTW.lm.poly.trend.pred <- forecast(trainTW.lm.poly.trend, h = nValid, level = 0) 
plot(trainTW.lm.poly.trend.pred, ylim = c(0, 1500), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1950,2015), main = "", flty = 2) 
axis(1, at = seq(1950, 2015, 1), labels = format(seq(1950, 2015, 1))) 
lines(trainTW.lm.poly.trend.pred$fitted, lwd = 2, col = "blue") 
lines(trainTW.lm.poly.trend.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainTW.lm.poly.trend.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validTW.ts)
accuracy(trainTW.lm.poly.trend.pred, valid.ts)
mean(TornadoWidth.ts)


#Seasonality for Tornado Width
trainTW.lm.season <- tslm(trainTW.ts ~ season)
summary(trainTW.lm.season)
trainTW.lm.season$data
trainTW.lm.season.pred <- forecast(trainTW.lm.season, h = nValid) 
plot(trainTW.lm.season.pred, ylim = c(0, 2000), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTW.lm.season.pred$fitted, lwd = 2, col = "blue") 
lines(trainTW.lm.season.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainTW.lm.season.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validTW.ts)
accuracy(trainTW.lm.season.pred, validTW.ts)
trainTW.lm.season.pred


#Trend and Seasonality for Tornado Width
trainTW.lm.trend.season <- tslm(trainTW.ts ~ trend + season)
summary(trainTW.lm.trend.season)
trainTW.lm.trend.season$data
trainTW.lm.trend.season.pred <- forecast(trainTW.lm.trend.season, h = nValid) 
plot(trainTW.lm.trend.season.pred, ylim = c(0, 1000), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTW.lm.trend.season.pred$fitted, lwd = 2, col = "blue") 
lines(trainTW.lm.trend.season.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainTW.lm.trend.season.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validTW.ts)
accuracy(trainTW.lm.trend.season.pred, validTW.ts)
trainTW.lm.trend.season.pred
valid.ts
trainTW.lm.trend.season1 <- tslm(trainTW.ts ~ trend + season)
summary(trainTW.lm.trend.season1)
trainTW.lm.trend.season1$data
trainTW.lm.trend.season.pred1<- forecast(trainTW.lm.trend.season1, h = nValid, level = 0) 
plot(trainTW.lm.trend.season.pred1, ylim = c(0, 2000), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1950,2015), main = "", flty = 2) 
axis(1, at = seq(1950, 2015, 1), labels = format(seq(1950, 2015, 1))) 
lines(trainTW.lm.trend.season.pred1$fitted, lwd = 2, col = "blue") 
lines(trainTW.lm.trend.season.pred1$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainTW.lm.trend.season.pred1$mean, lwd = 2, col = "black", lty = 3) 
lines(validTW.ts)
accuracy(trainTW.lm.trend.season.pred1, validTW.ts)


#Model with Quadratic, Sine and Cosine Terms for Tornado Width
trainTW.quad.sin.cos <- tslm(trainTW.ts ~ trend + I(trend^2) + I(sin(2*pi*trend/12)) + I(cos(2*pi*trend/12)))
summary(trainTW.quad.sin.cos)
trainTW.quad.sin.cos$data
trainTW.quad.sin.cos.pred <- forecast(trainTW.quad.sin.cos, h = nValid, level = 0) 
plot(trainTW.quad.sin.cos.pred, ylim = c(0, 1500), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1950,2015), main = "", flty = 2) 
axis(1, at = seq(1950, 2015, 1), labels = format(seq(1950, 2015, 1))) 
lines(trainTW.quad.sin.cos.pred$fitted, lwd = 2, col = "blue") 
lines(trainTW.quad.sin.cos.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainTW.quad.sin.cos.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validTW.ts)
accuracy(trainTW.quad.sin.cos.pred, validTL.ts)


#Autocorrelation for Tornado Width
TornadoWidth.24.ts <- window(TornadoWidth.ts, start = c(2009, 1), end = c(2009, 24)) 
Acf(TornadoWidth.24.ts, lag.max = 12, main = "")

#ARIMA for Tornado Width
trend <- TornadoWidth.ts
nValid <- 12 
nTrain <- length(TornadoWidth.ts) - nValid 
trainTW.ts <- window(TornadoWidth.ts, start = c(2009, 1), end = c(2009, nTrain)) 
validTW.ts <- window(TornadoWidth.ts, start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid)) 
?Arima()
trainTW.lm.trend.season <- tslm(trainTW.ts ~ trend + season) 
trainTW.arima <- Arima(trainTW.ts, order = c(1,0,0)) 
summary(trainTW.arima)
trainTW.arima.pred <- forecast(trainTW.arima, h = nValid) 
plot(trainTW.arima.pred, ylim = c(0,1500), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTW.arima.pred$fitted, lwd = 2, col = "blue") 
lines(trainTW.arima.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validTW.ts)
accuracy(trainTW.arima.pred, validTW.ts)
validTW.ts
trainTW.arima.pred


#model 1 
trainTW1.arima <- Arima(trainTW.ts, order = c(1,0,0), seasonal = c(0,1,2)) 
summary(trainTW1.arima)
trainTW1.arima.pred <- forecast(trainTW1.arima, h = nValid) 
plot(trainTW1.arima.pred, ylim = c(0,2000), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTW1.arima.pred$fitted, lwd = 2, col = "blue") 
lines(trainTW1.arima.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validTW.ts)
accuracy(trainTW1.arima.pred, validTW.ts)
validTW.ts
trainTW1.arima.pred


#Model 2 
trainTW2.arima <- Arima(trainTW.ts, order = c(1,0,0), seasonal = c(0,1,3)) 
summary(trainTW2.arima)
trainTW2.arima.pred <- forecast(trainTW2.arima, h = nValid) 
plot(trainTW2.arima.pred, ylim = c(0,1500), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTW2.arima.pred$fitted, lwd = 2, col = "blue") 
mean(TornadoWidth.ts)
lines(trainTW2.arima.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validTW.ts)
accuracy(trainTW2.arima.pred, validTW.ts)
validTW.ts
trainTW2.arima.pred


#Model 3 
trainTW.arima2a <- Arima(trainTW.ts, order = c(1,0,3), seasonal = c(1,0,3)) 
summary(trainTW.arima2a)
trainTW.arima.pred2a <- forecast(trainTW.arima2a, h = nValid) 
plot(trainTW.arima.pred2a, ylim = c(0,1000), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(trainTW.arima.pred2a$fitted, lwd = 2, col = "blue") 
lines(trainTW.arima.pred2a$mean, lwd = 2, col = "black", lty = 3) 
lines(validTW.ts)
accuracy(trainTW.arima.pred2a, validTW.ts)
validTW.ts
trainTW.arima.pred2a
trainTW.arima2F <- Arima(TornadoWidth.ts, order = c(1,0,3), seasonal = c(1,0,3)) 
summary(trainTW.arima2F)
trainTW.arima.pred2F <- forecast(trainTW.arima2F, h = nValid, level=99) 
plot(trainTW.arima.pred2F, ylim = c(0,1000), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2014,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(trainTW.arima.pred2F$fitted, lwd = 2, col = "blue") 
lines(trainTW.arima.pred2a$mean, lwd = 2, col = "black", lty = 3) 
lines(TornadoWidth2.ts)
accuracy(trainTW.arima.pred2F, TornadoWidth2.ts)
trainTW.arima.pred2F
TornadoWidth2.ts
mean(trainTW.arima.pred2F)
trainTW.arimaF1 <- Arima(TornadoWidth.ts, order = c(1,0,3), seasonal = c(1,0,3)) 
summary(trainTW.arimaF1)
plot(trainTW.arimaF1.pred, ylim = c(0, 2000), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "") 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainTW.arimaF$fitted, lwd = 2, col = "blue")
trainTW.arimaF1.pred <- forecast(trainTW.arimaF1, h = nValid) 
trainTW.arimaF1.pred


#Model 4 
trainTW.arima2b <- Arima(trainTW.ts, order = c(1,0,3), seasonal = c(1,0,0)) 
summary(trainTW.arima2b)
trainTW.arima.pred2b <- forecast(trainTW.arima2b, h = nValid) 
plot(trainTW.arima.pred2b, ylim = c(0,1000), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(trainTW.arima.pred2b$fitted, lwd = 2, col = "blue") 
lines(trainTW.arima.pred2b$mean, lwd = 2, col = "black", lty = 3) 
lines(validTW.ts)
accuracy(trainTW.arima.pred2b, validTW.ts)
validTW.ts
trainTW.arima.pred2b


#Model 5
trainTW.arima3a <- Arima(trainTW.ts, order = c(1,0,2), seasonal = c(1,0,2)) 
summary(trainTW.arima3a)
trainTW.arima.pred3a <- forecast(trainTW.arima3a, h = nValid) 
plot(trainTW.arima.pred3a, ylim = c(0,1000), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(trainTW.arima.pred3a$fitted, lwd = 2, col = "blue") 
lines(trainTW.arima.pred3a$mean, lwd = 2, col = "black", lty = 3) 
lines(validTW.ts)
accuracy(trainTW.arima.pred3a, validTW.ts)
validTW.ts
trainTW.arima.pred2a


#Model 6
trainTW.arima3b <- Arima(trainTW.ts, order = c(1,0,2), seasonal = c(1,0,0)) 
summary(trainTW.arima3b)
trainTW.arima.pred3b <- forecast(trainTW.arima3b, h = nValid) 
plot(trainTW.arima.pred3b, ylim = c(0,1000), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(trainTW.arima.pred3b$fitted, lwd = 2, col = "blue") 
lines(trainTW.arima.pred3b$mean, lwd = 2, col = "black", lty = 3) 
lines(validTW.ts)
accuracy(trainTW.arima.pred3b, validTW.ts)
validTW.ts
trainTW.arima.pred3b


#Model 7
trainTW.arima8 <- Arima(trainTW.ts, order = c(1,0,1), seasonal = c(1,0,0)) 
summary(trainTW.arima8)
trainTW.arima.pred8 <- forecast(trainTW.arima8, h = nValid) 
plot(trainTW.arima.pred8, ylim = c(0,1000), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(trainTW.arima.pred8$fitted, lwd = 2, col = "blue") 
lines(trainTW.arima.pred8$mean, lwd = 2, col = "black", lty = 3) 
lines(validTW.ts)
accuracy(trainTW.arima.pred8, validTW.ts)
validTW.ts
trainTW.arima.pred3b


#Neural Network for Tornado Width
set.seed(201) 
length(trainTW.ts)
TornadoWidth.nnetar <- nnetar(trainTW.ts, repeats = 20, p = 11, P = 1, size = 7) 
summary(TornadoWidth.nnetar$model[[1]]) 
TornadoWidth.nnetar.pred <- forecast(TornadoWidth.nnetar, h = nValid, level = c(80,95))  
plot(trainTW.ts, ylim = c(0, 1500), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), lty = 1) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(TornadoWidth.nnetar.pred$fitted, lwd = 2, col = "blue") 
lines(TornadoWidth.nnetar.pred$mean, lwd = 2, col = "black", lty = 2)
lines(TornadoWidth.nnetar.pred$level, lwd = 2, col = "yellow", lty = 2)
lines(validTW.ts)
TornadoWidth.nnetar.pred
set.seed(201) 
length(trainTW.ts)
TornadoWidth.nnetarF <- nnetar(TornadoWidth.ts, repeats = 20, p = 11, P = 1, size = 7) 
summary(TornadoWidth.nnetarF$model[[1]]) 
TornadoWidth.nnetar.predF <- forecast(TornadoWidth.nnetarF, h = nValid, level = c(80,95))  
plot(TornadoWidth.ts, ylim = c(0, 1000), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2014,2018), lty = 1) 
axis(1, at = seq(2014, 2018, 1), labels = format(seq(2014, 2018, 1))) 
lines(TornadoWidth.nnetar.predF$fitted, lwd = 2, col = "blue") 
lines(TornadoWidth.nnetar.predF$mean, lwd = 2, col = "yellow", lty = 2)
lines(TornadoWidth.nnetar.predF$level, lwd = 2, col = "yellow", lty = 2)
lines(validTW.ts)
TornadoWidth.nnetar.predF
validTW.ts
accuracy(TornadoWidth.nnetar.pred, validTW.ts)
TornadoWidth.nnetar.pred
validTW.ts
plot(TornadoWidth.nnetar.pred, plot.conf=TRUE)


#Time Series Beginning Latitude
BeginningLatitude.ts <- ts(StormDataNew$BeginningLatitude, start = c(2009,1), end = c(2014,12), freq = 12)
BeginningLatitude2.ts <- ts(StormDataNew$BeginningLatitude, start = c(2015,1), end = c(2015,12), freq = 12)
class(BeginningLatitude.ts)
start(BeginningLatitude.ts)
end(BeginningLatitude.ts)
frequency(BeginningLatitude.ts)
summary(BeginningLatitude.ts)
plot(BeginningLatitude.ts)
time(BeginningLatitude.ts)
cycle(BeginningLatitude.ts)
aggregate(BeginningLatitude.ts)
aggregate(BeginningLatitude.ts, FUN=mean)
plot(aggregate(BeginningLatitude.ts))
boxplot(BeginningLatitude.ts-cycle(BeginningLatitude.ts))
BeginningLatitude.ts.zoom <- window(BeginningLatitude.ts, start = c(1997, 1), end = c(2000, 12))
plot(BeginningLatitude.ts.zoom, xlab = "Time", ylab = "Beginning Latitude", ylim = c(0, 100), bty = "l")
decompose(BeginningLatitude.ts)
plot(decompose(BeginningLatitude.ts))
BeginningLatitude.ts.20092010 <- window(BeginningLatitude.ts, start = c(2009, 1), end = c(2010, 12))
plot(BeginningLatitude.ts.20092010, xlab = "Time", ylab = "Beginning Latitude", ylim = c(33, 37), bty = "l")
decompose(BeginningLatitude.ts.20092010)
plot(decompose(BeginningLatitude.ts.20092010))
BeginningLatitude.ts.20092010 <- window(BeginningLatitude.ts, start = c(2009, 1), end = c(2010, 12))
plot(BeginningLatitude.ts.20092010, xlab = "Time", ylab = "Beginning Latitude", ylim = c(33, 37), bty = "l")
decompose(BeginningLatitude.ts.20092010)
plot(decompose(BeginningLatitude.ts.20092010))
BeginningLatitude.ts.20112012 <- window(BeginningLatitude.ts, start = c(2011, 1), end = c(2012, 12))
plot(BeginningLatitude.ts.20112012, xlab = "Time", ylab = "Beginning Latitude", ylim = c(33, 37), bty = "l")
decompose(BeginningLatitude.ts.20112012)
plot(decompose(BeginningLatitude.ts.20112012))
BeginningLatitude.ts.20132014 <- window(BeginningLatitude.ts, start = c(2013, 1), end = c(2014, 12))
plot(BeginningLatitude.ts.20132014, xlab = "Time", ylab = "Beginning Latitude", ylim = c(33, 37), bty = "l")
decompose(BeginningLatitude.ts.20132014)
plot(decompose(BeginningLatitude.ts.20132014))


#Naive and Seasonal Naive Forecasts for Beginning Latitude
fixed.nValid <- 12
fixed.nTrain <- length(BeginningLatitude.ts) - fixed.nValid 
fixed.nTrain
trainBL.ts <- window(BeginningLatitude.ts, start = c(2009, 1), end = c(2009, fixed.nTrain)) 
validBL.ts <- window(BeginningLatitude.ts, start = c(2009, fixed.nTrain + 1), end = c(2009, fixed.nTrain + fixed.nValid)) 
naiveBL.pred <- naive(trainBL.ts, h = fixed.nValid) 
naiveBL.pred
snaiveBL.pred
snaiveBL.pred <- snaive(trainBL.ts, h = fixed.nValid) 
plot(naiveBL.pred, ylim = c(33,37), ylab = "Beginning Latitude", xlab = "Time", bty = "l",
     xaxt = "n", xlim = c(2009, 2016), main = "", flty = 2)
plot(snaiveBL.pred, ylim = c(33,37), ylab = "Beginning Latitude", xlab = "Time", bty = "l",
     xaxt = "n", xlim = c(2009, 2016), main = "", flty = 2)
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1)))
lines(snaiveBL.pred$fitted, lwd = 2)
validBL.ts <- window(BeginningLatitude.ts, start = c(2014,1), end = c(2015,12))
lines(validBL.ts)
accuracy(naiveBL.pred, validBL.ts) 
accuracy(snaiveBL.pred, validBL.ts) 
plot(naiveBL.predF, ylim = c(0,70), ylab = "Beginning Latitude", xlab = "Time", bty = "l",
     xaxt = "n", xlim = c(2015, 2018), main = "", flty = 2)
axis(1, at = seq(2015, 2018, 1), labels = format(seq(2015,2018, 1)))


#Trailing Moving Average Forecast for Beginning Latitude
nValid <- 12 
nTrain <- length(BeginningLatitude.ts) - nValid 
trainBL.ts <- window(BeginningLatitude.ts, start = c(2009, 1), end = c(2009, nTrain)) 
validBL.ts <- window(BeginningLatitude.ts, start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid)) 
ma.trailing <- rollmean(trainBL.ts, k = 12, align = "right") 
last.ma <- tail(ma.trailing, 1) 
ma.trailing.pred <- ts(rep(last.ma, nValid), start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid), freq = 12) 
plot(trainBL.ts, ylim = c(33, 37), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "") 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009,2018, 1))) 
lines(ma.trailing, lwd = 2, col = "blue") 
lines(ma.trailing.pred, lwd = 2, col = "blue", lty = 2) 
lines(validBL.ts)
ma.trailing.pred
accuracy(ma.trailing.pred, validBL.ts)


#Exponential Smoothing for Beginning Latitude
diff.twice.ts <- diff(diff(BeginningLatitude.ts, lag = 12), lag = 1)
nValid <- 12
nTrain <- length(BeginningLatitude.ts) - nValid 
trainBL.ts <- window(BeginningLatitude.ts, start = c(2009, 1), end = c(2009, nTrain + 1)) 
validBL.ts <- window(BeginningLatitude.ts, start = c(2009, nTrain + 2), end = c(2009, nTrain + 1 + nValid)) 
ses <- ets(trainBL.ts, model = "ZZZ", alpha = 0.2) 
ses.pred <- forecast(ses, h = nValid) 
ses.pred 
plot(ses.pred, ylim = c(33, 37), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2)
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(ses.pred$fitted, lwd = 2, col = "blue") 
lines(validBL.ts)
accuracy(ses.pred$mean, validBL.ts)
ses.pred[1:90]
names(ses.pred)
ses.pred$residuals
validBL.ts - ses.pred$mean
hist(ses.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")
ses.opt <- ets(trainBL.ts, model = "ANN") 
ses.opt.pred <- forecast(ses.opt, h = nValid, level = 0) 
accuracy(ses.pred, validBL.ts)
ses.opt


#Holt Winters Function with CV for Beginning Latitude
nValid <- 12
nTrain <- length(BeginningLatitude.ts) - nValid
nTrain
trainBL.ts <- window(BeginningLatitude.ts, start = c(2009, 1), end = c(2009, nTrain)) 
validBL.ts <- window(BeginningLatitude.ts, start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid)) 
hwF <- ets(trainBL.ts, model = "ZZZ", restrict = FALSE)
hwF.pred <- forecast(hwF, h = nValid) 
hwF.pred
plot(hwF.pred, ylim = c(33, 37), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(hwF.pred$fitted, lwd = 2, col = "blue") 
lines(validBL.ts)
hwF.pred
hwF
hw$states[nrow(hw$states), ]
ets(trainBL.ts)
accuracy(hwF.pred, validBL.ts)


#Linear Regression Model for Beginning Latitude
trend <- BeginningLatitude.ts
nValid <- 12 
nTrain <- length(BeginningLatitude.ts) - nValid 
trainBL.ts <- window(BeginningLatitude.ts, start = c(2009, 1), end = c(2009, nTrain)) 
validBL.ts <- window(BeginningLatitude.ts, start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid)) 
trainBL.lm <- tslm(trainBL.ts ~ trend)
plot(trainBL.ts, xlab = "Time", ylab = "Beginning Latitude", ylim = c(33, 37), bty="l")
lines(trainBL.lm$fitted, lwd=2)
summary(trainBL.lm)
trainBL.lm.pred <- forecast(trainBL.lm, h=nValid)
trainBL.lm.pred
plot(trainBL.lm.pred, ylim = c(33, 37), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1)))
lines(trainBL.lm.pred$fitted, lwd = 2, col = "blue") 
lines(validBL.ts)
accuracy(trainBL.lm.pred, validBL.ts)
trainBL.lm.pred2 <- forecast(trainBL.lm, h=2*nValid, level=0)
plot(trainBL.lm.pred2, ylim = c(0, 50), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1950,2021), main = "", flty = 2) 
axis(1, at = seq(1950, 2021, 1), labels = format(seq(1950, 2021, 1)))
lines(trainBL.lm.pred2$fitted, lwd = 2, col = "blue") 
lines(validBL.ts)
trainBL.lm.pred2
accuracy(trainBL.lm.pred2, validBL.ts)


#Exponential Trend for Tornado Width
nValid <- 72 
nTrain <- length(BeginningLatitude.ts) - nValid 
trainBL.ts <- window(BeginningLatitude.ts, start = c(1950, 1), end = c(1950, nTrain)) 
validBL.ts <- window(BeginningLatitude.ts, start = c(1950, nTrain + 1), end = c(1950, nTrain + nValid)) 
trainBL.lm.expo.trend <- tslm(trainBL.ts ~ trend, lambda = 1) 
trainBL.lm.expo.trend.pred <- forecast(trainBL.lm.expo.trend, h = nValid, level = 0) 
accuracy(trainBL.lm.expo.trend.pred, validBL.ts)
trainBL.lm.linear.trend <- tslm(trainBL.ts ~ trend, lambda = 1) 
trainBL.lm.linear.trend.pred <- forecast(trainBL.lm.linear.trend, h = nValid, level = 0) 
accuracy(trainBL.lm.linear.trend.pred, validBL.ts)
plot(trainBL.lm.expo.trend.pred, ylim = c(0, 50), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1950,2015), main = "", flty = 2) 
axis(1, at = seq(1950, 2015, 1), labels = format(seq(1950, 2015, 1))) 
lines(trainBL.lm.expo.trend.pred$fitted, lwd = 2, col = "blue") 
lines(trainBL.lm.linear.trend.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainBL.lm.linear.trend.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validBL.ts)
trainBL.lm.poly.trend <- tslm(trainBL.ts ~ poly(trend, 2, raw = TRUE))
summary(trainBL.lm.poly.trend)
trainBL.lm.poly.trend$data
trainBL.lm.poly.trend.pred <- forecast(trainBL.lm.poly.trend, h = nValid, level = 0) 
plot(trainBL.lm.poly.trend.pred, ylim = c(0, 50), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1950,2015), main = "", flty = 2) 
axis(1, at = seq(1950, 2015, 1), labels = format(seq(1950, 2015, 1))) 
lines(trainBL.lm.poly.trend.pred$fitted, lwd = 2, col = "blue") 
lines(trainBL.lm.poly.trend.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainBL.lm.poly.trend.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validBL.ts)
accuracy(trainBL.lm.poly.trend.pred, valid.ts)


#Seasonality for Beginning Latitude
trainBL.lm.season <- tslm(trainBL.ts ~ season)
summary(trainBL.lm.season)
trainBL.lm.season$data
trainBL.lm.season.pred <- forecast(trainBL.lm.season, h = nValid) 
trainBL.lm.season.pred
plot(trainBL.lm.season.pred, ylim = c(33, 37), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL.lm.season.pred$fitted, lwd = 2, col = "blue") 
lines(trainBL.lm.season.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainBL.lm.season.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validBL.ts)
validBL.ts
accuracy(trainBL.lm.season.pred, validBL.ts)


#Trend and Seasonality for Beginning Latitude
trainBL.lm.trend.season <- tslm(trainBL.ts ~ trend + season)
summary(trainBL.lm.trend.season)
trainBL.lm.trend.season$data
trainBL.lm.trend.season.pred <- forecast(trainBL.lm.trend.season, h = nValid) 
plot(trainBL.lm.trend.season.pred, ylim = c(33, 37), ylab = "Tornado Width", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL.lm.trend.season.pred$fitted, lwd = 2, col = "blue") 
lines(trainBL.lm.trend.season.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainBL.lm.trend.season.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validBL.ts)
accuracy(trainBL.lm.trend.season.pred, validBL.ts)
trainBL.lm.trend.season.pred


#Model with Quadratic, Sine and Cosine Terms for Beginning Latitude
trainBL.quad.sin.cos <- tslm(trainBL.ts ~ trend + I(trend^2) + I(sin(2*pi*trend/12)) + I(cos(2*pi*trend/12)))
summary(trainBL.quad.sin.cos)
trainBL.quad.sin.cos$data
trainBL.quad.sin.cos.pred <- forecast(trainBL.quad.sin.cos, h = nValid, level = 80) 
trainBL.quad.sin.cos.pred
plot(trainBL.quad.sin.cos.pred, ylim = c(0, 50), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL.quad.sin.cos.pred$fitted, lwd = 2, col = "blue") 
lines(trainBL.quad.sin.cos.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainBL.quad.sin.cos.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validBL.ts)
accuracy(trainBL.quad.sin.cos.pred, validBL.ts)


#Autocorrelation for Beginning Latitude
BeginningLatitude.24.ts <- window(BeginningLatitude.ts, start = c(2009, 1), end = c(2015, 12)) 
Acf(BeginningLatitude.24.ts, lag.max = 12, main = "")


#ARIMA for Beginning Latitude
trainBL.arima <- Arima(trainBL.ts, order = c(1,0,0)) 
trainBL.arima.pred <- forecast(trainBL.arima, h = nValid) 
plot(trainBL.arima.pred, ylim = c(33,37), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL.arima.pred$fitted, lwd = 2, col = "blue") 
trainBL.arima.pred
validBL.ts
lines(trainBL.arima.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validBL.ts)
accuracy(trainBL.arima.pred, validBL.ts)


#Model 1 
trainBL.arima1 <- Arima(trainBL.ts, order = c(1,0,0), seasonal = c(0,1,3)) 
trainBL.arima1.pred <- forecast(trainBL.arima1, h = nValid) 
plot(trainBL.arima1.pred, ylim = c(33,38), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL.arima1.pred$fitted, lwd = 2, col = "blue") 
trainBL.arima1.pred
validBL.ts
lines(trainBL.arima.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validBL.ts)
accuracy(trainBL.arima1.pred, validBL.ts)


#Model 1 - Prediction #1
trainBL.arima1P <- Arima(BeginningLatitude.ts, order = c(1,0,0), seasonal = c(0,1,3)) 
trainBL.arima1P.pred <- forecast(trainBL.arima1P, h = nValid, level = 80) 
plot(trainBL.arima1P.pred, ylim = c(23,50), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL.arima1P.pred$fitted, lwd = 2, col = "blue") 
trainBL.arima1P.pred


#Model 1 - Prediction #2 
trainBL.arima3 <- Arima(trainBL.ts, order = c(1,0,0), seasonal = c(0,1,3)) 
trainBL.arima3.pred <- forecast(trainBL.arima3, h = nValid) 
plot(trainBL.arima3.pred, ylim = c(33,37), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL.arima3.pred$fitted, lwd = 2, col = "blue") 
lines(validBL.ts)
accuracy(trainBL.arima3.pred, validBL.ts)
trainBL.arima3.pred


#Model 1 - Prediction #3 
trainBL.arima1P4 <- Arima(BeginningLatitude.ts, order = c(1,0,0), seasonal = c(0,1,3)) 
trainBL.arima1P4.pred <- forecast(trainBL.arima1P4, h = nValid) 
plot(trainBL.arima1P4.pred, ylim = c(23,50), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL.arima1P4.pred$fitted, lwd = 2, col = "blue") 
trainBL.arima1P4.pred


#Model 2
trainBL.arima2 <- Arima(trainBL.ts, order = c(1,0,0), seasonal = c(0,1,2)) 
trainBL.arima2.pred <- forecast(trainBL.arima2, h = nValid) 
plot(trainBL.arima2.pred, ylim = c(20,50), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL.arima2.pred$fitted, lwd = 2, col = "blue") 
lines(validBL.ts)
accuracy(trainBL.arima2.pred, validBL.ts)
trainBL.arima2.pred


#Model 2 - Prediction 1
trainBL.arima1P6 <- Arima(BeginningLatitude.ts, order = c(1,0,0), seasonal = c(0,1,2)) 
trainBL.arima1P6.pred <- forecast(trainBL.arima1P6, h = nValid, level=80) 
plot(trainBL.arima1P6.pred, ylim = c(30,50), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL.arima1P6.pred$fitted, lwd = 2, col = "blue") 
trainBL.arima1P6.pred

#Model 2 - Prediction 2
trainBL.arima1P6b <- Arima(BeginningLatitude.ts, order = c(1,0,0), seasonal = c(0,1,2)) 
trainBL.arima1P6b.pred <- forecast(trainBL.arima1P6b, h = nValid, level = 70) 
plot(trainBL.arima1P6b.pred, ylim = c(23,50), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL.arima1P6b.pred$fitted, lwd = 2, col = "blue") 
trainBL.arima1P6b.pred

#Model 2 - Prediction 3
trainBL.arima1P6c <- Arima(BeginningLatitude.ts, order = c(1,0,0), seasonal = c(0,1,2)) 
trainBL.arima1P6c.pred <- forecast(trainBL.arima1P6c, h = nValid, level = 20) 
plot(trainBL.arima1P6c.pred, ylim = c(23,50), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL.arima1P6c.pred$fitted, lwd = 2, col = "blue") 
trainBL.arima1P6c.pred


#Model 1 - Prediction 4
trainBL.arima1P7 <- Arima(BeginningLatitude.ts, order = c(1,0,0), seasonal = c(0,1,1)) 
trainBL.arima1P7.pred <- forecast(trainBL.arima1P7, h = nValid) 
plot(trainBL.arima1P7.pred, ylim = c(23,50), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL.arima1P6.pred$fitted, lwd = 2, col = "blue") 
trainBL.arima1P6.pred


#Model 1 - Prediction #10
trainBL.arima1P10 <- Arima(BeginningLatitude.ts, order = c(1,0,0), seasonal = c(1,1,1)) 
trainBL.arima1P10.pred <- forecast(trainBL.arima1P10, h = nValid) 
plot(trainBL.arima1P10.pred, ylim = c(23,50), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL.arima1P10.pred$fitted, lwd = 2, col = "blue") 
trainBL.arima1P10.pred


#Model 1 - Prediction #11 
trainBL.arima1P11 <- Arima(BeginningLatitude.ts, order = c(1,0,0), seasonal = c(1,1,2)) 
trainBL.arima1P11.pred <- forecast(trainBL.arima1P11, h = nValid) 
plot(trainBL.arima1P11.pred, ylim = c(23,50), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL.arima1P11.pred$fitted, lwd = 2, col = "blue") 
trainBL.arima1P11.pred

#Model 1 - Prediction #12 
trainBL.arima1P12 <- Arima(BeginningLatitude.ts, order = c(1,0,0), seasonal = c(1,1,3)) 
trainBL.arima1P12.pred <- forecast(trainBL.arima1P12, h = nValid) 
plot(trainBL.arima1P12.pred, ylim = c(23,50), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL.arima1P12.pred$fitted, lwd = 2, col = "blue") 
trainBL.arima1P12.pred


#Model 1 - Prediction #13
trainBL.arima1P13 <- Arima(BeginningLatitude.ts, order = c(1,0,0), seasonal = c(2,1,3)) 
trainBL.arima1P13.pred <- forecast(trainBL.arima1P13, h = nValid) 
plot(trainBL.arima1P13.pred, ylim = c(23,50), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL.arima1P13.pred$fitted, lwd = 2, col = "blue") 
trainBL.arima1P13.pred


#Model 1 - Prediction #14 - good
trainBL.arima1P14 <- Arima(BeginningLatitude.ts, order = c(1,0,0), seasonal = c(2,1,2)) 
trainBL.arima1P14.pred <- forecast(trainBL.arima1P14, h = nValid) 
plot(trainBL.arima1P14.pred, ylim = c(23,50), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL.arima1P14.pred$fitted, lwd = 2, col = "blue") 
trainBL.arima1P14.pred


#Model 1 - Prediction #15 - quite good
trainBL.arima1P15 <- Arima(BeginningLatitude.ts, order = c(1,0,0), seasonal = c(2,1,1)) 
trainBL.arima1P15.pred <- forecast(trainBL.arima1P15, h = nValid) 
plot(trainBL.arima1P15.pred, ylim = c(23,50), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL.arima1P15.pred$fitted, lwd = 2, col = "blue") 
trainBL.arima1P15.pred


#Model 3 
trainBL.arima1 <- Arima(trainBL.ts, order = c(1,0,0), seasonal = c(0,1,0)) 
trainBL.arima1.pred <- forecast(trainBL.arima1, h = nValid) 
plot(trainBL.arima1.pred, ylim = c(33,38), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL.arima1.pred$fitted, lwd = 2, col = "blue") 
trainBL.arima1.pred
validBL.ts
lines(trainBL.arima.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validBL.ts)
accuracy(trainBL.arima1.pred, validBL.ts)


#model 4
trainBL2.arima <- Arima(trainBL.ts, order = c(1,0,1), seasonal = c(0,1,0)) 
plot(BeginningLatitude.ts, ylim = c(30, 40), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "") 
trainBL2.arima.pred <- forecast(trainBL2.arima, h = nValid) 
plot(trainBL2.arima.pred, ylim = c(30,40), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL2.arima.pred$fitted, lwd = 2, col = "blue") 
trainBL2.arima.pred
validBL.ts
lines(trainBL2.arima.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validBL.ts)
accuracy(trainBL2.arima.pred, validBL.ts)


#model 5
trainBL4.arima <- Arima(trainBL.ts, order = c(1,0,2), seasonal = c(0,1,0)) 
trainBL4.arima.pred <- forecast(trainBL4.arima, h = nValid)
plot(trainBL4.arima.pred, ylim = c(33,38), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(trainBL4.arima.pred$fitted, lwd = 2, col = "blue") 
trainBL4.arima.pred
validBL.ts
lines(trainBL4.arima.pred$mean, lwd = 2, col = "red", lty = 3) 
lines(validBL.ts)
accuracy(trainBL4.arima.pred, validBL.ts)


#model 6 - optimal*
trainBL5.arima <- Arima(trainBL.ts, order = c(1,0,3), seasonal = c(0,1,0)) 
trainBL5.arima.pred <- forecast(trainBL5.arima, h = nValid)
plot(trainBL5.arima.pred, ylim = c(33,38), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(trainBL5.arima.pred$fitted, lwd = 2, col = "blue") 
trainBL5.arima.pred
validBL.ts
lines(trainBL5.arima.pred$mean, lwd = 2, col = "red", lty = 3) 
lines(validBL.ts)
accuracy(trainBL5.arima.pred, validBL.ts)
trainBL5P.arima <- Arima(BeginningLatitude.ts, order = c(1,0,3), seasonal = c(0,1,0)) 
trainBL5P.arima.pred <- forecast(trainBL5P.arima, h = nValid)
plot(trainBL5P.arima.pred, ylim = c(33,38), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(trainBL5P.arima.pred$fitted, lwd = 2, col = "blue") 
trainBL5P.arima.pred
validBL.ts
lines(trainBL5P.arima.pred$mean, lwd = 2, col = "red", lty = 3) 
lines(BeginningLatitude2.ts)
accuracy(trainBL5P.arima.pred, BeginningLatitude2.ts)


#model 7
trainBL6.arima <- Arima(trainBL.ts, order = c(1,0,3), seasonal = c(1,0,0)) 
trainBL6.arima.pred <- forecast(trainBL6.arima, h = nValid)
plot(trainBL6.arima.pred, ylim = c(33,38), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(trainBL6.arima.pred$fitted, lwd = 2, col = "blue") 
trainBL6.arima.pred
validBL.ts
lines(trainBL6.arima.pred$mean, lwd = 2, col = "red", lty = 3) 
lines(validBL.ts)


#model 7 - prediction #1
trainBL5P2.arima <- Arima(BeginningLatitude.ts, order = c(1,0,3), seasonal = c(1,0,0)) 
trainBL5P2.arima.pred <- forecast(trainBL5P2.arima, h = nValid)
plot(trainBL5P2.arima.pred, ylim = c(33,38), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(trainBL5P2.arima.pred$fitted, lwd = 2, col = "blue") 
trainBL5P2.arima.pred
BeginningLatitude2.ts
lines(trainBL5P2.arima.pred$mean, lwd = 2, col = "red", lty = 3) 
lines(BeginningLatitude2.ts)
accuracy(trainBL5P2.arima.pred, BeginningLatitude2.ts)


#model 8
trainBL6.arima <- Arima(trainBL.ts, order = c(1,1,0), seasonal = c(1,0,0)) 
trainBL6.arima.pred <- forecast(trainBL6.arima, h = nValid)
plot(trainBL6.arima.pred, ylim = c(33,38), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2016, 1), labels = format(seq(2009, 2016, 1))) 
lines(trainBL6.arima.pred$fitted, lwd = 2, col = "blue") 
trainBL6.arima.pred
validBL.ts
lines(trainBL6.arima.pred$mean, lwd = 2, col = "red", lty = 3) 
lines(validBL.ts)
accuracy(trainBL6.arima.pred, validBL.ts)

#model 4 - prediction#1 
trainBL4F.arima <- Arima(BeginningLatitude.ts, order = c(2,1,3), seasonal = c(2,1,3)) 
trainBL4F.arima.pred <- forecast(trainBL4F.arima, h = nValid, level = 80)
plot(trainBL4F.arima.pred, ylim = c(30,40), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL4F.arima.pred$fitted, lwd = 2, col = "blue") 
trainBL4F.arima.pred
validBL.ts
lines(trainBL4F.arima.pred$mean, lwd = 2, col = "red", lty = 3) 
lines(validBL.ts)
accuracy(trainBL4F.arima.pred, validBL.ts)


#model 4 - prediction#2 
trainBL4F2.arima <- Arima(BeginningLatitude.ts, order = c(2,1,3), seasonal = c(2,1,3)) 
trainBL4F2.arima.pred <- forecast(trainBL4F2.arima, h = nValid)
plot(trainBL4F2.arima.pred, ylim = c(30,40), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBL4F2.arima.pred$fitted, lwd = 2, col = "blue") 
trainBL4F2.arima.pred


#Neural Network for forecasting Beginning Latitude
set.seed(201) 
BeginningLatitude.nnetar <- nnetar(trainBL.ts, repeats = 20, p = 11, P = 1, size = 7) 
summary(BeginningLatitude.nnetar$model[[1]]) 
BeginningLatitude.nnetar.pred <- forecast(BeginningLatitude.nnetar, h = nValid)  
plot(BeginningLatitude.ts, ylim = c(33, 37), ylab = "Beginning Latitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), lty = 1) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(BeginningLatitude.nnetar.pred$fitted, lwd = 2, col = "blue") 
lines(BeginningLatitude.nnetar.pred$mean, lwd = 2, col = "red", lty = 2) 
lines(validBL.ts)
validBL.ts
BeginningLatitude.nnetar.pred
accuracy(BeginningLatitude.nnetar.pred, validBL.ts)


#Time Series Beginning Longitude
BeginningLongitude.ts <- ts(StormDataNew$BeginningLongitude, start = c(2009,1), end = c(2014,12), freq = 12)
BeginningLongitude2.ts <- ts(StormDataNew$BeginningLongitude, start = c(2015,1), end = c(2015,12), freq = 12)
class(BeginningLongitude.ts)
start(BeginningLongitude.ts)
end(BeginningLongitude.ts)
frequency(BeginningLongitude.ts)
summary(BeginningLongitude.ts)
plot(BeginningLongitude.ts)
time(BeginningLongitude.ts)
cycle(BeginningLongitude.ts)
aggregate(BeginningLongitude.ts)
plot(decompose(BeginningLongitude.ts))
aggregate(BeginningLongitude.ts, FUN=mean)
plot(aggregate(BeginningLongitude.ts))
boxplot(BeginningLongitude.ts-cycle(BeginningLongitude.ts))
BeginningLongitude.ts.zoom <- window(BeginningLongitude.ts, start = c(1997, 1), end = c(2000, 12))
plot(BeginningLongitude.ts.zoom, xlab = "Time", ylab = "Beginning Longitude", ylim = c(0, -100), bty = "l")
BeginningLongitude <- na.interpolation(BeginningLongitude.ts, option = "linear", na.identifier = NA)
BeginningLongitude.ts.20092010 <- window(BeginningLongitude.ts, start = c(2009, 1), end = c(2010, 12))
plot(BeginningLongitude.ts.20092010, xlab = "Time", ylab = "Beginning Longitude", ylim = c(-94, -103), bty = "l")
decompose(BeginningLongitude.ts.20092010)
plot(decompose(BeginningLongitude.ts.20092010))
BeginningLongitude.ts.20112012 <- window(BeginningLongitude.ts, start = c(2011, 1), end = c(2012, 12))
plot(BeginningLongitude.ts.20112012, xlab = "Time", ylab = "Beginning Longitude", ylim = c(-94, -103), bty = "l")
decompose(BeginningLongitude.ts.20112012)
plot(decompose(BeginningLongitude.ts.20112012))
BeginningLongitude.ts.20132014 <- window(BeginningLongitude.ts, start = c(2013, 1), end = c(2014, 12))
plot(BeginningLongitude.ts.20132014, xlab = "Time", ylab = "Beginning Longitude", ylim = c(-94, -103), bty = "l")
decompose(BeginningLongitude.ts.20132014)
plot(decompose(BeginningLongitude.ts.20132014))


#Naive and Seasonal Naive Forecasts for Beginning Longitude
fixed.nValid <- 12
fixed.nTrain <- length(BeginningLongitude.ts) - fixed.nValid 
trainBLO.ts <- window(BeginningLongitude.ts, start = c(2009, 1), end = c(2009, fixed.nTrain)) 
validBLO.ts <- window(BeginningLongitude.ts, start = c(2009, fixed.nTrain + 1), end = c(2009, fixed.nTrain + fixed.nValid)) 
naiveBLO.pred <- naive(trainBLO.ts, h = fixed.nValid) 
naiveBLO.pred
snaiveBLO.pred
snaiveBLO.pred <- snaive(trainBLO.ts, h = fixed.nValid) 
accuracy(naiveBLO.pred, validBLO.ts) 
accuracy(snaiveBLO.pred, validBLO.ts)
plot(naiveBLO.pred, ylim = c(-94,-103), ylab = "Beginning Longitude", xlab = "Time", bty = "l",
     xaxt = "n", xlim = c(2009, 2016), main = "", flty = 2)
plot(snaiveBLO.pred, ylim = c(-94,-103), ylab = "Beginning Longitude", xlab = "Time", bty = "l",
     xaxt = "n", xlim = c(2009, 2016), main = "", flty = 2)
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1)))
lines(snaiveBLO.pred$fitted, lwd = 2)
validBLO.ts <- window(BeginningLongitude.ts, start = c(2014,1), end = c(2015,12))
lines(validBLO.ts)
accuracy(naiveBLO.pred, validBLO.ts)
plot(snaiveBLO.pred, ylim = c(-30,-160), ylab = "Beginning Longitude", xlab = "Time", bty = "l",
     xaxt = "n", xlim = c(1950, 2015), main = "", flty = 2)
axis(1, at = seq(1950, 2021, 1), labels = format(seq(1950, 2021, 1)))
lines(snaiveBLO.pred$fitted, lwd = 2)
accuracy(snaiveBLO.pred, validBLO.ts)


#Moving Average Forecast for Beginning Longitude
nValid <- 12
nTrain <- length(BeginningLongitude.ts) - nValid 
trainBLO.ts <- window(BeginningLongitude.ts, start = c(2009, 1), end = c(2009, nTrain)) 
validBLO.ts <- window(BeginningLongitude.ts, start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid)) 
ma.trailing <- rollmean(trainBLO.ts, k = 12, align = "right") 
last.ma <- tail(ma.trailing, 1) 
ma.trailing.pred <- ts(rep(last.ma, nValid), start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid), freq = 12) 
plot(trainBLO.ts, ylim = c(-94, -103), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "") 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009,2018, 1))) 
lines(ma.trailing, lwd = 2, col = "blue") 
lines(ma.trailing.pred, lwd = 2, col = "blue", lty = 2) 
lines(validBLO.ts)
ma.trailing.pred
accuracy(ma.trailing.pred, validBLO.ts)


#Differencing and Exponential Smoothing for Beginning Longitude
diff.twice.ts <- diff(diff(BeginningLongitude.ts, lag = 12), lag = 1)
nValid <- 12
nTrain <- length(BeginningLongitude.ts) - nValid 
trainBLO.ts <- window(BeginningLongitude.ts, start = c(2009, 1), end = c(2009, nTrain + 1)) 
validBLO.ts <- window(BeginningLongitude.ts, start = c(2009, nTrain + 2), end = c(2009, nTrain + 1 + nValid)) 
ses <- ets(trainBLO.ts, model = "ZZZ", alpha = 0.2) 
ses.pred <- forecast(ses, h = nValid) 
ses.pred 
plot(ses.pred, ylim = c(-94, -103), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2)
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(ses.pred$fitted, lwd = 2, col = "blue") 
lines(validBLO.ts)
accuracy(ses.pred$mean, validBLO.ts)


#Holt Winters Function with CV for Beginning Longitude
length(BeginningLongitude.ts)
nValid <- 12
nTrain <- length(BeginningLongitude.ts) - nValid 
nTrain
trainBLO.ts <- window(BeginningLongitude.ts, start = c(2009, 1), end = c(2009, nTrain)) 
validBLO.ts <- window(BeginningLongitude.ts, start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid)) 
hw <- ets(trainBLO.ts, model = "ZZZ", restrict = FALSE)
hw.pred <- forecast(hw, h = nValid) 
hw.pred
plot(hw.pred, ylim = c(-94, -103), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(hw.pred$fitted, lwd = 2, col = "blue") 
lines(validBLO.ts)
hw.pred
hw
hw$states[nrow(hw$states), ]
ets(trainBLO.ts)
accuracy(hw.pred, validBLO.ts)


#Linear Regression Model for Beginning Longitude
trend<-BeginningLongitude.ts
nValid <- 12
nTrain <- length(BeginningLongitude.ts) - nValid 
trainBLO.ts <- window(BeginningLongitude.ts, start = c(2009, 1), end = c(2009, nTrain)) 
validBLO.ts <- window(BeginningLongitude.ts, start = c(2009, nTrain + 1), end = c(2009, nTrain + nValid)) 
trainBLO.lm <- tslm(trainBLO.ts ~ trend)
plot(BeginningLongitude.ts, xlab = "Time", ylab = "Beginning Longitude", ylim = c(-94, -103), bty="l")
lines(trainBLO.lm$fitted, lwd=2)
summary(trainBLO.lm)
trainBLO.lm.pred <- forecast(trainBLO.lm, h=nValid)
plot(trainBLO.lm.pred, ylim = c(-94, -103), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1)))
lines(trainBLO.lm.pred$fitted, lwd = 2, col = "blue") 
lines(validBLO.ts)
accuracy(trainBLO.lm.pred, validBLO.ts)


#Seasonality for Beginning Longitude
trainBLO.lm.season <- tslm(trainBLO.ts ~ season)
summary(trainBLO.lm.season)
trainBLO.lm.season$data
trainBLO.lm.season.pred <- forecast(trainBLO.lm.season, h = nValid) 
plot(trainBLO.lm.season.pred, ylim = c(-94, -103), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO.lm.season.pred$fitted, lwd = 2, col = "blue") 
lines(trainBLO.lm.season.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainBLO.lm.season.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validBLO.ts)
accuracy(trainBLO.lm.season.pred, validBLO.ts)


#Trend and Seasonality for Beginning Longitude
trend <- BeginningLongitude.ts
trainBLO.lm.trend.season <- tslm(trainBLO.ts ~ trend + season)
summary(trainBLO.lm.trend.season)
trainBLO.lm.trend.season$data
trainBLO.lm.trend.season.pred <- forecast(trainBLO.lm.trend.season, h = nValid) 
plot(trainBLO.lm.trend.season.pred, ylim = c(-94, -103), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO.lm.trend.season.pred$fitted, lwd = 2, col = "blue") 
lines(trainBLO.lm.trend.season.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainBLO.lm.trend.season.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validBLO.ts)
accuracy(trainBLO.lm.trend.season.pred, validBLO.ts)


#Model with Quadratic, Sine and Cosine Terms for Beginning Longitude
trainBLO.quad.sin.cos <- tslm(trainBLO.ts ~ trend + I(trend^2) + I(sin(2*pi*trend/12)) + I(cos(2*pi*trend/12)))
summary(trainBLO.quad.sin.cos)
trainBLO.quad.sin.cos$data
trainBLO.quad.sin.cos.pred <- forecast(trainBLO.quad.sin.cos, h = nValid, level = 0) 
plot(trainBLO.quad.sin.cos.pred, ylim = c(0, -100), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1950,2015), main = "", flty = 2) 
axis(1, at = seq(1950, 2015, 1), labels = format(seq(1950, 2015, 1))) 
lines(trainBLO.quad.sin.cos.pred$fitted, lwd = 2, col = "blue") 
lines(trainBLO.quad.sin.cos.pred$fitted, lwd = 2, col = "black", lty = 3) 
lines(trainBLO.quad.sin.cos.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validBLO.ts)


#Autocorrelation for Beginning Longitude
BeginningLongitude.24.ts <- window(BeginningLongitude.ts, start = c(2009, 1), end = c(2014, 12)) 
Acf(BeginningLongitude.24.ts, lag.max = 12, main = "")


#ARIMA for Beginning Longitude
trainBLO.arima <- Arima(trainBLO.ts, order = c(1,0,0)) 
plot(BeginningLongitude.ts, ylim = c(-94, -103), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "") 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO.arima$fitted, lwd = 2, col = "blue")
trainBLO.arima.pred <- forecast(trainBLO.arima, h = nValid) 
plot(trainBLO.arima.pred, ylim = c(-94,-103), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO.arima.pred$fitted, lwd = 2, col = "blue") 
lines(trainBLO.arima.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validBLO.ts)
accuracy(trainBLO.arima.pred, validBLO.ts)
validBLO.ts


#Model 1 
library(forecast)
trainBLO1.arima <- Arima(trainBLO.ts, order = c(1,0,0), seasonal = c(0,1,3)) 
trainBLO1.arima.pred <- forecast(trainBLO1.arima, h = nValid) 
plot(trainBLO1.arima.pred, ylim = c(-90,-105), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO1.arima.pred$fitted, lwd = 2, col = "blue") 
trainBLO1.arima.pred
validBLO.ts
lines(trainBLO1.arima.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validBLO.ts)
accuracy(trainBLO1.arima.pred, validBLO.ts)


#Model 1 - Prediction #1
trainBLO1P.arima <- Arima(BeginningLongitude.ts, order = c(1,0,0), seasonal = c(0,1,3)) 
trainBLO1P.arima.pred <- forecast(trainBLO1P.arima, h = nValid, level = 80) 
plot(trainBLO1P.arima.pred, ylim = c(-90,-105), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO1P.arima.pred$fitted, lwd = 2, col = "blue") 
trainBLO1P.arima.pred
validBLO.ts
lines(trainBLO1.arima.pred$mean, lwd = 2, col = "black", lty = 3) 
lines(validBLO.ts)
accuracy(trainBLO1.arima.pred, validBLO.ts)


#Model 2 - better than 1
trainBLO2.arima <- Arima(trainBLO.ts, order = c(1,0,0), seasonal = c(0,1,2)) 
trainBLO2.arima.pred <- forecast(trainBLO2.arima, h = nValid) 
plot(trainBLO2.arima.pred, ylim = c(-90,-105), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO2.arima.pred$fitted, lwd = 2, col = "blue") 
lines(validBLO.ts)
accuracy(trainBLO2.arima.pred, validBLO.ts)
trainBLO2.arima.pred


#Model 1 - Prediction #2
trainBLO1P2.arima <- Arima(BeginningLongitude.ts, order = c(1,0,0), seasonal = c(0,1,2)) 
trainBLO1P2.arima.pred <- forecast(trainBLO1P2.arima, h = nValid) 
plot(trainBLO1P2.arima.pred, ylim = c(-90,-105), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO1P2.arima.pred$fitted, lwd = 2, col = "blue") 
trainBLO1P2.arima.pred


#Model 3 - way better than 1 and 2 
trainBLO3.arima <- Arima(trainBLO.ts, order = c(1,0,0), seasonal = c(0,1,1)) 
trainBLO3.arima.pred <- forecast(trainBLO3.arima, h = nValid) 
plot(trainBLO3.arima.pred, ylim = c(-90,-105), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO3.arima.pred$fitted, lwd = 2, col = "blue") 
lines(validBLO.ts)
accuracy(trainBLO3.arima.pred, validBLO.ts)
trainBLO3.arima.pred


#Model 4 
trainBLO4.arima <- Arima(trainBLO.ts, order = c(1,0,2), seasonal = c(1,0,2)) 
trainBLO4.arima.pred <- forecast(trainBLO4.arima, h = nValid) 
plot(trainBLO4.arima.pred, ylim = c(-90,-105), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO4.arima.pred$fitted, lwd = 2, col = "blue") 
lines(validBLO.ts)
accuracy(trainBLO4.arima.pred, validBLO.ts)
trainBLO4.arima.pred
accuracy(trainBLO4.arima.pred, validBLO.ts)
validBLO.ts


#Model 4 - Prediction#1
trainBLO4F.arima <- Arima(BeginningLongitude.ts, order = c(1,0,2), seasonal = c(1,0,2)) 
trainBLO4F.arima.pred <- forecast(trainBLO4F.arima, h = nValid) 
plot(trainBLO4F.arima.pred, ylim = c(-90,-105), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO4F.arima.pred$fitted, lwd = 2, col = "blue") 
lines(BeginningLongitude2.ts)
accuracy(trainBLO4F.arima.pred, BeginningLongitude2.ts)
trainBLO4F.arima.pred
BeginningLongitude2.ts


#Model 4b 
trainBLO4b.arima <- Arima(trainBLO.ts, order = c(1,0,3), seasonal = c(1,0,3)) 
trainBLO4b.arima.pred <- forecast(trainBLO4b.arima, h = nValid) 
plot(trainBLO4b.arima.pred, ylim = c(-90,-105), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO4b.arima.pred$fitted, lwd = 2, col = "blue") 
lines(validBLO.ts)
accuracy(trainBLO4b.arima.pred, validBLO.ts)
trainBLO4b.arima.pred
accuracy(trainBLO4b.arima.pred, validBLO.ts)
validBLO.ts


#Model 4 - Prediction#2
trainBLO4bF.arima <- Arima(BeginningLongitude.ts, order = c(1,0,3), seasonal = c(1,0,3)) 
trainBLO4bF.arima.pred <- forecast(trainBLO4bF.arima, h = nValid) 
plot(trainBLO4bF.arima.pred, ylim = c(-90,-105), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO4bF.arima.pred$fitted, lwd = 2, col = "blue") 
lines(BeginningLongitude2.ts)
accuracy(trainBLO4bF.arima.pred, BeginningLongitude2.ts)
trainBLO4bF.arima.pred
BeginningLongitude2.ts
accuracy(trainBLO4bF.arima.pred, BeginningLongitude2.ts)


#Model 4c 
trainBLO4c.arima <- Arima(trainBLO.ts, order = c(1,0,0), seasonal = c(1,1,3)) 
trainBLO4c.arima.pred <- forecast(trainBLO4c.arima, h = nValid) 
plot(trainBLO4c.arima.pred, ylim = c(-90,-105), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO4c.arima.pred$fitted, lwd = 2, col = "blue") 
lines(validBLO.ts)
accuracy(trainBLO4c.arima.pred, validBLO.ts)
trainBLO4c.arima.pred
accuracy(trainBLO4c.arima.pred, validBLO.ts)
validBLO.ts


#Model 4P - SELECTED*
trainBLO4P.arima <- Arima(BeginningLongitude.ts, order = c(1,0,0), seasonal = c(1,1,3)) 
trainBLO4P.arima.pred <- forecast(trainBLO4P.arima, h = nValid) 
plot(trainBLO4P.arima.pred, ylim = c(-90,-105), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO4P.arima.pred$fitted, lwd = 2, col = "blue") 
lines(BeginningLongitude2.ts)
accuracy(trainBLO4P.arima.pred, BeginningLongitude2.ts)
trainBLO4P.arima.pred
BeginningLongitude2.ts


#Model 5
trainBLO5.arima <- Arima(trainBLO.ts, order = c(1,0,0), seasonal = c(1,1,2)) 
trainBLO5.arima.pred <- forecast(trainBLO5.arima, h = nValid) 
plot(trainBLO5.arima.pred, ylim = c(-94,-103), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO5.arima.pred$fitted, lwd = 2, col = "blue") 
trainBLO5.arima.pred
accuracy(trainBLO5.arima.pred, validBLO.ts)
validBLO.ts
trainBLO5P.arima <- Arima(BeginningLongitude.ts, order = c(1,0,0), seasonal = c(1,1,2)) 
trainBLO5P.arima.pred <- forecast(trainBLO5P.arima, h = nValid) 
plot(trainBLO5P.arima.pred, ylim = c(-94,-103), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO5P.arima.pred$fitted, lwd = 2, col = "blue") 
trainBLO5P.arima.pred
accuracy(trainBLO5P.arima.pred, BeginningLongitude2.ts)
BeginningLongitude2.ts


#Model 6
trainBLO6.arima <- Arima(trainBLO.ts, order = c(1,0,0), seasonal = c(1,1,3)) 
trainBLO6.arima.pred <- forecast(trainBLO6.arima, h = nValid) 
plot(trainBLO6.arima.pred, ylim = c(-94,-103), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO6.arima.pred$fitted, lwd = 2, col = "blue") 
trainBLO6.arima.pred
accuracy(trainBLO6.arima.pred, validBLO.ts)
validBLO.ts


#Model 7 
trainBLO7.arima <- Arima(trainBLO.ts, order = c(2,2,3), seasonal = c(2,2,3)) 
trainBLO7.arima.pred <- forecast(trainBLO7.arima, h = nValid) 
plot(trainBLO7.arima.pred, ylim = c(-94,-103), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO7.arima.pred$fitted, lwd = 2, col = "blue") 
trainBLO7.arima.pred
accuracy(trainBLO7.arima.pred, validBLO.ts)
validBLO.ts


#Model 8 
trainBLO8.arima <- Arima(trainBLO.ts, order = c(1,0,0), seasonal = c(0,1,2)) 
trainBLO8.arima.pred <- forecast(trainBLO8.arima, h = nValid) 
plot(trainBLO8.arima.pred, ylim = c(-94,-103), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2016), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO8.arima.pred$fitted, lwd = 2, col = "blue") 
trainBLO8.arima.pred
accuracy(trainBLO8.arima.pred, validBLO.ts)
validBLO.ts
trainBLO1P3.arima <- Arima(BeginningLongitude.ts, order = c(1,0,0), seasonal = c(1,1,2)) 
trainBLO1P3.arima.pred <- forecast(trainBLO1P3.arima, h = nValid) 
plot(trainBLO1P3.arima.pred, ylim = c(-90,-105), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO1P3.arima.pred$fitted, lwd = 2, col = "blue") 
trainBLO1P3.arima.pred

#Model 1 - Prediction #4 
trainBLO2.arima <- Arima(trainBLO.ts, order = c(1,0,0), seasonal = c(1,1,3)) 
trainBLO2.arima.pred <- forecast(trainBLO2.arima, h = nValid, level=66) 
plot(trainBLO2.arima.pred, ylim = c(-94,-103), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO2.arima.pred$fitted, lwd = 2, col = "blue") 
trainBLO2.arima.pred
accuracy(trainBLO2.arima.pred, validBLO.ts)

trainBLO1P4.arima <- Arima(BeginningLongitude.ts, order = c(1,0,0), seasonal = c(1,1,3)) 
trainBLO1P4.arima.pred <- forecast(trainBLO1P4.arima, h = nValid) 
plot(trainBLO1P4.arima.pred, ylim = c(-94,-103), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO1P4.arima.pred$fitted, lwd = 2, col = "blue") 
trainBLO1P4.arima.pred

mean(DamageProperty.ts)
mean(TornadoWidth.ts)
sd(TornadoWidth.ts)
#Model 1 - Prediction #4b
trainBLO1P4b.arima <- Arima(BeginningLongitude.ts, order = c(1,0,0), seasonal = c(1,1,3)) 
trainBLO1P4b.arima.pred <- forecast(trainBLO1P4b.arima, h = nValid, level=80) 
plot(trainBLO1P4b.arima.pred, ylim = c(-90,-110), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO1P4b.arima.pred$fitted, lwd = 2, col = "blue") 
trainBLO1P4b.arima.pred


#Model 1 - Prediction #5 
trainBLO1P5.arima <- Arima(BeginningLongitude.ts, order = c(1,0,0), seasonal = c(1,1,0)) 
trainBLO1P5.arima.pred <- forecast(trainBLO1P5.arima, h = nValid) 
plot(trainBLO1P5.arima.pred, ylim = c(-90,-105), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO1P5.arima.pred$fitted, lwd = 2, col = "blue") 
trainBLO1P5.arima.pred


#Model 1 - Prediction #6
trainBLO1P6.arima <- Arima(BeginningLongitude.ts, order = c(1,0,0), seasonal = c(2,1,3)) 
trainBLO1P6.arima.pred <- forecast(trainBLO1P6.arima, h = nValid) 
plot(trainBLO1P6.arima.pred, ylim = c(-90,-105), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), main = "", flty = 2) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(trainBLO1P6.arima.pred$fitted, lwd = 2, col = "blue") 
trainBLO1P6.arima.pred



#Neural Network for forecasting Beginning Longitude
set.seed(201) 
BeginningLongitude.nnetar <- nnetar(trainBLO.ts, repeats = 20, p = 11, P = 1, size = 7) 
summary(BeginningLongitude.nnetar$model[[1]]) 
BeginningLongitude.nnetar.pred <- forecast(BeginningLongitude.nnetar, h = nValid, level = 95)  
plot(trainBLO.ts, ylim = c(-94, -103), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(2009,2018), lty = 1) 
axis(1, at = seq(2009, 2018, 1), labels = format(seq(2009, 2018, 1))) 
lines(BeginningLongitude.nnetar.pred$fitted, lwd = 2, col = "blue") 
lines(BeginningLongitude.nnetar.pred$mean, lwd = 2, col = "blue", lty = 2) 
lines(validBLO.ts)
BeginningLongitude.nnetar.pred
validBLO.ts
accuracy(BeginningLongitude.nnetar.pred, validBLO.ts)
BeginningLongitude.nnetar.pred2 <- forecast(BeginningLongitude.nnetar.pred, h = 2*nValid)  
plot(trainBLO.ts, ylim = c(0, -100), ylab = "Beginning Longitude", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1950,2021), lty = 1) 
axis(1, at = seq(1950, 2021, 1), labels = format(seq(1950, 2021, 1))) 
lines(BeginningLongitude.nnetar.pred2$fitted, lwd = 2, col = "blue") 
lines(BeginningLongitude.nnetar.pred2$mean, lwd = 2, col = "blue", lty = 2) 
lines(validBLO.ts)
BeginningLongitude.nnetar.pred2 
accuracy(BeginningLongitude.nnetar.pred2, validBLO.ts)

###########################################################

#Data Mining R Code

StormDataCollective <- read.csv("StormDataCollective.csv")
TornadoData <- StormDataCollective
names(TornadoData)
dim(TornadoData)
summary(TornadoData)
InjuriesDirect <- as.numeric(TornadoData$INJURIES_DIRECT)
InjuriesDirect <- scale(InjuriesDirect)
InjuriesIndirect <- as.numeric(TornadoData$INJURIES_INDIRECT) 
InjuriesIndirect <- scale(InjuriesIndirect)
DeathsDirect <- as.numeric(TornadoData$DEATHS_DIRECT) 
DeathsDirect <- scale(DeathsDirect)
DeathsIndirect <- as.numeric(TornadoData$DEATHS_INDIRECT)
DeathsIndirect <- scale(DeathsIndirect)
DamageProperty <- as.numeric(TornadoData$DAMAGE_PROPERTY)
DamageProperty <- scale(DamageProperty)
TornadoLength <- as.numeric(TornadoData$TOR_LENGTH)
TornadoLength <- scale(TornadoLength)
TornadoWidth <- as.numeric(TornadoData$TOR_WIDTH)
TornadoWidth <- scale(TornadoWidth)
mean(DamageProperty)
TornadoDataNew <- data.frame(DamageProperty, TornadoLength, TornadoWidth,
                             DeathsIndirect, DeathsDirect, InjuriesIndirect)                                                                                  
names(TornadoDataNew)
dim(TornadoDataNew)
summary(TornadoDataNew)
mean(TornadoDataNew$DamageProperty)
TornadoDataFinal <- na.omit(TornadoDataNew)
dim(TornadoDataFinal)
summary(TornadoDataFinal)
mean(TornadoDataFinal$DamageProperty)
plot(TornadoDataFinal$DamageProperty)
head(TornadoDataFinal$DamageProperty)
summary(TornadoDataFinal$DamageProperty)
cor(TornadoDataFinal)
M <- cor(TornadoDataFinal)
corrplot(M)
head(TornadoDataFinal)
plot(TornadoDataFinal)
hist(DamageProperty)
train <- sample(1:nrow(TornadoDataFinal),nrow(TornadoDataFinal)/(5/4))
test <- (-train)
test
train.TornadoDataFinal <-  TornadoDataFinal[train,]
test.TornadoDataFinal <- TornadoDataFinal[test,]
dim(train.TornadoDataFinal)
names(train.TornadoDataFinal)
dim(test.TornadoDataFinal)
TrainingData1 <- train.TornadoDataFinal
names(TrainingData1)
dim(TrainingData1)
summary(TrainingData1)
train1 <- sample(1:nrow(TrainingData1),nrow(TrainingData1)/(5/4))
valid1 <- (-train1)
valid1
train1.TrainingData1 <-  TrainingData1[train1,]
valid1.TrainingData1 <- TrainingData1[valid1,]
dim(train1.TrainingData1)
dim(valid1.TrainingData1)


#Linear Regression Model 1 (scaling predictor variables and including them)
lm.fit = lm(DamageProperty ~. , data = train1.TrainingData1)
summary(lm.fit)
dim(valid1.TrainingData1)
AIC(lm.fit)
BIC(lm.fit)
summary(valid1.TrainingData1)
dim(valid1.TrainingData1)
plot(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
lm.pred <- predict(lm.fit, valid1.TrainingData1, interval = "confidence")
lm.pred[1:50]
mean((lm.pred - valid1.TrainingData1$DamageProperty)^2)
(mean((lm.pred - valid1.TrainingData1$DamageProperty)^2)^(1/2))


#Ridge Regression (Applied to scaled data)
library(glmnet)
grid = 10^seq(10,-2,length=100)
x = model.matrix(DamageProperty~.,train1.TrainingData1)
y = train1.TrainingData1$DamageProperty
dim(train1.TrainingData1)
ridge.mod = glmnet(x, y, alpha=0, lambda=grid)
summary(ridge.mod)
ridge.mod
coef(ridge.mod)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
predict(ridge.mod, s=20, type="coefficients")[1:7,]
set.seed(1000)
train = sample(1:nrow(x), nrow(x)/2)
valid = (-train)
y.valid = y[valid]
y.valid
y.train = nrow(x) - y.valid
dim(y.train)
y.train = y[train]
ridge.mod = glmnet(x, y, alpha=0, lambda=grid)
ridge.pred = predict(ridge.mod, s=0, newx=x[valid,])
summary(ridge.pred)
mean((ridge.pred - y.valid)^2)
mean((mean(y[train])- y.valid)^2)
ridge.pred2 = predict(ridge.mod, s=0, newx=x[valid,], exact=T)
mean((ridge.pred2 - y.valid)^2)
(mean((ridge.pred2 - y.valid)^2))^(1/2)
lm(y~x, subset=train)
summary(lm(y~x, subset=train))


#Cross validation for choosing tuning parameter
set.seed(1000)
cv.out = cv.glmnet(x[train,],y[train], alpha=0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
ridge.pred.BEST = predict(ridge.mod, s=bestlam, newx=x[valid,])
mean((ridge.pred.BEST - y.valid)^2)
(mean((ridge.pred.BEST - y.valid)^2)^(1/2))
out = glmnet(x,y,alpha=0) 
predict(out,type="coefficients",s=bestlam)[1:7,]


#The Lasso (Applied to scaled data)
par(mfrow(1,1))
par(mfrow = c(1,1))
lasso.mod = glmnet(x[train,],y[train], alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1000)
cv.out = cv.glmnet(x[train,],y[train], lambda = grid)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
lasso.pred = predict(lasso.mod, s=bestlam, newx=x[valid,])
mean(lasso.pred)
mean((lasso.pred - y.valid)^2)
(mean((lasso.pred - y.valid)^2)^(1/2))
out2 = glmnet(x,y,alpha=0,lambda=grid)
lasso.coef = predict(out2, type="coefficients", s=bestlam)[1:7,]
lasso.coef


#The Lasso model on test data for inference
lasso.modF = glmnet(x1[train,],y1[train], alpha=1,lambda=grid)
plot(lasso.modF)
set.seed(1000)
cv.outL = cv.glmnet(x1[train,],y1[train], lambda = grid)
plot(cv.outL)
bestlamL = cv.outL$lambda.min
bestlamL
lasso.predF = predict(lasso.modF, s=bestlamL, newx=x1[valid,])
mean(lasso.predF)
mean((lasso.predF - y1.valid)^2)
outF2 = glmnet(x1,y1,lambda=grid)
lasso.coefF = predict(outF2, type="coefficients", s=bestlamL)[1:4,]
lasso.coefF
summary(TornadoDataFinal)


#Principal Components Regression(for scaled variables)
library(pls)
set.seed(2)
pcr.fit = pcr(DamageProperty ~ .,data=train1.TrainingData1,
              scale=TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type = "MSEP")
set.seed(1)
pcr.fit2 = pcr(DamageProperty ~., data=train1.TrainingData1,
               subset=train, scale=TRUE, validation="CV")
summary(pcr.fit2)
plot(pcr.fit2)
validationplot(pcr.fit2,val.type="MSEP")
pcr.pred2 = predict(pcr.fit2, valid1.TrainingData1, ncomp = 5)
y.valid <- valid1.TrainingData1$DamageProperty
mean((pcr.pred2 - y.valid)^2)
(mean((pcr.pred2 - y.valid)^2)^(1/2))
pcr.fit$coef


#Partial least Squares Regression (on scaled data)
set.seed(1000)
library(pls)
pls.fit = plsr(DamageProperty ~., data = train1.TrainingData1,
               subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)
pls.pred = predict(pls.fit, data = valid1.TrainingData1, ncomp=5)
y.valid <- valid1.TrainingData1$DamageProperty
mean((pls.pred - y.valid)^2)
pls.fit$coef
coef(pls.fit)
plot(pls.fit)
validationplot(pls.fit,val.type="MSEP")


#GAM(scaled data)
library(gam)
gam1 = lm(DamageProperty ~ ns(TornadoLength,4) + ns(TornadoWidth,4) 
        + ns(DeathsIndirect,4) + ns(DeathsDirect,4) + ns(InjuriesIndirect,4),data = train1.TrainingData1)       
summary(gam1)
par(mfrow = c(1,1))          
plot(gam1, se=TRUE, col="red")
gam2 = lm(DamageProperty ~ ns(TornadoLength,5) + ns(TornadoWidth,5) 
          + ns(InjuriesIndirect, 5) + ns(DeathsDirect, 5) + ns(DeathsIndirect, 5),data = train1.TrainingData1)                                                            
summary(gam2)
par(mfrow = c(1,1))          
plot(gam2, se=TRUE, col="blue")


#Comparing the fitted GAM models with anova
anova(gam1, gam2, test="F")
plot(anova(gam1,gam2, test="F"))


#Prediction with GAM 1
summary(gam1)
preds = predict(gam1, newdata = valid1.TrainingData1)
preds[1:30]
plot(preds)
y.valid <- valid1.TrainingData1$DamageProperty
mean((preds - y.valid)^2)
(mean((preds - y.valid)^2))^(1/2)
preds$coef
gam2$coef


#Decision Trees
set.seed(1000)
library(tree)
tree.PD = tree(DamageProperty ~., data = train1.TrainingData1)
summary(tree.PD)
par(mfrow=c(1,2))
plot(tree.PD)
text(tree.PD, pretty=0)
cv.PD = cv.tree(tree.PD)
plot(cv.PD$size, cv.PD$dev, type="b")
prune.PD = prune.tree(tree.PD, best=2)
plot(prune.PD)
text(prune.PD, pretty=0)
yhat = predict(tree.PD, newdata = valid1.TrainingData1)
PD.valid = valid1.TrainingData1$DamageProperty 
plot(yhat, PD.valid)
abline(0,1)
mean((yhat - PD.valid)^2)
(mean((yhat - PD.valid)^2))^(1/2)


#Bagging
names(train1.TrainingData1)
library(randomForest)
set.seed(1000)
bag.PD = randomForest(DamageProperty ~ . ,data = train1.TrainingData1,
                      importance = TRUE)
bag.PD
plot(bag.PD)
yhat.bag = predict(bag.PD, newdata = valid1.TrainingData1)
PD.valid = valid1.TrainingData1$DamageProperty
plot(yhat.bag, PD.valid)
abline(0,1)
mean((yhat.bag - PD.valid)^2)
(mean((yhat - PD.valid)^2))^(1/2)


#RandomForest(for scaled values)
set.seed(1000)
rf.PD = randomForest(DamageProperty ~ . ,data = train1.TrainingData1,
                    importance = TRUE)
rf.PD
yhat.rf = predict(rf.PD, newdata = valid1.TrainingData1)
plot(rf.PD)
PD.valid = valid1.TrainingData1$DamageProperty 
plot(yhat.rf, PD.valid)
abline(0,1)
mean((yhat.rf - PD.valid)^2)
(mean((yhat.rf - PD.valid)^2))^(1/2)


#Boosting
library(gbm)
set.seed(1000)
?gbm
boost.PD = gbm(DamageProperty ~ . ,data = train1.TrainingData1,
              distribution = "gaussian", n.trees = 5000, interaction.depth = 3,
              cv.folds = 10)
boost.PD
summary(boost.PD)
par(mfrow=c(1,2))
plot(boost.PD, i="TornadoLength")
plot(boost.PD, i="TornadoWidth")
yhat.boost = predict(boost.PD, newdata = valid1.TrainingData1, n.trees = 100)
yhat.boost[1:20]
yhat.boost
summary(yhat.boost)
PD.valid = valid1.TrainingData1$DamageProperty
plot(yhat.boost, PD.valid)
abline(0,1)
mean((yhat.boost - PD.valid)^2)
(mean((yhat.boost - PD.valid)^2))^(1/2)


#Boosting model on test data - estimating property damage
train <- sample(1:nrow(TornadoDataFinal),nrow(TornadoDataFinal)/(5/4))
test <- (-train)
test
train.TornadoDataFinal <-  TornadoDataFinal[train,]
test.TornadoDataFinal <- TornadoDataFinal[test,]
dim(train.TornadoDataFinal)
names(train.TornadoDataFinal)
dim(test.TornadoDataFinal)
summary(test.TornadoDataFinal)
names(test.TornadoDataFinal)
summary(test.TornadoDataFinal)
boost.PDF = gbm(DamageProperty ~ . ,data = train.TornadoDataFinal,
               n.trees = 5000,
               interaction.depth = 3,distribution = "gaussian",
               cv.folds = 10)
boost.PDF
summary(boost.PDF)
yhat.boost.F = predict(boost.PDF, newdata = test.TornadoDataFinal, n.trees = 5000)
yhat.boost.F
summary(yhat.boost.F)
yhat.boost.F[1:20]
PD.test = test.TornadoDataFinal$DamageProperty
plot(yhat.boost.F, PD.test)
abline(0,1)
mean((yhat.boost.F - PD.test)^2)
summary(TornadoDataFinal)
