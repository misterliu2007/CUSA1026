n = 200

# simulate White-noise processes
par(mfrow=c(1,3))
for(i in 1:3){
  # Generate and plot white noise
  WN <- arima.sim(model=list(order=c(0,0,0)),n=n)
  plot(WN)
}

# simulate MA(1) processes
par(mfrow=c(1,3))
for(i in 1:3){
  # Generate and plot an MA(1)
  MA1 <- arima.sim(model=list(order=c(0,0,1), ma=0.9),n=n)
  plot(MA1)
}
plot(MA1[1:(n-1)],MA1[2:n])
plot(MA1[1:(n-2)],MA1[3:n])

# simulate MA(2) processes
par(mfrow=c(1,3))
for(i in 1:3){
  # Generate and plot an MA(2) 
  MA2 <- arima.sim(model=list(order=c(0,0,2), ma=c(-1,0.6)),n=n)
  plot(MA2)
}
plot(MA2[1:(n-1)],MA2[2:n])
plot(MA2[1:(n-2)],MA2[3:n])
plot(MA2[1:(n-3)],MA2[4:n])


# simulate AR(1) processes
par(mfrow=c(2,4))
phi = c(-0.5,-0.8,0.4,0.9)
for(i in 1:4){
  # Generate and plot an AR(2)
  AR1 <- arima.sim(model=list(order=c(1,0,0), ar=phi[i]),n=n)
  plot(AR1)
  acf(AR1)
}
par(mfrow=c(1,1))


# simulate ARIMA(2,2) processes
par(mfrow=c(1,3))
# Generate and plot an ARIMA(0,2,2) 
ARIMA <- arima.sim(model=list(order=c(0,2,2), ma=c(-0.5,0.1)),n=n)
plot(ARIMA)
plot(diff(ARIMA,differences = 1))
plot(diff(ARIMA,differences = 2))

par(mfrow=c(1,1))

###############################
# Alibaba (9988.HK)
###############################
# https://hk.finance.yahoo.com/quote/9988.HK/history?p=9988.HK
alibaba = read.csv('9988.HK_d.csv')
alibaba
n  = nrow(alibaba)
alibaba$Date = as.Date(alibaba$Date, format="%Y-%m-%d")
alibaba

# plot open and close price of 9988.HK
plot(x=alibaba$Date, y=alibaba$Close, type="l", ylab="Price", xlab="Date")
lines(x=alibaba$Date, y=alibaba$Open, type="l", col="red")
legend(alibaba$Date[1],70,legend=c("Close", "Open"), col=c("black","red"), lty=c(1,1))

# plot the difference of close price of 9988.HK
alibaba$Close_diff = c(NA,diff(alibaba$Close))
plot(x=alibaba$Date, y=alibaba$Close_diff, type="l", ylab="diff(Price)", xlab="Date")

par(mfrow=c(1,2))
acf(alibaba$Close_diff, na.action=na.pass, main="ACF", ylim=c(-1,1))
pacf(alibaba$Close_diff, na.action=na.pass, main="PACF", ylim=c(-1,1))
par(mfrow=c(1,1))

###############################
# Predict Snowfall
###############################
# https://www.data.jma.go.jp/obd/stats/etrn/view/monthly_s3_en.php?block_no=47412&view=14
sapporo = tail(read.csv("sapporo.csv"),10)
sapporo = sapporo[,-ncol(sapporo)]
sapporo[is.na(sapporo)] = 0
head(sapporo)


library(tidyverse)
sapporo2 <- sapporo |> gather(key="time", value="snow", -c(Year)) |> arrange(Year)
sapporo2$date <- as.Date(paste(sapporo2$Year,sapporo2$time, 1), format="%Y %b %d")

plot(x=sapporo2$date, y=sapporo2$snow, type="l", 
     ylab="Monthly total of snowfall depth (cm)",
     xlab="Time")
par(mfrow=c(1,2))
acf(sapporo2$snow, na.action=na.pass, main="ACF", ylim=c(-1,1))
pacf(sapporo2$snow, na.action=na.pass, main="PACF", ylim=c(-1,1))
par(mfrow=c(1,1))

library(forecast)
fit = arima(sapporo2$snow, order=c(0,0,0), 
            seasonal=list(order=c(1,0,0),period=12))
fit
plot(x=sapporo2$date, y=sapporo2$snow,type="l")
lines(x=sapporo2$date,fitted(fit), col="red")

plot(forecast(fit,h=24),ylab="Monthly total of snowfall depth (cm)",)

prediction = data.frame(forecast(fit,h=24))
prediction$date = seq.Date(from=as.Date("2024-1-1"), by="month", length.out=24)

plot(x=prediction$date,
     y=prediction$Point.Forecast, type="l",
     xaxt = "n",
     ylab="Monthly total of snowfall depth (cm)",
     xlab="Time")
axis(1,                                                   # Add dates to x-axis
     prediction$date,
     format(prediction$date, "%Y-%m-%d"))

###############################
# Predict temperature
###############################
# https://www.hko.gov.hk/en/abouthko/opendata_intro.htm
temp = tail(read.csv("sai_kung.csv"),360*4)
temp$date=as.Date(paste0(temp$Year,"/",temp$Month,"/",temp$Day), format="%Y/%m/%d")

# temp = temp[temp$Day==1,]
temp$Temperature = as.numeric(temp$Temperature)
plot(x=temp$date, y=temp$Temperature, type="l",
     ylab="Daily Temperature in Celsius in Sai Kung",
     xlab="Year")

par(mfrow=c(1,2))
acf(temp$Temperature, na.action=na.pass, main="ACF", ylim=c(-1,1))
pacf(temp$Temperature, na.action=na.pass, main="PACF", ylim=c(-1,1))
par(mfrow=c(1,1))

fit = arima(temp$Temperature, order=c(4,0,0))
fit
plot(x=temp$date,y=temp$Temperature,type="l",
     ylab="Daily Temperature in Celsius in Sai Kung",
     xlab="Year")
lines(x=temp$date,y=fitted(fit), col="red")
predict = forecast(fit,h=20)
plot(forecast(fit,h=400))


fit = arima(temp$Temperature, order=c(0,0,0), seasonal=list(order=c(2,0,2), period=12))
fit
predict = forecast(fit,h=20)
plot(forecast(fit,h=400))

acf(temp$Temperature, na.action=na.pass)
pacf(temp$Temperature, na.action=na.pass)

plot(temp$Temperature,type="l", xlim=c(0,400))
lines(fitted(fit), col="red")
predict = forecast(fit,h=20)
plot(forecast(fit,h=40))
lines(predict$mean)







library(tidyverse)
sapporo2 <- sapporo |> gather(key="time", value="snow", -c(Year)) |> arrange(Year)
sapporo2$date <- as.Date(paste(sapporo2$Year,sapporo2$time, 1), format="%Y %b %d")

plot(x=sapporo2$date, y=sapporo2$snow, type="l", 
     ylab="Monthly total of snowfall depth (cm)",
     xlab="Time")
par(mfrow=c(1,2))
acf(sapporo2$snow, na.action=na.pass, main="ACF", ylim=c(-1,1))
pacf(sapporo2$snow, na.action=na.pass, main="PACF", ylim=c(-1,1))
par(mfrow=c(1,1))

library(forecast)
fit = arima(sapporo2$snow, order=c(0,0,0), 
            seasonal=list(order=c(1,0,0),period=12))
fit
plot(x=sapporo2$date, y=sapporo2$snow,type="l")
lines(x=sapporo2$date,fitted(fit), col="red")

plot(forecast(fit,h=24),ylab="Monthly total of snowfall depth (cm)",)

prediction = data.frame(forecast(fit,h=24))
prediction$date = seq.Date(from=as.Date("2024-1-1"), by="month", length.out=24)

plot(x=prediction$date,
     y=prediction$Point.Forecast, type="l",
     xaxt = "n",
     ylab="Monthly total of snowfall depth (cm)",
     xlab="Time")
axis(1,                                                   # Add dates to x-axis
     prediction$date,
     format(prediction$date, "%Y-%m-%d"))


