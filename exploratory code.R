#install packages 
install.packages("quantmod")
install.packages("fBasics")
#get data
library("quantmod")
getSymbols("GS",from="2016-03-15",to="2020-03-15",src="yahoo",auto.assign=TRUE)
#graph the prices
chartSeries(GS, theme="white")
# Filled Density Plot of stock price
d <- density(GS$GS.Adjusted)
plot(d, main="Kernel Density of TWLO daily price")
polygon(d, col="gray", border="blue")
#get statitistics of net return
library("fBasics")
Pvec<-as.vector(GS$GS.Adjusted)
#compute net return
Rvec<-Pvec[-1]/Pvec[-length(Pvec)]-1
#compute the log return
rvec<-diff(log(Pvec))
######all the following analysis is based on net return if not specified
#get basic statistics of net return 
basicStats(Rvec)
# Add a Normal Curve 
h<-hist(Rvec, breaks=10, col="yellow", xlab="net return",
        main="Histogram with Normal Curve")
xfit<-seq(min(Rvec),max(Rvec),length=40)
yfit<-dnorm(xfit,mean=mean(Rvec),sd=sd(Rvec))
yfit <- yfit*diff(h$mids[1:2])*length(Rvec)
lines(xfit, yfit, col="blue", lwd=2)
# Filled Density Plot of stock net return
m <- density(Rvec)
plot(m, main="Kernel Density of TWLO net return")
polygon(m, col="yellow", border="blue")

#Fitting the AR Model to the time series
AR <- arima(Rvec, order = c(1,0,0))
print(AR)
#plot the series along with the fitted values
ts.plot(Rvec)
AR_fit <- Rvec - residuals(AR)
points(AR_fit, type = "l", col = 2, lty = 2)
#plotting the series plus the forecast and 95% prediction intervals
ts.plot(Rvec)
AR_forecast <- predict(AR, n.ahead = 10)$pred
AR_forecast_se <- predict(AR, n.ahead = 10)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
#test the goodness of fit
# Find AIC of AR
AIC(AR)
# Find BIC of AR
BIC(AR)
