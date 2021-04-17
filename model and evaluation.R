library(quantmod)
library(ggplot2)
library(magrittr)
library(broom)


getSymbols(c("GS", "SPY"),from="2007-03-16",to="2021-03-16",src="yahoo",auto.assign=TRUE)

#head(SPY)

#making a data frame that captures the rows with just the adjusted price of each ticker

stocks = as.xts(data.frame(GS = GS[, "GS.Adjusted"], SPY = SPY[, "SPY.Adjusted"]))

names(stocks) = c("Goldman Sachs", "S&P500")
index(stocks) = as.Date(index(stocks))

stocks_series = tidy(stocks) %>% 
  
  ggplot(aes(x=index,y=value, color= series)) + geom_line() +
  
  labs(title = "GS Share Price Changes", colours = " ") + 
  xlab("Year") + ylab("End of Day Adjusted Prices")

stocks_series

chartSeries()

# Calculating the monthly returns for GS and its competitors
library(tidyquant)
library(timetk)
library(fBasics)

GS <- tq_get("GS", from = '2007-03-16', to = "2021-03-16", get = "stock.prices")


GS_daily_returns <- GS %>%
  tq_transmute(select = adjusted,             
               mutate_fun = periodReturn,  
               period = "daily",     
               col_rename = "GS_returns")

GS_daily_returns %>%
  ggplot(aes(x = date, y = GS_returns)) +
  geom_line() +
  theme_classic() +
  labs(x = "Date", y = "Daily returns") +
  ggtitle("Daily Returns for Goldman Sachs") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-0.5,0.6,0.05),
                     labels = scales::percent) 

basicStats(GS_daily_returns$GS_returns)

basicStats(C_daily_returns$C_returns)

basicStats(MS_daily_returns$MS_returns)

# --------------------- fit the model--------------

library("quantmod")
getSymbols("GS",from="2017-03-15",to="2021-03-15",src="yahoo",auto.assign=TRUE)
Pvec<-as.vector(GS$GS.Adjusted)
rvec<-diff(log(Pvec))
plot(rvec)

#first observe the general trend to see if it is stationary and conduct some tests
acf(rvec, main="Sample ACF for GS daily log-returns")
Box.test(rvec, lag = 10, type = "Ljung-Box") 

#fit the AR(1) model
fitAR1 = arima(rvec, order = c(1,0,0))
print(fitAR1)
acf(resid(fitAR1), main="Sample ACF for the residuals")

#using auto.arima to find better fit
library(forecast) 
auto.arima(rvec, max.p = 20, max.q = 0, d = 0, ic = "aic")
auto.arima(rvec, max.p = 20, max.q = 0, d = 0, ic = "bic")
#auto.arima suggested order (1,0,0) so no need to change the model


summary(fitAR1)
tsdisplay(fitAR1$residuals, lag.max = 30)
hist(fitAR1$residuals)
mean(fitAR1$residuals)
var(fitAR1$residuals)

#----------------esidual diagnostic to validate the model-------------
#(1)check if the residuals are stationary.
## timeseries plot of the residuals indicates stationarity
AR1_resid = resid(fitAR1)
plot(AR1_resid, main = "plot of residuals")

#(2)check if the residuals are weak WN
Box.test(AR1_resid, lag = 5, type = "Ljung-Box", fitdf = 2)
acf(AR1_resid, main="Sample ACF for the residuals")

#(3)check if the volatility is constant
## plot timeseries of residual^2 and fit a scatter plot smoother to highlight changes
##reuslt:  The volatility seems to be changing.
par(mfrow=c(1,2));par(mar=c(3,3,3,3))
plot(resid(fitAR1)**2, type="l", col=1, main = expression(residual^2))
smoother = loess((resid(fitAR1)**2) ~ seq(1,length(resid(fitAR1))), span=0.1)
lines(seq(1,length(resid(fitAR1)),fitted(smoother),col=2)

#We also check for autocorrelation in the squared residuals by using an ACF plot of residual^2
##result: The squared residuals seemed to be slightly autocorrelated
acf((resid(fitAR1)**2), main=expression("sample ACF of "~ residual^2))
#Ljung Box test result: no autocorrelation in the squared residuals
Box.test(resid(fitAR1)**2, lag = 10, type = "Ljung-Box", fitdf = 1)

#(4)check normality of residual
qqnorm(AR1_resid, datax = TRUE,
       xlab = "normal quantile",
       ylab = "sample quantile of residuals",
       main = "normal probability plot for residuals")
qqline(AR1_resid, datax=TRUE, col = 2)
##result indicate non-normality. This is expected, since the volatility is not constant

#(5) check Heteroskedasticity of residuals


