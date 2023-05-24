library(forecast)
library(ggplot2)
library(ggfortify)
library(tseries)
library(dplyr)
library(rugarch)
library(tsbox)
library(zoo)
library(Metrics)
library(gridExtra)
library(bayesforecast)
library(fPortfolio)
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
setwd("/Users/jekaterina/Desktop/Bakalauras/Data/Clean data")

#################################################################################

read_data <- function(ticker) {
  filename <- paste(ticker, '_daily.csv', sep='')
  df <- read.csv(filename, header=TRUE, sep=';')
  df$Date <- as.Date(df$Date, format="%Y-%m-%d")
  
  return (df)
}


price_data_prep <- function(df) {
  price <- ts(df[,'Price'])
  
  return (price)
}


price_plot <- function(ticker, df) {
  title <- paste(ticker, 'kaina')
  
  g1 = ggplot(df, aes(x=Date, y=Price))+geom_line()
  g1+ scale_x_date(limits = as.Date(c("2018-03-05","2023-03-02")))
  g1 + labs(x = "Data", y = "Kaina (USD)", title = title)
}


arima_model <- function(data) {
  arima <- auto.arima(data)
  print(summary(arima))
  
  p <- ggtsdisplay(residuals(arima), plot.type = "histogram")
  p <- p + labs(y = "Dažnis", x = "Liekanos") 
  print(p, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))
  
  return(arima)
}


arima_forecast_plot <- function(ticker, model, days, cryptodata, data) {
  fcst <- forecast(model, h=days)
  
  df <- data.frame(real = data,
                   pred = c(rep(NA,length(data)-length(data.frame(fcst)[,1])),data.frame(fcst)[,1]),
                   time = cryptodata$Date,
                   Lo80 =c(rep(NA,length(data)-length(data.frame(fcst)[,2])),data.frame(fcst)[,2]),
                   Hi80 =c(rep(NA,length(data)-length(data.frame(fcst)[,3])),data.frame(fcst)[,3]),
                   Lo95 =c(rep(NA,length(data)-length(data.frame(fcst)[,4])),data.frame(fcst)[,4]),
                   Hi95 =c(rep(NA,length(data)-length(data.frame(fcst)[,5])),data.frame(fcst)[,5])) 
  
  ggplot(df, aes(time, real, group = 1)) +
    geom_line() +
    geom_line(aes(time, pred, group = 1), color="blue")+
    geom_ribbon(aes(time, ymin = Lo95, ymax = Hi95), fill = "blue", alpha = 0.25) +
    geom_ribbon(aes(time, ymin = Lo80, ymax = Hi80), fill = "blue", alpha = 0.25) +
    theme_light() +
    labs(title=paste("ARIMA", ticker, days, "dienų kainų prognozė"),
         x ="Data", y = "Kaina (USD)")
}


garch_model <- function(armaorder, data) {
  spec <- ugarchspec(mean.model=list(armaOrder=armaorder))
  ugfit <- ugarchfit(spec=spec, data=data)
  ugfit
  
  var <- ugfit@fit$var
  res <- (ugfit@fit$residuals)^2
  plot(res, type='l')
  lines(var, col='green')
  # check if residuals are independently distributed
  Box.test(ugfit@fit$residuals, lag = 1, type = "Ljung-Box")
  
  return(ugfit)
}


garch_forecast <- function(fit, days, data, ticker) {
  garch_fcst <- ugarchforecast(fit, n.ahead=days)
  pred <- garch_fcst@forecast$seriesFor
  metrics <- calculate_metrics(pred, tail(data, days))
  return(metrics)
} 


garch_forecast_plot <- function(ticker, fit, days, cryptodata, data) {
  garch_fcst <- ugarchforecast(fit, n.ahead=days)
  pred <- garch_fcst@forecast$seriesFor
  pred <- as.data.frame(pred)
  names(pred) <- c("Forecast")
  df <- data.frame(real = data,
                   pred = c(rep(NA,(length(data)-length(pred$Forecast))), pred$Forecast),
                   time = cryptodata$Date)
  
  ggplot(df, aes(time, real, group = 1)) +
    geom_line() +
    geom_line(aes(time, pred, group = 1), color="blue")+
    theme_light() +
    labs(title=paste("GARCH", ticker, days, "dienų kainų prognozė"), x ="Data", y = "Kaina (USD)")
}

calculate_metrics <- function(pred, actual) {
  err <- pred - actual
  Q <- pred / actual
  rmse <- rmse(actual, pred)
  mae <- mae(actual, pred)
  mape <- mape(actual, pred)
  metrics <- cbind(rmse, mae, mape)
  return(metrics)
}


calculate_price_metrics <- function(pred, actual, cryptodata) {
  first_price <- cryptodata$Price[1]
  price <- exp(cumsum(actual)) * first_price
  price_pred <- exp(cumsum(pred)) * first_price
  rmse_price <- rmse(price, price_pred)
  mae_price <- mae(price, price_pred)
  mape_price <- mape(price, price_pred)
  metrics_price <- cbind(rmse_price, mae_price, mape_price)
  return(metrics_price)
}


return_data_prep <- function(cryptodata) {
  returns <- diff(log(cryptodata$Price))
  returns <- as.ts(returns)
  date <- data.frame(cryptodata$Date)
  returnsdata <- data.frame(date[-1, ], returns)
  names(returnsdata) <- c("Date", "Price")
  
  return(returnsdata)
}


returns_plot <- function(ticker, df) {
  title <- paste(ticker, 'grąža')
  
  g2 = ggplot(df, aes(x=Date, y=Price))+geom_line()
  g2 + scale_x_date(limits = as.Date(c("2018-03-07","2023-03-02")))
  g2 + labs(x = "Data", y = "Log grąža", title = title)
}


save_arima_results <- function(model, days, cryptodata, ticker) {
  pred <- forecast(model, h=days)
  pred <- as.data.frame(pred)
  result <- data.frame(Date = cryptodata$Date,
                       Real = cryptodata$Price,
                       Pred = c(rep(NA,(length(cryptodata$Price)-length(pred$`Point Forecast`))), pred$`Point Forecast`))
  filename <- paste(ticker, "forecast_week.csv", sep="_")
  write.csv(result, filename, row.names=FALSE)
}


save_garch_results <- function(model, days, cryptodata, ticker) {
  garch_fcst <- ugarchforecast(model, n.ahead=days)
  pred <- garch_fcst@forecast$seriesFor
  pred <- as.data.frame(pred)
  names(pred) <- c("Forecast")
  result <- data.frame(Date = cryptodata$Date,
                       Real = cryptodata$Price,
                       Pred = c(rep(NA,(length(cryptodata$Price)-length(pred$Forecast))), pred$Forecast))
  filename <- paste(ticker, "forecast_week.csv", sep = "_")
  write.csv(result, filename, row.names=FALSE)
}

read_forecast <- function(ticker) {
  filename <- paste(ticker, '_forecast_week.csv', sep='')
  df <- read.csv(filename, header=TRUE, sep=',')
  df$Date <- as.Date(df$Date, format="%Y-%m-%d")
  
  return (df)
}


data_preparation <- function(names) {
  for (i in 1:length(names)) {
    if (i==1) {
      FCST <- read_forecast(names[i])
      colnames(FCST)[1] <- "Date"
      colnames(FCST)[ncol(FCST)-1] <- paste(names[i], "real")
      colnames(FCST)[ncol(FCST)] <- paste(names[i], "pred")
    }
    
    else {
      df <- read_forecast(names[i])
      FCST <- cbind(FCST, df$Real, df$Pred)
      colnames(FCST)[ncol(FCST)-1] <- paste(names[i], "real")
      colnames(FCST)[ncol(FCST)] <- paste(names[i], "pred")
    }
  }
  
  return(FCST)
}

get_prediction <- function(names) {
  for (i in 1:length(names)) {
    if (i==1) {
      df <- read_forecast(names[i])
      FCST <- cbind(df$Date, df$Pred)
      FCST <- as.data.frame(FCST)
      colnames(FCST) <- c("Date", paste(names[i], "pred"))
      FCST$Date <- as.Date(FCST$Date, format="%Y-%m-%d")
    }
    
    else {
      df <- read_forecast(names[i])
      FCST <- cbind(FCST, df$Pred)
      colnames(FCST)[ncol(FCST)] <- paste(names[i], "pred")
    }
  }
  
  return(FCST)
}

#####################################################################

BTC <- read_data("BTC")
BTCprice <- price_data_prep(BTC)
length(BTC[,1])
split <- length(BTC[,1]) - 7
BTCtrain <- head(BTC, split)
BTCtest <- tail(BTC, length(BTC[,1])-split)
days <- length(BTCtest[,1])

BTC_arima_model <- arima_model(BTCtrain$Price)
arima_forecast_plot('BTC', BTC_arima_model, days, BTC, BTCprice)

save_arima_results(BTC_arima_model, days, BTC, "BTC")

#################################################################################

ETH <- read_data("ETH")
ETHprice <- price_data_prep(ETH)
split <- length(ETH[,1]) - 7
ETHtrain <- head(ETH, split)
ETHtest <- tail(ETH, length(ETH[,1])-split)
days <- length(ETHtest[,1])

ETH_garch_model1 <- garch_model(c(1,1), ETHtrain$Price)
ETH_garch_model1_metrics <- garch_forecast(ETH_garch_model1, days, ETHprice, "ETH")
garch_forecast_plot("ETH", ETH_garch_model1, days, ETH, ETHprice)

save_garch_results(ETH_garch_model1, days, ETH, "ETH")

#################################################################################

BNB <- read_data("BNB")
BNBprice <- price_data_prep(BNB)
split <- length(BNB[,1]) - 7
BNBtrain <- head(BNB, split)
BNBtest <- tail(BNB, length(BNB[,1])-split)
days <- length(BNBtest[,1])

BNB_garch_model2 <- garch_model(c(1,0), BNBtrain$Price)
BNB_garch_model2_metrics <- garch_forecast(BNB_garch_model2, days, BNBprice, "BNB")
garch_forecast_plot("BNB", BNB_garch_model2, days, BNB, BNBprice)

save_garch_results(BNB_garch_model2, days, BNB, "BNB")

#################################################################################

XRP <- read_data("XRP")
XRPprice <- price_data_prep(XRP)
split <- length(XRP[,1]) - 7
XRPtrain <- head(XRP, split)
XRPtest <- tail(XRP, length(XRP[,1])-split)
days <- length(XRPtest[,1])

XRP_arima_model <- arima_model(XRPtrain$Price)
arima_forecast_plot('XRP', XRP_arima_model, days, XRP, XRPprice)

save_arima_results(XRP_arima_model, days, XRP, "XRP")

#################################################################################

ADA <- read_data("ADA")
ADAprice <- price_data_prep(ADA)
split <- length(ADA[,1]) - 7
ADAtrain <- head(ADA, split)
ADAtest <- tail(ADA, length(ADA[,1])-split)
days <- length(ADAtest[,1])

ADA_garch_model1 <- garch_model(c(1,1), ADAtrain$Price)
ADA_garch_model1_metrics <- garch_forecast(ADA_garch_model1, days, ADAprice, "ADA")
garch_forecast_plot("ADA", ADA_garch_model1, days, ADA, ADAprice)

save_garch_results(ADA_garch_model1, days, ADA, "ADA")

#################################################################################

DOGE <- read_data("DOGE")
DOGEprice <- price_data_prep(DOGE)
split <- length(DOGE[,1]) - 7
DOGEtrain <- head(DOGE, split)
DOGEtest <- tail(DOGE, length(DOGE[,1])-split)
days <- length(DOGEtest[,1])

DOGE_garch_model1 <- garch_model(c(1,1), DOGEtrain$Price)
DOGE_garch_model1_metrics <- garch_forecast(DOGE_garch_model1, days, DOGEprice, "DOGE")
garch_forecast_plot("DOGE", DOGE_garch_model1, days, DOGE, DOGEprice)

save_garch_results(DOGE_garch_model1, days, DOGE, "DOGE")

#################################################################################

LTC <- read_data("LTC")
LTCprice <- price_data_prep(LTC)
split <- length(LTC[,1]) - 7
LTCtrain <- head(LTC, split)
LTCtest <- tail(LTC, length(LTC[,1])-split)
days <- length(LTCtest[,1])

LTC_arima_model <- arima_model(LTCtrain$Price)
arima_forecast_plot('LTC', LTC_arima_model, days, LTC, LTCprice)

save_arima_results(LTC_arima_model, days, LTC, "LTC")

#################################################################################

TRX <- read_data("TRX")
TRXprice <- price_data_prep(TRX)
split <- length(TRX[,1]) - 7
TRXtrain <- head(TRX, split)
TRXtest <- tail(TRX, length(TRX[,1])-split)
days <- length(TRXtest[,1])

TRX_arima_model <- arima_model(TRXtrain$Price)
arima_forecast_plot('TRX', TRX_arima_model, days, TRX, TRXprice)

save_arima_results(TRX_arima_model, days, TRX, "TRX")

#################################################################################

LINK <- read_data("LINK")
LINKprice <- price_data_prep(LINK)
split <- length(LINK[,1]) - 7
LINKtrain <- head(LINK, split)
LINKtest <- tail(LINK, length(LINK[,1])-split)
days <- length(LINKtest[,1])

LINK_garch_model1 <- garch_model(c(1,1), LINKtrain$Price)
LINK_garch_model1_metrics <- garch_forecast(LINK_garch_model1, days, LINKprice, "LINK")
garch_forecast_plot("LINK", LINK_garch_model1, days, LINK, LINKprice)

save_garch_results(LINK_garch_model1, days, LINK, "LINK")

#################################################################################

BCH <- read_data("BCH")
BCHprice <- price_data_prep(BCH)
split <- length(BCH[,1]) - 7
BCHtrain <- head(BCH, split)
BCHtest <- tail(BCH, length(BCH[,1])-split)
days <- length(BCHtest[,1])

BCH_arima_model <- arima_model(BCHtrain$Price)
arima_forecast_plot('BCH', BCH_arima_model, days, BCH, BCHprice)

save_arima_results(BCH_arima_model, days, BCH, "BCH")

#################################################################################

EOS <- read_data("EOS")
EOSprice <- price_data_prep(EOS)
split <- length(EOS[,1]) - 7
EOStrain <- head(EOS, split)
EOStest <- tail(EOS, length(EOS[,1])-split)
days <- length(EOStest[,1])

EOS_arima_model <- arima_model(EOStrain$Price)
arima_forecast_plot('EOS', EOS_arima_model, days, EOS, EOSprice)

save_arima_results(EOS_arima_model, days, EOS, "EOS")

#################################################################################

OMG <- read_data("OMG")
OMGprice <- price_data_prep(OMG)
split <- length(OMG[,1]) - 7
OMGtrain <- head(OMG, split)
OMGtest <- tail(OMG, length(OMG[,1])-split)
days <- length(OMGtest[,1])

OMG_arima_model <- arima_model(OMGtrain$Price)
arima_forecast_plot('OMG', OMG_arima_model, days, OMG, OMGprice)

save_arima_results(OMG_arima_model, days, OMG, "OMG")

#################################################################################

XMR <- read_data("XMR")
XMRprice <- price_data_prep(XMR)
split <- length(XMR[,1]) - 7
XMRtrain <- head(XMR, split)
XMRtest <- tail(XMR, length(XMR[,1])-split)
days <- length(XMRtest[,1])

XMR_garch_model2 <- garch_model(c(1,0), XMRtrain$Price)
XMR_garch_model2_metrics <- garch_forecast(XMR_garch_model2, days, XMRprice, "XMR")
garch_forecast_plot("XMR", XMR_garch_model2, days, XMR, XMRprice)

save_garch_results(XMR_garch_model2, days, XMR, "XMR")

#################################################################################

ZEC <- read_data("ZEC")
ZECprice <- price_data_prep(ZEC)
split <- length(ZEC[,1]) - 7
ZECtrain <- head(ZEC, split)
ZECtest <- tail(ZEC, length(ZEC[,1])-split)
days <- length(ZECtest[,1])

ZEC_arima_model <- arima_model(ZECtrain$Price)
arima_forecast_plot('ZEC', ZEC_arima_model, days, ZEC, ZECprice)

save_arima_results(ZEC_arima_model, days, ZEC, "ZEC")

#################################################################################

names <- c("BTC", "ETH", "BNB", "XRP", "ADA", "DOGE", "LTC", "TRX", "LINK", "BCH", "EOS", "OMG", "XMR", "ZEC")
allData <- data_preparation(names)
forecasts <- get_prediction(names)
forecasts <- na.omit(forecasts)
FCST <- xts(forecasts[,2:15], order.by=forecasts$Date)
FCST <- as.timeSeries(FCST)
portData <- portfolioData(data = FCST, spec = portfolioSpec())

returns <- Return.calculate(forecasts, method='discrete')
returns <- na.omit(returns)
returns$Date <- rownames(returns)
returns$Date <- as.Date(returns$Date, format="%Y-%m-%d")
RTRN <- xts(returns[,1:14], order.by = returns$Date)
RTRN <- as.timeSeries(RTRN)
portRetData <- portfolioData(data = RTRN, spec = portfolioSpec())

# PORTFOLIO OPTIMIZATION #

# First portfolio #

portf1 <- portfolio.spec(names)
portf1 <- add.constraint(portf1, type="weight_sum", 
                         min_sum=0.99, max_sum=1.01)
portf1 <- add.constraint(portf1, type="box", min=0, max=0.5)
portf1 <- add.constraint(portf1, type="position_limit", max_pos=14)

rp1 <- random_portfolios(portf1, 100, "sample")
opt1 <- optimize.portfolio(RTRN, portfolio=portf1, 
                           optimize_method="random", rp=rp1,
                           trace=TRUE)
print(opt1)

port.ret1 <- Return.portfolio(RTRN, opt1$weights)
print(port.ret1)
chart.Weights(opt1)

charts.PerformanceSummary(port.ret1)
chart.CumReturns(port.ret1, main="Grąža, strategija 1, port. 1")

VaR(RTRN, p=0.95, method="gaussian", portfolio_method="component", weights=opt1$weights)


# Second portfolio #
portf2 <- portfolio.spec(names)
portf2 <- add.constraint(portf2, type="weight_sum", 
                         min_sum=0.99, max_sum=1.01)
portf2 <- add.constraint(portf2, type="box", min=0, max=0.5)
portf2 <- add.objective(portf2, type="return", name="mean",
                        target=0.0015)
portf2 <- add.objective(portf2, type="risk", name="StdDev",
                        target=0.02)
rp2 <- random_portfolios(portf2, 20, "sample")
opt2 <- optimize.portfolio(RTRN, portfolio=portf2,
                           optimize_method="random", rp=rp2,
                           trace=TRUE)
print(opt2)

port.ret2 <- Return.portfolio(RTRN, opt2$weights)
print(port.ret2) # returns are positive but very small
chart.Weights(opt2)

charts.PerformanceSummary(port.ret2)
chart.CumReturns(port.ret2, main="Grąža, strategija 1, port. 2")

VaR(RTRN, p=0.95, method="gaussian", portfolio_method="component", weights=opt2$weights)

# Third portfolio - not used later#

portf3 <- portfolio.spec(names) 
portf3 <- add.constraint(portf3, type="weight_sum", 
                         min_sum=0.99, max_sum=1.01)
portf3 <- add.constraint(portf3, type="box", min=0, max=0.5)
portf3 <- add.constraint(portf3, type="position_limit", max_pos=10)
portf3 <- add.objective(portf3, type="return", name="mean",
                        target=0.01)
portf3 <- add.objective(portf3, type="risk", name="StdDev",
                        target=0.02)
rp3 <- random_portfolios(portf3, 20, "sample")
opt3 <- optimize.portfolio(RTRN, portfolio=portf3,
                           optimize_method="random", rp=rp3,
                           trace=TRUE)
print(opt3)

port.ret3 <- Return.portfolio(RTRN, opt3$weights)
print(port.ret3)
chart.Weights(opt3)

charts.PerformanceSummary(port.ret3)
chart.CumReturns(port.ret3, main="Grąža, strategija 1, port. 3")

VaR(RTRN, p=0.95, method="gaussian", portfolio_method="component", weights=opt3$weights)


