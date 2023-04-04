library(forecast)
library(ggplot2)
library(ggfortify)
library(tseries)
library(dplyr)
library(rugarch)
library(tsbox)
library(zoo)
library(Metrics)
library(bayesforecast)
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
  filename <- paste(ticker, "forecast.csv", sep="_")
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
  filename <- paste(ticker, "forecast.csv", sep = "_")
  write.csv(result, filename, row.names=FALSE)
}

#################################################################################
# BTC price data

BTC <- read_data("BTC")
BTCprice <- price_data_prep(BTC)
split <- as.integer(length(BTC[,1])*0.8)
BTCtrain <- head(BTC, split)
BTCtest <- tail(BTC, length(BTC[,1])-split)
days <- length(BTCtest[,1])

price_plot('BTC', BTC)

# ARIMA model - ARIMA (0,1,2) - first differencing
BTC_arima_model <- arima_model(BTCtrain$Price)
arima_forecast_plot('BTC', BTC_arima_model, days, BTC, BTCprice)
# predicts const

# GARCH
BTC_garch_model1 <- garch_model(c(1,1), BTCtrain$Price)
BTC_garch_model1_metrics <- garch_forecast(BTC_garch_model1, days, BTCprice, "BTC")
garch_forecast_plot("BTC", BTC_garch_model1, days, BTC, BTCprice)

BTC_garch_model2 <- garch_model(c(1,0), BTCtrain$Price)
BTC_garch_model2_metrics <- garch_forecast(BTC_garch_model2, days, BTCprice, "BTC")
garch_forecast_plot("BTC", BTC_garch_model2, days, BTC, BTCprice)

BTC_garch_model3 <- garch_model(c(0,1), BTCtrain$Price)
BTC_garch_model3_metrics <- garch_forecast(BTC_garch_model3, days, BTCprice, "BTC")
garch_forecast_plot("BTC", BTC_garch_model3, days, BTC, BTCprice)

BTC_garch_model4 <- garch_model(c(1,2), BTCtrain$Price)
BTC_garch_model4_metrics <- garch_forecast(BTC_garch_model4, days, BTCprice, "BTC")
garch_forecast_plot("BTC", BTC_garch_model4, days, BTC, BTCprice)
# best model according to the metrics, but predicts const

#ARIMA is better for prices

save_arima_results(BTC_arima_model, days, BTC, "BTC")
#################################################################################
# BTC returns data

BTCreturns <- return_data_prep(BTC)
BTCrettrain <- head(BTCreturns, split)
BTCrettest <- tail(BTCreturns, length(BTCreturns[,1])-split)
returns_plot("BTC", BTCreturns)

# ARIMA model - ARIMA(3,0,4)
BTC_returns_arima_model <- arima_model(BTCrettrain$Price)
arima_forecast_plot('BTC', BTC_returns_arima_model, days, BTCreturns, BTCreturns$Price)

# GARCH
BTC_returns_garch_model1 <- garch_model(c(1,1), BTCrettrain$Price)
BTC_returns_garch_model1_metrics <- garch_forecast(BTC_returns_garch_model1, days, BTCreturns$Price, "BTC")
garch_forecast_plot("BTC", BTC_returns_garch_model1, days, BTCreturns, BTCreturns$Price)
# best model according to the metrics, but predicts const

BTC_returns_garch_model2 <- garch_model(c(1,0), BTCrettrain$Price)
BTC_returns_garch_model2_metrics <- garch_forecast(BTC_returns_garch_model2, days, BTCreturns$Price, "BTC")
garch_forecast_plot("BTC", BTC_returns_garch_model2, days, BTCreturns, BTCreturns$Price)

BTC_returns_garch_model3 <- garch_model(c(0,1), BTCrettrain$Price)
BTC_garch_model3_metrics <- garch_forecast(BTC_returns_garch_model3, days, BTCreturns$Price, "BTC")
garch_forecast_plot("BTC", BTC_returns_garch_model3, days, BTCreturns, BTCreturns$Price)

BTC_returns_garch_model4 <- garch_model(c(1,2), BTCreturns$Price)
BTC_garch_model4_metrics <- garch_forecast(BTC_returns_garch_model4, days, BTCreturns$Price, "BTC")
garch_forecast_plot("BTC", BTC_returns_garch_model4, days, BTCreturns, BTCreturns$Price)

#################################################################################
# ETH price data

ETH <- read_data("ETH")
ETHprice <- price_data_prep(ETH)
split <- as.integer(length(ETH[,1])*0.8)
ETHtrain <- head(ETH, split)
ETHtest <- tail(ETH, length(ETH[,1])-split)
days <- length(ETHtest[,1])

price_plot('ETH', ETH)

# ARIMA model - ARIMA (0,1,2) - first differencing
ETH_arima_model <- arima_model(ETHtrain$Price)
arima_forecast_plot('ETH', ETH_arima_model, days, ETH, ETHprice)

# GARCH
ETH_garch_model1 <- garch_model(c(1,1), ETHtrain$Price)
ETH_garch_model1_metrics <- garch_forecast(ETH_garch_model1, days, ETHprice, "ETH")
garch_forecast_plot("ETH", ETH_garch_model1, days, ETH, ETHprice)
# best model according to the metrics

ETH_garch_model2 <- garch_model(c(1,0), ETHtrain$Price)
ETH_garch_model2_metrics <- garch_forecast(ETH_garch_model2, days, ETHprice, "ETH")
garch_forecast_plot("ETH", ETH_garch_model2, days, ETH, ETHprice)

ETH_garch_model3 <- garch_model(c(0,1), ETHtrain$Price)
ETH_garch_model3_metrics <- garch_forecast(ETH_garch_model3, days, ETHprice, "ETH")
garch_forecast_plot("ETH", ETH_garch_model3, days, ETH, ETHprice)

ETH_garch_model4 <- garch_model(c(1,2), ETHtrain$Price)
ETH_garch_model4_metrics <- garch_forecast(ETH_garch_model4, days, ETHprice, "ETH")
garch_forecast_plot("ETH", ETH_garch_model4, days, ETH, ETHprice)

# first GARCH model is best
save_garch_results(ETH_garch_model1, days, ETH, "ETH")

#################################################################################
# ETH returns data

ETHreturns <- return_data_prep(ETH)
ETHrettrain <- head(ETHreturns, split)
ETHrettest <- tail(ETHreturns, length(ETHreturns[,1])-split)
returns_plot("ETH", ETHreturns)

# ARIMA model - ARIMA(3,0,4)
ETH_returns_arima_model <- arima_model(ETHrettrain$Price)
arima_forecast_plot('ETH', ETH_returns_arima_model, days, ETHreturns, ETHreturns$Price)

# GARCH
ETH_returns_garch_model1 <- garch_model(c(1,1), ETHrettrain$Price)
ETH_returns_garch_model1_metrics <- garch_forecast(ETH_returns_garch_model1, days, ETHreturns$Price, "ETH")
garch_forecast_plot("ETH", ETH_returns_garch_model1, days, ETHreturns, ETHreturns$Price)
# best model according to the metrics, but predicts const

ETH_returns_garch_model2 <- garch_model(c(1,0), ETHrettrain$Price)
ETH_returns_garch_model2_metrics <- garch_forecast(ETH_returns_garch_model2, days, ETHreturns$Price, "ETH")
garch_forecast_plot("ETH", ETH_returns_garch_model2, days, ETHreturns, ETHreturns$Price)

ETH_returns_garch_model3 <- garch_model(c(0,1), ETHrettrain$Price)
ETH_returns_garch_model3_metrics <- garch_forecast(ETH_returns_garch_model3, days, ETHreturns$Price, "ETH")
garch_forecast_plot("ETH", ETH_returns_garch_model3, days, ETHreturns, ETHreturns$Price)

ETH_returns_garch_model4 <- garch_model(c(1,2), ETHreturns$Price)
ETH_returns_garch_model4_metrics <- garch_forecast(ETH_returns_garch_model4, days, ETHreturns$Price, "ETH")
garch_forecast_plot("ETH", ETH_returns_garch_model4, days, ETHreturns, ETHreturns$Price)
# best for ETH returns


#################################################################################
# BNB price data

BNB <- read_data("BNB")
BNBprice <- price_data_prep(BNB)
split <- as.integer(length(BNB[,1])*0.8)
BNBtrain <- head(BNB, split)
BNBtest <- tail(BNB, length(BNB[,1])-split)
days <- length(BNBtest[,1])

price_plot('BNB', BNB)

# ARIMA model - ARIMA (0,1,3) - first differencing
BNB_arima_model <- arima_model(BNBtrain$Price)
arima_forecast_plot('BNB', BNB_arima_model, days, BNB, BNBprice)
# forecasts const

# GARCH
BNB_garch_model1 <- garch_model(c(1,1), BNBtrain$Price)
BNB_garch_model1_metrics <- garch_forecast(BNB_garch_model1, days, BNBprice, "BNB")
garch_forecast_plot("BNB", BNB_garch_model1, days, BNB, BNBprice)

BNB_garch_model2 <- garch_model(c(1,0), BNBtrain$Price)
BNB_garch_model2_metrics <- garch_forecast(BNB_garch_model2, days, BNBprice, "BNB")
garch_forecast_plot("BNB", BNB_garch_model2, days, BNB, BNBprice)
# best for BNB prices according to the metrics

BNB_garch_model3 <- garch_model(c(0,1), BNBtrain$Price)
BNB_garch_model3_metrics <- garch_forecast(BNB_garch_model3, days, BNBprice, "BNB")
garch_forecast_plot("BNB", BNB_garch_model3, days, BNB, BNBprice)

BNB_garch_model4 <- garch_model(c(1,2), BNBtrain$Price)
BNB_garch_model4_metrics <- garch_forecast(BNB_garch_model4, days, BNBprice, "BNB")
garch_forecast_plot("BNB", BNB_garch_model4, days, BNB, BNBprice)

save_garch_results(BNB_garch_model2, days, BNB, "BNB")

#################################################################################
# BNB returns data

BNBreturns <- return_data_prep(BNB)
BNBrettrain <- head(BNBreturns, split)
BNBrettest <- tail(BNBreturns, length(BNBreturns[,1])-split)
returns_plot("BNB", BNBreturns)

# ARIMA model - ARIMA(1,0,1)
BNB_returns_arima_model <- arima_model(BNBrettrain$Price)
arima_forecast_plot('BNB', BNB_returns_arima_model, days, BNBreturns, BNBreturns$Price)

# GARCH
BNB_returns_garch_model1 <- garch_model(c(1,1), BNBrettrain$Price)
BNB_returns_garch_model1_metrics <- garch_forecast(BNB_returns_garch_model1, days, BNBreturns$Price, "BNB")
garch_forecast_plot("BNB", BNB_returns_garch_model1, days, BNBreturns, BNBreturns$Price)

BNB_returns_garch_model2 <- garch_model(c(1,0), BNBrettrain$Price)
BNB_returns_garch_model2_metrics <- garch_forecast(BNB_returns_garch_model2, days, BNBreturns$Price, "BNB")
garch_forecast_plot("BNB", BNB_returns_garch_model2, days, BNBreturns, BNBreturns$Price)
# best model according to the metrics, but predicts const

BNB_returns_garch_model3 <- garch_model(c(0,1), BNBrettrain$Price)
BNB_returns_garch_model3_metrics <- garch_forecast(BNB_returns_garch_model3, days, BNBreturns$Price, "BNB")
garch_forecast_plot("BNB", BNB_returns_garch_model3, days, BNBreturns, BNBreturns$Price)

BNB_returns_garch_model4 <- garch_model(c(1,2), BNBreturns$Price)
BNB_returns_garch_model4_metrics <- garch_forecast(BNB_returns_garch_model4, days, BNBreturns$Price, "BNB")
garch_forecast_plot("BNB", BNB_returns_garch_model4, days, BNBreturns, BNBreturns$Price)
# best for BNB returns


#################################################################################
# XRP price data

XRP <- read_data("XRP")
XRPprice <- price_data_prep(XRP)
split <- as.integer(length(XRP[,1])*0.8)
XRPtrain <- head(XRP, split)
XRPtest <- tail(XRP, length(XRP[,1])-split)
days <- length(XRPtest[,1])

price_plot('XRP', XRP)

# ARIMA model - ARIMA (1,1,3) - first differencing
XRP_arima_model <- arima_model(XRPtrain$Price)
arima_forecast_plot('XRP', XRP_arima_model, days, XRP, XRPprice)

# GARCH
XRP_garch_model1 <- garch_model(c(1,1), XRPtrain$Price)
XRP_garch_model1_metrics <- garch_forecast(XRP_garch_model1, days, XRPprice, "XRP")
garch_forecast_plot("XRP", XRP_garch_model1, days, XRP, XRPprice)

XRP_garch_model2 <- garch_model(c(1,0), XRPtrain$Price)
XRP_garch_model2_metrics <- garch_forecast(XRP_garch_model2, days, XRPprice, "XRP")
garch_forecast_plot("XRP", XRP_garch_model2, days, XRP, XRPprice)

XRP_garch_model3 <- garch_model(c(0,1), XRPtrain$Price)
XRP_garch_model3_metrics <- garch_forecast(XRP_garch_model3, days, XRPprice, "XRP")
garch_forecast_plot("XRP", XRP_garch_model3, days, XRP, XRPprice)

XRP_garch_model4 <- garch_model(c(1,2), XRPtrain$Price)
XRP_garch_model4_metrics <- garch_forecast(XRP_garch_model4, days, XRPprice, "XRP")
garch_forecast_plot("XRP", XRP_garch_model4, days, XRP, XRPprice)

# GARCH model predicts only const

save_arima_results(XRP_arima_model, days, XRP, "XRP")

#################################################################################
# XRP returns data

XRPreturns <- return_data_prep(XRP)
XRPrettrain <- head(XRPreturns, split)
XRPrettest <- tail(XRPreturns, length(XRPreturns[,1])-split)
returns_plot("XRP", XRPreturns)

# ARIMA model - ARIMA(1,0,1)
XRP_returns_arima_model <- arima_model(XRPrettrain$Price)
arima_forecast_plot('XRP', XRP_returns_arima_model, days, XRPreturns, XRPreturns$Price)

# GARCH
XRP_returns_garch_model1 <- garch_model(c(1,1), XRPrettrain$Price)
XRP_returns_garch_model1_metrics <- garch_forecast(XRP_returns_garch_model1, days, XRPreturns$Price, "XRP")
garch_forecast_plot("XRP", XRP_returns_garch_model1, days, XRPreturns, XRPreturns$Price)
# best model according to the metrics, but predicts const

XRP_returns_garch_model2 <- garch_model(c(1,0), XRPrettrain$Price)
XRP_returns_garch_model2_metrics <- garch_forecast(XRP_returns_garch_model2, days, XRPreturns$Price, "XRP")
garch_forecast_plot("XRP", XRP_returns_garch_model2, days, XRPreturns, XRPreturns$Price)

XRP_returns_garch_model3 <- garch_model(c(0,1), XRPrettrain$Price)
XRP_returns_garch_model3_metrics <- garch_forecast(XRP_returns_garch_model3, days, XRPreturns$Price, "XRP")
garch_forecast_plot("XRP", XRP_returns_garch_model3, days, XRPreturns, XRPreturns$Price)

XRP_returns_garch_model4 <- garch_model(c(1,2), XRPreturns$Price)
XRP_returns_garch_model4_metrics <- garch_forecast(XRP_returns_garch_model4, days, XRPreturns$Price, "XRP")
garch_forecast_plot("XRP", XRP_returns_garch_model4, days, XRPreturns, XRPreturns$Price)
# predicting const

#################################################################################
# ADA price data

ADA <- read_data("ADA")
ADAprice <- price_data_prep(ADA)
split <- as.integer(length(ADA[,1])*0.8)
ADAtrain <- head(ADA, split)
ADAtest <- tail(ADA, length(ADA[,1])-split)
days <- length(ADAtest[,1])

price_plot('ADA', ADA)

# ARIMA model - ARIMA (3,1,5) - first differencing
ADA_arima_model <- arima_model(ADAtrain$Price)
arima_forecast_plot('ADA', ADA_arima_model, days, ADA, ADAprice)
# predicts wrong direction

# GARCH
ADA_garch_model1 <- garch_model(c(1,1), ADAtrain$Price)
ADA_garch_model1_metrics <- garch_forecast(ADA_garch_model1, days, ADAprice, "ADA")
garch_forecast_plot("ADA", ADA_garch_model1, days, ADA, ADAprice)
# best for ADA prices according to the metrics

ADA_garch_model2 <- garch_model(c(1,0), ADAtrain$Price)
ADA_garch_model2_metrics <- garch_forecast(ADA_garch_model2, days, ADAprice, "ADA")
garch_forecast_plot("ADA", ADA_garch_model2, days, ADA, ADAprice)

ADA_garch_model3 <- garch_model(c(0,1), ADAtrain$Price)
ADA_garch_model3_metrics <- garch_forecast(ADA_garch_model3, days, ADAprice, "ADA")
garch_forecast_plot("ADA", ADA_garch_model3, days, ADA, ADAprice)
# always weird predictions

ADA_garch_model4 <- garch_model(c(1,2), ADAtrain$Price)
ADA_garch_model4_metrics <- garch_forecast(ADA_garch_model4, days, ADAprice, "ADA")
garch_forecast_plot("ADA", ADA_garch_model4, days, ADA, ADAprice)

save_garch_results(ADA_garch_model1, days, ADA, "ADA")
#################################################################################
# ADA returns data

ADAreturns <- return_data_prep(ADA)
ADArettrain <- head(ADAreturns, split)
ADArettest <- tail(ADAreturns, length(ADAreturns[,1])-split)
returns_plot("ADA", ADAreturns)

# ARIMA model - ARIMA(1,0,1)
ADA_returns_arima_model <- arima_model(ADArettrain$Price)
arima_forecast_plot('ADA', ADA_returns_arima_model, days, ADAreturns, ADAreturns$Price)

# GARCH
ADA_returns_garch_model1 <- garch_model(c(1,1), ADArettrain$Price)
ADA_returns_garch_model1_metrics <- garch_forecast(ADA_returns_garch_model1, days, ADAreturns$Price, "ADA")
garch_forecast_plot("ADA", ADA_returns_garch_model1, days, ADAreturns, ADAreturns$Price)
# best model according to the metrics, but predicts const

ADA_returns_garch_model2 <- garch_model(c(1,0), ADArettrain$Price)
ADA_returns_garch_model2_metrics <- garch_forecast(ADA_returns_garch_model2, days, ADAreturns$Price, "ADA")
garch_forecast_plot("ADA", ADA_returns_garch_model2, days, ADAreturns, ADAreturns$Price)

ADA_returns_garch_model3 <- garch_model(c(0,1), ADArettrain$Price)
ADA_returns_garch_model3_metrics <- garch_forecast(ADA_returns_garch_model3, days, ADAreturns$Price, "ADA")
garch_forecast_plot("ADA", ADA_returns_garch_model3, days, ADAreturns, ADAreturns$Price)

ADA_returns_garch_model4 <- garch_model(c(1,2), ADAreturns$Price)
ADA_returns_garch_model4_metrics <- garch_forecast(ADA_returns_garch_model4, days, ADAreturns$Price, "ADA")
garch_forecast_plot("ADA", ADA_returns_garch_model4, days, ADAreturns, ADAreturns$Price)


#################################################################################
# DOGE price data

DOGE <- read_data("DOGE")
DOGEprice <- price_data_prep(DOGE)
split <- as.integer(length(DOGE[,1])*0.8)
DOGEtrain <- head(DOGE, split)
DOGEtest <- tail(DOGE, length(DOGE[,1])-split)
days <- length(DOGEtest[,1])

price_plot('DOGE', DOGE)

# ARIMA model - ARIMA (0,0,0) - first differencing
DOGE_arima_model <- arima_model(DOGEtrain$Price)
arima_forecast_plot('DOGE', DOGE_arima_model, days, DOGE, DOGEprice)
# predicts wrong direction

# GARCH
DOGE_garch_model1 <- garch_model(c(1,1), DOGEtrain$Price)
DOGE_garch_model1_metrics <- garch_forecast(DOGE_garch_model1, days, DOGEprice, "DOGE")
garch_forecast_plot("DOGE", DOGE_garch_model1, days, DOGE, DOGEprice)
# best model

DOGE_garch_model2 <- garch_model(c(1,0), DOGEtrain$Price)
DOGE_garch_model2_metrics <- garch_forecast(DOGE_garch_model2, days, DOGEprice, "DOGE")
garch_forecast_plot("DOGE", DOGE_garch_model2, days, DOGE, DOGEprice)
# best for DOGE prices according to the metrics

DOGE_garch_model3 <- garch_model(c(0,1), DOGEtrain$Price)
DOGE_garch_model3_metrics <- garch_forecast(DOGE_garch_model3, days, DOGEprice, "DOGE")
garch_forecast_plot("DOGE", DOGE_garch_model3, days, DOGE, DOGEprice)
# always weird predictions

DOGE_garch_model4 <- garch_model(c(1,2), DOGEtrain$Price)
DOGE_garch_model4_metrics <- garch_forecast(DOGE_garch_model4, days, DOGEprice, "DOGE")
garch_forecast_plot("DOGE", DOGE_garch_model4, days, DOGE, DOGEprice)

save_garch_results(DOGE_garch_model1, days, DOGE, "DOGE")

#################################################################################
# DOGE returns data

DOGEreturns <- return_data_prep(DOGE)
DOGErettrain <- head(DOGEreturns, split)
DOGErettest <- tail(DOGEreturns, length(DOGEreturns[,1])-split)
returns_plot("DOGE", DOGEreturns)

# ARIMA model - ARIMA(1,0,1)
DOGE_returns_arima_model <- arima_model(DOGErettrain$Price)
arima_forecast_plot('DOGE', DOGE_returns_arima_model, days, DOGEreturns, DOGEreturns$Price)

# GARCH
DOGE_returns_garch_model1 <- garch_model(c(1,1), DOGErettrain$Price)
DOGE_returns_garch_model1_metrics <- garch_forecast(DOGE_returns_garch_model1, days, DOGEreturns$Price, "DOGE")
garch_forecast_plot("DOGE", DOGE_returns_garch_model1, days, DOGEreturns, DOGEreturns$Price)
# best model according to the metrics, but predicts const

DOGE_returns_garch_model2 <- garch_model(c(1,0), DOGErettrain$Price)
DOGE_returns_garch_model2_metrics <- garch_forecast(DOGE_returns_garch_model2, days, DOGEreturns$Price, "DOGE")
garch_forecast_plot("DOGE", DOGE_returns_garch_model2, days, DOGEreturns, DOGEreturns$Price)

DOGE_returns_garch_model3 <- garch_model(c(0,1), DOGErettrain$Price)
DOGE_returns_garch_model3_metrics <- garch_forecast(DOGE_returns_garch_model3, days, DOGEreturns$Price, "DOGE")
garch_forecast_plot("DOGE", DOGE_returns_garch_model3, days, DOGEreturns, DOGEreturns$Price)

DOGE_returns_garch_model4 <- garch_model(c(1,2), DOGEreturns$Price)
DOGE_returns_garch_model4_metrics <- garch_forecast(DOGE_returns_garch_model4, days, DOGEreturns$Price, "DOGE")
garch_forecast_plot("DOGE", DOGE_returns_garch_model4, days, DOGEreturns, DOGEreturns$Price)
# maybe remove DOGE from the list? unpredistable prices and returns

#################################################################################
# LTC price data

LTC <- read_data("LTC")
LTCprice <- price_data_prep(LTC)
split <- as.integer(length(LTC[,1])*0.8)
LTCtrain <- head(LTC, split)
LTCtest <- tail(LTC, length(LTC[,1])-split)
days <- length(LTCtest[,1])

price_plot('LTC', LTC)

# ARIMA model - ARIMA (5,1,3) - first differencing
LTC_arima_model <- arima_model(LTCtrain$Price)
arima_forecast_plot('LTC', LTC_arima_model, days, LTC, LTCprice)

# GARCH
LTC_garch_model1 <- garch_model(c(1,1), LTCtrain$Price)
LTC_garch_model1_metrics <- garch_forecast(LTC_garch_model1, days, LTCprice, "LTC")
garch_forecast_plot("LTC", LTC_garch_model1, days, LTC, LTCprice)

LTC_garch_model2 <- garch_model(c(1,0), LTCtrain$Price)
LTC_garch_model2_metrics <- garch_forecast(LTC_garch_model2, days, LTCprice, "LTC")
garch_forecast_plot("LTC", LTC_garch_model2, days, LTC, LTCprice)
# best for LTC prices according to the metrics

LTC_garch_model3 <- garch_model(c(0,1), LTCtrain$Price)
LTC_garch_model3_metrics <- garch_forecast(LTC_garch_model3, days, LTCprice, "LTC")
garch_forecast_plot("LTC", LTC_garch_model3, days, LTC, LTCprice)

LTC_garch_model4 <- garch_model(c(1,2), LTCtrain$Price)
LTC_garch_model4_metrics <- garch_forecast(LTC_garch_model4, days, LTCprice, "LTC")
garch_forecast_plot("LTC", LTC_garch_model4, days, LTC, LTCprice)

save_arima_results(LTC_arima_model, days, LTC, "LTC")
#################################################################################
# LTC returns data

LTCreturns <- return_data_prep(LTC)
LTCrettrain <- head(LTCreturns, split)
LTCrettest <- tail(LTCreturns, length(LTCreturns[,1])-split)
returns_plot("LTC", LTCreturns)

# ARIMA model - ARIMA(1,0,1)
LTC_returns_arima_model <- arima_model(LTCrettrain$Price)
arima_forecast_plot('LTC', LTC_returns_arima_model, days, LTCreturns, LTCreturns$Price)

# GARCH
LTC_returns_garch_model1 <- garch_model(c(1,1), LTCrettrain$Price)
LTC_returns_garch_model1_metrics <- garch_forecast(LTC_returns_garch_model1, days, LTCreturns$Price, "LTC")
garch_forecast_plot("LTC", LTC_returns_garch_model1, days, LTCreturns, LTCreturns$Price)
# best model according to the metrics, but predicts const

LTC_returns_garch_model2 <- garch_model(c(1,0), LTCrettrain$Price)
LTC_returns_garch_model2_metrics <- garch_forecast(LTC_returns_garch_model2, days, LTCreturns$Price, "LTC")
garch_forecast_plot("LTC", LTC_returns_garch_model2, days, LTCreturns, LTCreturns$Price)

LTC_returns_garch_model3 <- garch_model(c(0,1), LTCrettrain$Price)
LTC_returns_garch_model3_metrics <- garch_forecast(LTC_returns_garch_model3, days, LTCreturns$Price, "LTC")
garch_forecast_plot("LTC", LTC_returns_garch_model3, days, LTCreturns, LTCreturns$Price)

LTC_returns_garch_model4 <- garch_model(c(1,2), LTCreturns$Price)
LTC_returns_garch_model4_metrics <- garch_forecast(LTC_returns_garch_model4, days, LTCreturns$Price, "LTC")
garch_forecast_plot("LTC", LTC_returns_garch_model4, days, LTCreturns, LTCreturns$Price)

# for LTC returns - with GARCH model only const predictions


#################################################################################
# TRX price data

TRX <- read_data("TRX")
TRXprice <- price_data_prep(TRX)
split <- as.integer(length(TRX[,1])*0.8)
TRXtrain <- head(TRX, split)
TRXtest <- tail(TRX, length(TRX[,1])-split)
days <- length(TRXtest[,1])

price_plot('TRX', TRX)

# ARIMA model - ARIMA (4,1,4) - first differencing
TRX_arima_model <- arima_model(TRXtrain$Price)
arima_forecast_plot('TRX', TRX_arima_model, days, TRX, TRXprice)
# forecast is very near const, almost not changing

# GARCH
TRX_garch_model1 <- garch_model(c(1,1), TRXtrain$Price)
TRX_garch_model1_metrics <- garch_forecast(TRX_garch_model1, days, TRXprice, "TRX")
garch_forecast_plot("TRX", TRX_garch_model1, days, TRX, TRXprice)

TRX_garch_model2 <- garch_model(c(1,0), TRXtrain$Price)
TRX_garch_model2_metrics <- garch_forecast(TRX_garch_model2, days, TRXprice, "TRX")
garch_forecast_plot("TRX", TRX_garch_model2, days, TRX, TRXprice)
# best for TRX prices according to the metrics

TRX_garch_model3 <- garch_model(c(0,1), TRXtrain$Price)
TRX_garch_model3_metrics <- garch_forecast(TRX_garch_model3, days, TRXprice, "TRX")
garch_forecast_plot("TRX", TRX_garch_model3, days, TRX, TRXprice)

TRX_garch_model4 <- garch_model(c(1,2), TRXtrain$Price)
TRX_garch_model4_metrics <- garch_forecast(TRX_garch_model4, days, TRXprice, "TRX")
garch_forecast_plot("TRX", TRX_garch_model4, days, TRX, TRXprice)

save_arima_results(TRX_arima_model, days, TRX, "TRX")
#################################################################################
# TRX returns data

TRXreturns <- return_data_prep(TRX)
TRXrettrain <- head(TRXreturns, split)
TRXrettest <- tail(TRXreturns, length(TRXreturns[,1])-split)
returns_plot("TRX", TRXreturns)

# ARIMA model - ARIMA(0,0,2)
TRX_returns_arima_model <- arima_model(TRXrettrain$Price)
arima_forecast_plot('TRX', TRX_returns_arima_model, days, TRXreturns, TRXreturns$Price)

# GARCH
TRX_returns_garch_model1 <- garch_model(c(1,1), TRXrettrain$Price)
TRX_returns_garch_model1_metrics <- garch_forecast(TRX_returns_garch_model1, days, TRXreturns$Price, "TRX")
garch_forecast_plot("TRX", TRX_returns_garch_model1, days, TRXreturns, TRXreturns$Price)
# best model according to the metrics, but predicts const

TRX_returns_garch_model2 <- garch_model(c(1,0), TRXrettrain$Price)
TRX_returns_garch_model2_metrics <- garch_forecast(TRX_returns_garch_model2, days, TRXreturns$Price, "TRX")
garch_forecast_plot("TRX", TRX_returns_garch_model2, days, TRXreturns, TRXreturns$Price)

TRX_returns_garch_model3 <- garch_model(c(0,1), TRXrettrain$Price)
TRX_returns_garch_model3_metrics <- garch_forecast(TRX_returns_garch_model3, days, TRXreturns$Price, "TRX")
garch_forecast_plot("TRX", TRX_returns_garch_model3, days, TRXreturns, TRXreturns$Price)

TRX_returns_garch_model4 <- garch_model(c(1,2), TRXreturns$Price)
TRX_returns_garch_model4_metrics <- garch_forecast(TRX_returns_garch_model4, days, TRXreturns$Price, "TRX")
garch_forecast_plot("TRX", TRX_returns_garch_model4, days, TRXreturns, TRXreturns$Price)


#################################################################################
# LINK price data

LINK <- read_data("LINK")
LINKprice <- price_data_prep(LINK)
split <- as.integer(length(LINK[,1])*0.8)
LINKtrain <- head(LINK, split)
LINKtest <- tail(LINK, length(LINK[,1])-split)
days <- length(LINKtest[,1])

price_plot('LINK', LINK)

# ARIMA model - ARIMA (1,1,2) - first differencing
LINK_arima_model <- arima_model(LINKtrain$Price)
arima_forecast_plot('LINK', LINK_arima_model, days, LINK, LINKprice)
# forecast is const

# GARCH
LINK_garch_model1 <- garch_model(c(1,1), LINKtrain$Price)
LINK_garch_model1_metrics <- garch_forecast(LINK_garch_model1, days, LINKprice, "LINK")
garch_forecast_plot("LINK", LINK_garch_model1, days, LINK, LINKprice)
# guesses right direction, straight line. best according to the metrics

LINK_garch_model2 <- garch_model(c(1,0), LINKtrain$Price)
LINK_garch_model2_metrics <- garch_forecast(LINK_garch_model2, days, LINKprice, "LINK")
garch_forecast_plot("LINK", LINK_garch_model2, days, LINK, LINKprice)

LINK_garch_model3 <- garch_model(c(0,1), LINKtrain$Price)
LINK_garch_model3_metrics <- garch_forecast(LINK_garch_model3, days, LINKprice, "LINK")
garch_forecast_plot("LINK", LINK_garch_model3, days, LINK, LINKprice)

LINK_garch_model4 <- garch_model(c(1,2), LINKtrain$Price)
LINK_garch_model4_metrics <- garch_forecast(LINK_garch_model4, days, LINKprice, "LINK")
garch_forecast_plot("LINK", LINK_garch_model4, days, LINK, LINKprice)

save_garch_results(LINK_garch_model1, days, LINK, "LINK")
#################################################################################
# LINK returns data

LINKreturns <- return_data_prep(LINK)
LINKrettrain <- head(LINKreturns, split)
LINKrettest <- tail(LINKreturns, length(LINKreturns[,1])-split)
returns_plot("LINK", LINKreturns)

# ARIMA model - ARIMA(1,0,3)
LINK_returns_arima_model <- arima_model(LINKrettrain$Price)
arima_forecast_plot('LINK', LINK_returns_arima_model, days, LINKreturns, LINKreturns$Price)

# GARCH
LINK_returns_garch_model1 <- garch_model(c(1,1), LINKrettrain$Price)
LINK_returns_garch_model1_metrics <- garch_forecast(LINK_returns_garch_model1, days, LINKreturns$Price, "LINK")
garch_forecast_plot("LINK", LINK_returns_garch_model1, days, LINKreturns, LINKreturns$Price)
# best model according to the metrics, but predicts const

LINK_returns_garch_model2 <- garch_model(c(1,0), LINKrettrain$Price)
LINK_returns_garch_model2_metrics <- garch_forecast(LINK_returns_garch_model2, days, LINKreturns$Price, "LINK")
garch_forecast_plot("LINK", LINK_returns_garch_model2, days, LINKreturns, LINKreturns$Price)

LINK_returns_garch_model3 <- garch_model(c(0,1), LINKrettrain$Price)
LINK_returns_garch_model3_metrics <- garch_forecast(LINK_returns_garch_model3, days, LINKreturns$Price, "LINK")
garch_forecast_plot("LINK", LINK_returns_garch_model3, days, LINKreturns, LINKreturns$Price)

LINK_returns_garch_model4 <- garch_model(c(1,2), LINKreturns$Price)
LINK_returns_garch_model4_metrics <- garch_forecast(LINK_returns_garch_model4, days, LINKreturns$Price, "LINK")
garch_forecast_plot("LINK", LINK_returns_garch_model4, days, LINKreturns, LINKreturns$Price)


#################################################################################
# BCH price data

BCH <- read_data("BCH")
BCHprice <- price_data_prep(BCH)
split <- as.integer(length(BCH[,1])*0.8)
BCHtrain <- head(BCH, split)
BCHtest <- tail(BCH, length(BCH[,1])-split)
days <- length(BCHtest[,1])

price_plot('BCH', BCH)

# ARIMA model - ARIMA (3,1,2) - first differencing
BCH_arima_model <- arima_model(BCHtrain$Price)
arima_forecast_plot('BCH', BCH_arima_model, days, BCH, BCHprice)
# forecast is nearly const

# GARCH
BCH_garch_model1 <- garch_model(c(1,1), BCHtrain$Price)
BCH_garch_model1_metrics <- garch_forecast(BCH_garch_model1, days, BCHprice, "BCH")
garch_forecast_plot("BCH", BCH_garch_model1, days, BCH, BCHprice)
# guesses right direction, straight line. best according to the metrics

BCH_garch_model2 <- garch_model(c(1,0), BCHtrain$Price)
BCH_garch_model2_metrics <- garch_forecast(BCH_garch_model2, days, BCHprice, "BCH")
garch_forecast_plot("BCH", BCH_garch_model2, days, BCH, BCHprice)
# just straight line, not capturing the price movements

BCH_garch_model3 <- garch_model(c(0,1), BCHtrain$Price)
BCH_garch_model3_metrics <- garch_forecast(BCH_garch_model3, days, BCHprice, "BCH")
garch_forecast_plot("BCH", BCH_garch_model3, days, BCH, BCHprice)
# straight line

BCH_garch_model4 <- garch_model(c(1,2), BCHtrain$Price)
BCH_garch_model4_metrics <- garch_forecast(BCH_garch_model4, days, BCHprice, "BCH")
garch_forecast_plot("BCH", BCH_garch_model4, days, BCH, BCHprice)

save_arima_results(BCH_arima_model, days, BCH, "BCH")
#################################################################################
# BCH returns data

BCHreturns <- return_data_prep(BCH)
BCHrettrain <- head(BCHreturns, split)
BCHrettest <- tail(BCHreturns, length(BCHreturns[,1])-split)
returns_plot("BCH", BCHreturns)

# ARIMA model - ARIMA(3,0,1)
BCH_returns_arima_model <- arima_model(BCHrettrain$Price)
arima_forecast_plot('BCH', BCH_returns_arima_model, days, BCHreturns, BCHreturns$Price)

# GARCH
BCH_returns_garch_model1 <- garch_model(c(1,1), BCHrettrain$Price)
BCH_returns_garch_model1_metrics <- garch_forecast(BCH_returns_garch_model1, days, BCHreturns$Price, "BCH")
garch_forecast_plot("BCH", BCH_returns_garch_model1, days, BCHreturns, BCHreturns$Price)
# best model according to the metrics, but predicts const

BCH_returns_garch_model2 <- garch_model(c(1,0), BCHrettrain$Price)
BCH_returns_garch_model2_metrics <- garch_forecast(BCH_returns_garch_model2, days, BCHreturns$Price, "BCH")
garch_forecast_plot("BCH", BCH_returns_garch_model2, days, BCHreturns, BCHreturns$Price)

BCH_returns_garch_model3 <- garch_model(c(0,1), BCHrettrain$Price)
BCH_returns_garch_model3_metrics <- garch_forecast(BCH_returns_garch_model3, days, BCHreturns$Price, "BCH")
garch_forecast_plot("BCH", BCH_returns_garch_model3, days, BCHreturns, BCHreturns$Price)

BCH_returns_garch_model4 <- garch_model(c(1,2), BCHreturns$Price)
BCH_returns_garch_model4_metrics <- garch_forecast(BCH_returns_garch_model4, days, BCHreturns$Price, "BCH")
garch_forecast_plot("BCH", BCH_returns_garch_model4, days, BCHreturns, BCHreturns$Price)

# returns is always just a straight line, nearly 0 - maybe we should skip these calculations?


#################################################################################
# EOS price data

EOS <- read_data("EOS")
EOSprice <- price_data_prep(EOS)
split <- as.integer(length(EOS[,1])*0.8)
EOStrain <- head(EOS, split)
EOStest <- tail(EOS, length(EOS[,1])-split)
days <- length(EOStest[,1])

price_plot('EOS', EOS)

# ARIMA model - ARIMA (4,1,3) - first differencing
EOS_arima_model <- arima_model(EOStrain$Price)
arima_forecast_plot('EOS', EOS_arima_model, days, EOS, EOSprice)
# forecast is nearly const

# GARCH
EOS_garch_model1 <- garch_model(c(1,1), EOStrain$Price)
EOS_garch_model1_metrics <- garch_forecast(EOS_garch_model1, days, EOSprice, "EOS")
garch_forecast_plot("EOS", EOS_garch_model1, days, EOS, EOSprice)
# guesses right direction, straight line. 

EOS_garch_model2 <- garch_model(c(1,0), EOStrain$Price)
EOS_garch_model2_metrics <- garch_forecast(EOS_garch_model2, days, EOSprice, "EOS")
garch_forecast_plot("EOS", EOS_garch_model2, days, EOS, EOSprice)
# best according to the metrics

EOS_garch_model3 <- garch_model(c(0,1), EOStrain$Price)
EOS_garch_model3_metrics <- garch_forecast(EOS_garch_model3, days, EOSprice, "EOS")
garch_forecast_plot("EOS", EOS_garch_model3, days, EOS, EOSprice)
# straight line

EOS_garch_model4 <- garch_model(c(1,2), EOStrain$Price)
EOS_garch_model4_metrics <- garch_forecast(EOS_garch_model4, days, EOSprice, "EOS")
garch_forecast_plot("EOS", EOS_garch_model4, days, EOS, EOSprice)

save_arima_results(EOS_arima_model, days, EOS, "EOS")
#################################################################################
# EOS returns data

EOSreturns <- return_data_prep(EOS)
EOSrettrain <- head(EOSreturns, split)
EOSrettest <- tail(EOSreturns, length(EOSreturns[,1])-split)
returns_plot("EOS", EOSreturns)

# ARIMA model - ARIMA(3,0,1)
EOS_returns_arima_model <- arima_model(EOSrettrain$Price)
arima_forecast_plot('EOS', EOS_returns_arima_model, days, EOSreturns, EOSreturns$Price)

# GARCH
EOS_returns_garch_model1 <- garch_model(c(1,1), EOSrettrain$Price)
EOS_returns_garch_model1_metrics <- garch_forecast(EOS_returns_garch_model1, days, EOSreturns$Price, "EOS")
garch_forecast_plot("EOS", EOS_returns_garch_model1, days, EOSreturns, EOSreturns$Price)
# best model according to the metrics, but predicts const

EOS_returns_garch_model2 <- garch_model(c(1,0), EOSrettrain$Price)
EOS_returns_garch_model2_metrics <- garch_forecast(EOS_returns_garch_model2, days, EOSreturns$Price, "EOS")
garch_forecast_plot("EOS", EOS_returns_garch_model2, days, EOSreturns, EOSreturns$Price)

EOS_returns_garch_model3 <- garch_model(c(0,1), EOSrettrain$Price)
EOS_returns_garch_model3_metrics <- garch_forecast(EOS_returns_garch_model3, days, EOSreturns$Price, "EOS")
garch_forecast_plot("EOS", EOS_returns_garch_model3, days, EOSreturns, EOSreturns$Price)

EOS_returns_garch_model4 <- garch_model(c(1,2), EOSreturns$Price)
EOS_returns_garch_model4_metrics <- garch_forecast(EOS_returns_garch_model4, days, EOSreturns$Price, "EOS")
garch_forecast_plot("EOS", EOS_returns_garch_model4, days, EOSreturns, EOSreturns$Price)


#################################################################################
# OMG price data

OMG <- read_data("OMG")
OMGprice <- price_data_prep(OMG)
split <- as.integer(length(OMG[,1])*0.8)
OMGtrain <- head(OMG, split)
OMGtest <- tail(OMG, length(OMG[,1])-split)
days <- length(OMGtest[,1])

price_plot('OMG', OMG)

# ARIMA model - ARIMA (5,1,3) - first differencing
OMG_arima_model <- arima_model(OMGtrain$Price)
arima_forecast_plot('OMG', OMG_arima_model, days, OMG, OMGprice)
# forecast is nearly const

# GARCH
OMG_garch_model1 <- garch_model(c(1,1), OMGtrain$Price)
OMG_garch_model1_metrics <- garch_forecast(OMG_garch_model1, days, OMGprice, "OMG")
garch_forecast_plot("OMG", OMG_garch_model1, days, OMG, OMGprice)

OMG_garch_model2 <- garch_model(c(1,0), OMGtrain$Price)
OMG_garch_model2_metrics <- garch_forecast(OMG_garch_model2, days, OMGprice, "OMG")
garch_forecast_plot("OMG", OMG_garch_model2, days, OMG, OMGprice)
# best model

OMG_garch_model3 <- garch_model(c(0,1), OMGtrain$Price)
OMG_garch_model3_metrics <- garch_forecast(OMG_garch_model3, days, OMGprice, "OMG")
garch_forecast_plot("OMG", OMG_garch_model3, days, OMG, OMGprice)
# best according to the metrics

OMG_garch_model4 <- garch_model(c(1,2), OMGtrain$Price)
OMG_garch_model4_metrics <- garch_forecast(OMG_garch_model4, days, OMGprice, "OMG")
garch_forecast_plot("OMG", OMG_garch_model4, days, OMG, OMGprice)

save_arima_results(OMG_arima_model, days, OMG, "OMG")
#################################################################################
# OMG returns data

OMGreturns <- return_data_prep(OMG)
OMGrettrain <- head(OMGreturns, split)
OMGrettest <- tail(OMGreturns, length(OMGreturns[,1])-split)
returns_plot("OMG", OMGreturns)

# ARIMA model - ARIMA(3,0,1)
OMG_returns_arima_model <- arima_model(OMGrettrain$Price)
arima_forecast_plot('OMG', OMG_returns_arima_model, days, OMGreturns, OMGreturns$Price)

# GARCH
OMG_returns_garch_model1 <- garch_model(c(1,1), OMGrettrain$Price)
OMG_returns_garch_model1_metrics <- garch_forecast(OMG_returns_garch_model1, days, OMGreturns$Price, "OMG")
garch_forecast_plot("OMG", OMG_returns_garch_model1, days, OMGreturns, OMGreturns$Price)
# best model according to the metrics, but predicts const

OMG_returns_garch_model2 <- garch_model(c(1,0), OMGrettrain$Price)
OMG_returns_garch_model2_metrics <- garch_forecast(OMG_returns_garch_model2, days, OMGreturns$Price, "OMG")
garch_forecast_plot("OMG", OMG_returns_garch_model2, days, OMGreturns, OMGreturns$Price)

OMG_returns_garch_model3 <- garch_model(c(0,1), OMGrettrain$Price)
OMG_returns_garch_model3_metrics <- garch_forecast(OMG_returns_garch_model3, days, OMGreturns$Price, "OMG")
garch_forecast_plot("OMG", OMG_returns_garch_model3, days, OMGreturns, OMGreturns$Price)

OMG_returns_garch_model4 <- garch_model(c(1,2), OMGreturns$Price)
OMG_returns_garch_model4_metrics <- garch_forecast(OMG_returns_garch_model4, days, OMGreturns$Price, "OMG")
garch_forecast_plot("OMG", OMG_returns_garch_model4, days, OMGreturns, OMGreturns$Price)


#################################################################################
# XMR price data

XMR <- read_data("XMR")
XMRprice <- price_data_prep(XMR)
split <- as.integer(length(XMR[,1])*0.8)
XMRtrain <- head(XMR, split)
XMRtest <- tail(XMR, length(XMR[,1])-split)
days <- length(XMRtest[,1])

price_plot('XMR', XMR)

# ARIMA model - ARIMA (2,1,3) - first differencing
XMR_arima_model <- arima_model(XMRtrain$Price)
arima_forecast_plot('XMR', XMR_arima_model, days, XMR, XMRprice)
# forecast is nearly const

# GARCH
XMR_garch_model1 <- garch_model(c(1,1), XMRtrain$Price)
XMR_garch_model1_metrics <- garch_forecast(XMR_garch_model1, days, XMRprice, "XMR")
garch_forecast_plot("XMR", XMR_garch_model1, days, XMR, XMRprice)

XMR_garch_model2 <- garch_model(c(1,0), XMRtrain$Price)
XMR_garch_model2_metrics <- garch_forecast(XMR_garch_model2, days, XMRprice, "XMR")
garch_forecast_plot("XMR", XMR_garch_model2, days, XMR, XMRprice)
# best according to the metrics

XMR_garch_model3 <- garch_model(c(0,1), XMRtrain$Price)
XMR_garch_model3_metrics <- garch_forecast(XMR_garch_model3, days, XMRprice, "XMR")
garch_forecast_plot("XMR", XMR_garch_model3, days, XMR, XMRprice)

XMR_garch_model4 <- garch_model(c(1,2), XMRtrain$Price)
XMR_garch_model4_metrics <- garch_forecast(XMR_garch_model4, days, XMRprice, "XMR")
garch_forecast_plot("XMR", XMR_garch_model4, days, XMR, XMRprice)

save_garch_results(XMR_garch_model2, days, XMR, "XMR")
#################################################################################
# XMR returns data

XMRreturns <- return_data_prep(XMR)
XMRrettrain <- head(XMRreturns, split)
XMRrettest <- tail(XMRreturns, length(XMRreturns[,1])-split)
returns_plot("XMR", XMRreturns)

# ARIMA model - ARIMA(4,0,4)
XMR_returns_arima_model <- arima_model(XMRrettrain$Price)
arima_forecast_plot('XMR', XMR_returns_arima_model, days, XMRreturns, XMRreturns$Price)

# GARCH
XMR_returns_garch_model1 <- garch_model(c(1,1), XMRrettrain$Price)
XMR_returns_garch_model1_metrics <- garch_forecast(XMR_returns_garch_model1, days, XMRreturns$Price, "XMR")
garch_forecast_plot("XMR", XMR_returns_garch_model1, days, XMRreturns, XMRreturns$Price)
# best model according to the metrics, but predicts const

XMR_returns_garch_model2 <- garch_model(c(1,0), XMRrettrain$Price)
XMR_returns_garch_model2_metrics <- garch_forecast(XMR_returns_garch_model2, days, XMRreturns$Price, "XMR")
garch_forecast_plot("XMR", XMR_returns_garch_model2, days, XMRreturns, XMRreturns$Price)

XMR_returns_garch_model3 <- garch_model(c(0,1), XMRrettrain$Price)
XMR_returns_garch_model3_metrics <- garch_forecast(XMR_returns_garch_model3, days, XMRreturns$Price, "XMR")
garch_forecast_plot("XMR", XMR_returns_garch_model3, days, XMRreturns, XMRreturns$Price)

XMR_returns_garch_model4 <- garch_model(c(1,2), XMRreturns$Price)
XMR_returns_garch_model4_metrics <- garch_forecast(XMR_returns_garch_model4, days, XMRreturns$Price, "XMR")
garch_forecast_plot("XMR", XMR_returns_garch_model4, days, XMRreturns, XMRreturns$Price)


#################################################################################
# ZEC price data

ZEC <- read_data("ZEC")
ZECprice <- price_data_prep(ZEC)
split <- as.integer(length(ZEC[,1])*0.8)
ZECtrain <- head(ZEC, split)
ZECtest <- tail(ZEC, length(ZEC[,1])-split)
days <- length(ZECtest[,1])

price_plot('ZEC', ZEC)

# ARIMA model - ARIMA (1,1,1) - first differencing
ZEC_arima_model <- arima_model(ZECtrain$Price)
arima_forecast_plot('ZEC', ZEC_arima_model, days, ZEC, ZECprice)
# forecast is nearly const

# GARCH
ZEC_garch_model1 <- garch_model(c(1,1), ZECtrain$Price)
ZEC_garch_model1_metrics <- garch_forecast(ZEC_garch_model1, days, ZECprice, "ZEC")
garch_forecast_plot("ZEC", ZEC_garch_model1, days, ZEC, ZECprice)

ZEC_garch_model2 <- garch_model(c(1,0), ZECtrain$Price)
ZEC_garch_model2_metrics <- garch_forecast(ZEC_garch_model2, days, ZECprice, "ZEC")
garch_forecast_plot("ZEC", ZEC_garch_model2, days, ZEC, ZECprice)
# best according to the metrics

ZEC_garch_model3 <- garch_model(c(0,1), ZECtrain$Price)
ZEC_garch_model3_metrics <- garch_forecast(ZEC_garch_model3, days, ZECprice, "ZEC")
garch_forecast_plot("ZEC", ZEC_garch_model3, days, ZEC, ZECprice)
# weird forecast

ZEC_garch_model4 <- garch_model(c(1,2), ZECtrain$Price)
ZEC_garch_model4_metrics <- garch_forecast(ZEC_garch_model4, days, ZECprice, "ZEC")
garch_forecast_plot("ZEC", ZEC_garch_model4, days, ZEC, ZECprice)

# all other GARCH models forecast just a straight line, parallel to the X axis

save_arima_results(ZEC_arima_model, days, ZEC, "ZEC")
#################################################################################
# ZEC returns data

ZECreturns <- return_data_prep(ZEC)
ZECrettrain <- head(ZECreturns, split)
ZECrettest <- tail(ZECreturns, length(ZECreturns[,1])-split)
returns_plot("ZEC", ZECreturns)

# ARIMA model - ARIMA(3,0,3)
ZEC_returns_arima_model <- arima_model(ZECrettrain$Price)
arima_forecast_plot('ZEC', ZEC_returns_arima_model, days, ZECreturns, ZECreturns$Price)

# GARCH
ZEC_returns_garch_model1 <- garch_model(c(1,1), ZECrettrain$Price)
ZEC_returns_garch_model1_metrics <- garch_forecast(ZEC_returns_garch_model1, days, ZECreturns$Price, "ZEC")
garch_forecast_plot("ZEC", ZEC_returns_garch_model1, days, ZECreturns, ZECreturns$Price)
# best model according to the metrics, but predicts const

ZEC_returns_garch_model2 <- garch_model(c(1,0), ZECrettrain$Price)
ZEC_returns_garch_model2_metrics <- garch_forecast(ZEC_returns_garch_model2, days, ZECreturns$Price, "ZEC")
garch_forecast_plot("ZEC", ZEC_returns_garch_model2, days, ZECreturns, ZECreturns$Price)

ZEC_returns_garch_model3 <- garch_model(c(0,1), ZECrettrain$Price)
ZEC_returns_garch_model3_metrics <- garch_forecast(ZEC_returns_garch_model3, days, ZECreturns$Price, "ZEC")
garch_forecast_plot("ZEC", ZEC_returns_garch_model3, days, ZECreturns, ZECreturns$Price)

ZEC_returns_garch_model4 <- garch_model(c(1,2), ZECreturns$Price)
ZEC_returns_garch_model4_metrics <- garch_forecast(ZEC_returns_garch_model4, days, ZECreturns$Price, "ZEC")
garch_forecast_plot("ZEC", ZEC_returns_garch_model4, days, ZECreturns, ZECreturns$Price)

