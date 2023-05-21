library(tseries)
library(quantmod)
library(DMwR)
library(randomForest)
library(nnet)
library(e1071)
library(kernlab)
library(earth)
library(fPortfolio)
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

setwd("/Users/jekaterina/Desktop/Bakalauras/Data/Clean data")

################################################################################

T.ind <- function(quotes, tgt.margin = 0.025, n.days = 10) {
  v <- apply(HLC(quotes), 1, mean)
  r <- matrix(NA, ncol = n.days, nrow = NROW(quotes))
  for (x in 1:n.days) r[, x] <- Next(Delt(v, k = x), x)
  x <- apply(r, 1, function(x) sum(x[x > tgt.margin | x < (-tgt.margin)]))
  if (is.xts(quotes))
    xts(x, time(quotes))
  else x
}

read_data <- function(ticker) {
  filename <- paste(ticker, '_daily.csv', sep='')
  df <- read.csv(filename, header=TRUE, sep=';')
  df$Date <- as.Date(df$Date, format="%Y-%m-%d")
  
  return (df)
}

myATR <- function(x) ATR(HLC(x))[, "atr"]
mySMI <- function(x) SMI(HLC(x))[, "SMI"]
myADX <- function(x) ADX(HLC(x))[, "ADX"]
myBB <- function(x) BBands(HLC(x))[, "pctB"]
myChaikinVol <- function(x) Delt(chaikinVolatility(x[, c("High","Low")]))[, 1]
myMACD <- function(x) MACD(Cl(x))[, 2]
mySAR <- function(x) SAR(x[, c("High", "Close")])[, 1]

################################################################################
# TRADING STRATEGIES, Trading policy 1

policy.1 <- function(signals, market, opened.pos, money,
                     bet=0.2, hold.time=10,
                     exp.prof=0.025, max.loss=0.05) {
  d <- NROW(market)
  orders <- NULL
  n0s <- NROW(opened.pos)
  
  if(!n0s && signals[d] == 'h') return (orders)
  
  # long positions
  if(signals[d] == 'b' && !n0s) {
    quant <- round(bet*money/market[d,'Close'],0)
    if(quant > 0)
      orders <- rbind(orders,
                      data.frame(order=c(1,-1,-1), order.type=c(1,2,3),
                                 val = c(quant,
                                         market[d,'Close']*(1+exp.prof),
                                         market[d,'Close']*(1-max.loss)
                                 ),
                                 action = c('open', 'close', 'close'),
                                 posID = c(NA, NA, NA)
                      )
      )
    # short positions
  } else if (signals[d] == 's' && !n0s) {
    need2buy <- sum(opened.pos[opened.pos[,'pos.type'] == -1,
                               "N.stocks"])*market[d,'Close']
    quant <- round(bet*(money-need2buy)/market[d,'Close'],0)
    if(quant > 0)
      orders <- rbind(orders,
                      data.frame(order=c(-1,1,1), order.type=c(1,2,3),
                                 val=c(quant,
                                       market[d, 'Close']*(1-exp.prof),
                                       market[d, 'Close']*(1+max.loss)
                                 ),
                                 action = c('open', 'close', 'close'),
                                 posID = c(NA, NA, NA)
                      )
      )
  }
  
  if (n0s)
    for(i in 1:n0s) {
      if (d - opened.pos[i,'Odate'] >= hold.time)
        orders <- rbind(orders,
                        data.frame(order=-opened.pos[i,'pos.type'],
                                   order.type=1,
                                   val=NA,
                                   action='close',
                                   posID = rownames(opened.pos)[i]
                        )
        )
    }
  orders
}

################################################################################
# Select best model

singleModelSelect <- function(df, formula, train, eval) {

  print("Single, svmC, pol1: ")
  print(single(df, formula, train, eval, 'svmC', 'pol1'))
  print("Single, svmC, pol2: ")
  print(single(df, formula, train, eval, 'svmC', 'pol2'))
  
  print("Single, svmR, pol1: ")
  print(single(df, formula, train, eval, 'svmR', 'pol1'))
  print("Single, svmR, pol2: ")
  print(single(df, formula, train, eval, 'svmR', 'pol2'))
  
  print("Single, earth, pol1: ")
  print(single(df, formula, train, eval, 'earth', 'pol1'))
  print("Single, earth, pol2: ")
  print(single(df, formula, train, eval, 'earth', 'pol2'))
  
}

slideModelSelect <- function(df, formula, train, eval) {
  
  for (step in c(30, 60, 120)) {
    print(paste("Slide,", "svmC,", "pol1,", step, sep=' '))
    print(slide(df, formula, train, eval, 'svmC', step, 'pol1'))
  }
  
  for (step in c(30, 60, 120)) {
    print(paste("Slide,", "svmC,", "pol2,", step, sep=' '))
    print(slide(df, formula, train, eval, 'svmC', step, 'pol2'))
  }
  
  for (step in c(30, 60, 120)) {
    print(paste("Slide,", "svmR,", "pol1,", step, sep=' '))
    print(slide(df, formula, train, eval, 'svmR', step, 'pol1'))
  }
  
  for (step in c(30, 60, 120)) {
    print(paste("Slide,", "svmR,", "pol2,", step, sep=' '))
    print(slide(df, formula, train, eval, 'svmR', step, 'pol2'))
  }
  
  for (step in c(30, 60, 120)) {
    print(paste("Slide,", "earth,", "pol1,", step, sep=' '))
    print(slide(df, formula, train, eval, 'earth', step, 'pol1'))
  }
  
  for (step in c(30, 60, 120)) {
    print(paste("Slide,", "earth,", "pol2,", step, sep=' '))
    print(slide(df, formula, train, eval, 'earth', step, 'pol2'))
  }
  
}

growModelSelect <- function(df, formula, train, eval) {

  for (step in c(30, 60, 120)) {
    print(paste("Grow,", "svmC,", "pol1,", step, sep=' '))
    print(grow(df, formula, train, eval, 'svmC', step, 'pol1'))
  }
  
  for (step in c(30, 60, 120)) {
    print(paste("Grow,", "svmC,", "pol2,", step, sep=' '))
    print(grow(df, formula, train, eval, 'svmC', step, 'pol2'))
  }

  for (step in c(30, 60, 120)) {
    print(paste("Grow,", "svmR,", "pol1,", step, sep=' '))
    print(grow(df, formula, train, eval, 'svmR', step, 'pol1'))
  }
  
  for (step in c(30, 60, 120)) {
    print(paste("Grow,", "svmR,", "pol2,", step, sep=' '))
    print(grow(df, formula, train, eval, 'svmR', step, 'pol2'))
  }
  
  for (step in c(30, 60, 120)) {
    print(paste("Grow,", "earth,", "pol1,", step, sep=' '))
    print(grow(df, formula, train, eval, 'earth', step, 'pol1'))
  }
  
  for (step in c(30, 60, 120)) {
    print(paste("Grow,", "earth,", "pol2,", step, sep=' '))
    print(grow(df, formula, train, eval, 'earth', step, 'pol2'))
  }
  
}

################################################################################

MC.svmR <- function(form, train, test, b.t = 0.1, s.t = -0.1,
                    ...) {
  require(e1071)
  t <- svm(form, train, ...)
  p <- predict(t, test)
  trading.signals(p, b.t, s.t)
}

MC.svmC <- function(form, train, test, b.t=0.1, s.t=-0.1,
                    ...) {
  require(e1071)
  tgtName <- all.vars(form)[1]
  train[, tgtName] <- trading.signals(train[, tgtName],
                                      b.t, s.t)
  t <- svm(form, train, ...)
  p <- predict(t, test)
  factor(p, levels=c('s', 'h', 'b'))
}

MC.earth <- function(form, train, test, b.t=0.1, s.t=-0.1,
                     ...) {
  require(earth)
  t <- earth(form, train, ...)
  p <- predict(t, test)
  trading.signals(p, b.t, s.t)
}

eval.stats <- function(data, form, train, test, preds, b.t=0.1, s.t=-0.1, ...) {
  tgtName <- all.vars(form)[1]
  test[, tgtName] <- trading.signals(test[,tgtName], b.t, s.t)
  st <- sigs.PR(preds, test[, tgtName])
  dim(st) <- NULL
  names(st) <- paste(rep(c('prec', 'rec'), each=3),
                     c('s', 'b', 'sb'), sep='.')
  
  date <- rownames(test)[1]
  market <- data[paste(date,"/", sep='')][1:length(preds),]
  trade.res <- trading.simulator(market, preds, ...)
  
  c(st, tradingEvaluation(trade.res))
}

single <- function(data, form, train, test, learner, policy.func,
                   ...) {
  p <- do.call(paste("MC", learner, sep='.'), list(form, train, test, ...))
  eval.stats(data, form, train, test, p, policy.func = policy.func)
}

slide <- function(data, form, train, test, learner, relearn.step,
                  policy.func, ...) {
  real.learner <- learner(paste("MC", learner, sep="."),
                          pars = list(...))
  p <- slidingWindowTest(real.learner, form, train, test,
                         relearn.step)
  p <- factor(p, levels=1:3, labels=c('s', 'h', 'b'))
  eval.stats(data, form, train, test, p, policy.func = policy.func)
}

grow <- function(data, form, train, test, learner, relearn.step,
                 policy.func, ...) {
  real.learner <- learner(paste("MC", learner, sep="."),
                          pars=list(...))
  p <- growingWindowTest(real.learner, form, train, test,
                         relearn.step)
  p <- factor(p, levels = 1:3, labels = c('s', 'h', 'b'))
  eval.stats(data, form, train, test, p, policy.func = policy.func)
}

pol1 <- function(signals, market, op, money) {
  policy.1(signals, market, op, money,
           bet=0.2, exp.prof=0.025, max.loss=0.05, hold.time=10)
}

pol2 <- function(signals, market, op, money) {
  policy.1(signals, market, op, money,
           bet=0.2, exp.prof=0.05, max.loss=0.05, hold.time=20)
}

################################################################################

BTC <- read_data("BTC")
colnames(BTC)
colnames(BTC) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Circulating Marketcap",
                   "Circulating Supply", "New Issuance", "Active Addresses Count",
                   "Addresses with balance more than 10M", "Volatility", "Addresses Count",
                   "Transaction Volume", "Transactions Count", "Price")

ETH <- read_data("ETH")
colnames(ETH) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Circulating Marketcap",
                   "Circulating Supply", "New Issuance", "Active Addresses Count",
                   "Addresses with balance more than 10M", "Volatility", "Addresses Count",
                   "Transaction Volume", "Transactions Count", "Price")

BNB <- read_data("BNB")
colnames(BNB) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Circulating Marketcap",
                   "Circulating Supply", "Active Addresses Count",
                   "Addresses with balance more than 10M", "Volatility", "Addresses Count",
                   "Transaction Volume", "Transactions Count", "Price")

XRP <- read_data("XRP")
colnames(XRP) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Circulating Marketcap",
                   "Circulating Supply", "Active Addresses Count",
                   "Addresses with balance more than 10M", "Volatility", "Addresses Count",
                   "Transaction Volume", "Transactions Count", "Price")

ADA <- read_data("ADA")
colnames(ADA) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Circulating Marketcap",
                   "Circulating Supply", "New Issuance", "Active Addresses Count",
                   "Addresses with balance more than 10M", "Volatility", "Addresses Count",
                   "Transaction Volume", "Transactions Count", "Price")

DOGE <- read_data('DOGE')
colnames(DOGE) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Circulating Marketcap",
                    "Circulating Supply", "New Issuance", "Active Addresses Count",
                    "Addresses with balance more than 10M", "Volatility", "Addresses Count",
                    "Transaction Volume", "Transactions Count", "Price")

LTC <- read_data("LTC")
colnames(LTC) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Circulating Marketcap",
                   "Circulating Supply", "New Issuance", "Active Addresses Count",
                   "Addresses with balance more than 10M", "Volatility", "Addresses Count",
                   "Transaction Volume", "Transactions Count", "Price")

TRX <- read_data("TRX")
colnames(TRX) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Circulating Marketcap",
                   "Circulating Supply", "New Issuance", "Volatility", "Addresses Count",
                   "Transaction Volume", "Transactions Count", "Price")

LINK <- read_data("LINK")
colnames(LINK) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Circulating Marketcap",
                    "Circulating Supply", "Active Addresses Count",
                    "Addresses with balance more than 10M", "Volatility", "Addresses Count",
                    "Transaction Volume", "Transactions Count", "Price")

BCH <- read_data("BCH")
colnames(BCH) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Circulating Marketcap",
                   "Circulating Supply", "New Issuance", "Active Addresses Count",
                   "Addresses with balance more than 10M", "Volatility", "Addresses Count",
                   "Transaction Volume", "Transactions Count", "Price")

EOS <- read_data("EOS")
colnames(EOS) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Circulating Marketcap",
                   "Circulating Supply", "Active Addresses Count",
                   "Volatility",
                   "Transaction Volume", "Transactions Count", "Price")

OMG <- read_data("OMG")
colnames(OMG) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Circulating Marketcap",
                   "Circulating Supply",  "Active Addresses Count",
                   "Addresses with balance more than 10M", "Volatility", "Addresses Count",
                   "Transaction Volume", "Transactions Count", "Price")

XMR <- read_data("XMR")
colnames(XMR) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Circulating Marketcap",
                   "Circulating Supply", "New Issuance", "Volatility",
                   "Transactions Count", "Price")

ZEC <- read_data("ZEC")
colnames(ZEC) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Circulating Marketcap",
                   "Circulating Supply", "New Issuance", "Active Addresses Count",
                   "Addresses with balance more than 10M", "Volatility", "Addresses Count",
                   "Transaction Volume", "Price")

################################################################################
BTC <- xts(BTC[,2:16], order.by=BTC$Date)

data.model <- specifyModel(T.ind(BTC) ~ Delt(Cl(BTC), k=1:10) +
                             + myATR(BTC) + mySMI(BTC) +
                             + myADX(BTC) + myBB(BTC) + 
                             + myChaikinVol(BTC) + 
                             + myMACD(BTC) + mySAR(BTC))
set.seed(1234)
rf <- buildModel(data.model, method='randomForest',
                 training.per=c(start(BTC), index(BTC["2022-03-05"])),
                 ntree=50, importance=T)
varImpPlot(rf@fitted.model, type=1)
imp <- importance(rf@fitted.model, type=1)
rownames(imp)[which(imp > 10)]
# labiausiai tinkami myATR, myADX, mySAR parametrai
rownames(imp)[which(imp > 8)]
# 5 geriausi - myATR, myADX, myBB, myMACD, mySAR
# juos paliekam galutiniam modeliui

# Final model:
BTCmodel <- specifyModel(T.ind(BTC) ~ myATR(BTC) + 
                           + myADX(BTC) + myBB(BTC) +
                           + myMACD(BTC) + mySAR(BTC))

BTCtrain <- as.data.frame(modelData(BTCmodel,
                                       data.window=c('2018-03-05', '2022-03-02')))
BTCeval <- na.omit(as.data.frame(modelData(BTCmodel,
                                              data.window=c('2022-03-03', '2023-03-02'))))
BTCform <- as.formula('T.ind.BTC ~ .')

singleModelSelect(BTC, BTCform, BTCtrain, BTCeval)

slideModelSelect(BTC, BTCform, BTCtrain, BTCeval)

growModelSelect(BTC, BTCform, BTCtrain, BTCeval)

# Slide, svmR, pol2, 30
model <- learner("MC.svmR", list(maxit=100, linout=T, trace=F, size=10, decay=0.001))
preds <- slidingWindowTest(model, BTCform, BTCtrain, BTCeval, relearn.step=30)
signals <- factor(preds, levels=1:3, labels=c('s', 'h', 'b'))
date <- rownames(BTCeval)[1]
market <- BTC[paste(date, '/', sep='')][1:length(signals),]
trade.res <- trading.simulator(market, signals, policy.func = 'pol2')
plot(trade.res, market, theme='white', name='BTC final test')

# get returns
rets <- Return.calculate(trade.res@trading$Equity, method = "discrete")
BTCrets <- na.omit(rets)
BTCrets <- as.timeSeries(BTCrets)
colnames(BTCrets) <- 'BTCret'

chart.CumReturns(BTCrets, legend.loc="topleft", main="Cumulative Daily Returns for BTC")

RTRN <- as.data.frame(rownames(BTCrets))
colnames(RTRN) <- "Date"
RTRN$Date <- as.Date(RTRN$Date, format="%Y-%m-%d")
RTRN <- cbind(RTRN, BTCrets$BTCret)
colnames(RTRN) <- c("Date", "BTCret")

################################################################################
ETH <- xts(ETH[,2:16], order.by=ETH$Date)

data.model <- specifyModel(T.ind(ETH) ~ Delt(Cl(ETH), k=1:10) +
                             + myATR(ETH) + mySMI(ETH) +
                             + myADX(ETH) + myBB(ETH) + 
                             + myChaikinVol(ETH) + 
                             + myMACD(ETH) + mySAR(ETH))
set.seed(1234)
rf <- buildModel(data.model, method='randomForest',
                 training.per=c(start(ETH), index(ETH["2022-03-05"])),
                 ntree=50, importance=T)
varImpPlot(rf@fitted.model, type=1)
imp <- importance(rf@fitted.model, type=1)
rownames(imp)[which(imp > 10)]
# labiausiai tinkami myATR, mySMI, mySAR parametrai
rownames(imp)[which(imp > 8)]
# 5 geriausi - myATR, mySMI, myADX, myMACD, mySAR
# juos paliekam galutiniam modeliui

# Final model:
ETHmodel <- specifyModel(T.ind(ETH) ~ myATR(ETH) + 
                           + mySMI(ETH) + myADX(ETH) +
                           + myMACD(ETH) + mySAR(ETH))

ETHtrain <- as.data.frame(modelData(ETHmodel,
                                    data.window=c('2018-03-05', '2022-03-02')))
ETHeval <- na.omit(as.data.frame(modelData(ETHmodel,
                                           data.window=c('2022-03-03', '2023-03-02'))))
ETHform <- as.formula('T.ind.ETH ~ .')


singleModelSelect(ETH, ETHform, ETHtrain, ETHeval)

slideModelSelect(ETH, ETHform, ETHtrain, ETHeval)

growModelSelect(ETH, ETHform, ETHtrain, ETHeval)

# Slide, svmC, pol2, 30
model <- learner("MC.svmC", list(maxit=100, linout=T, trace=F, size=10, decay=0.001))
preds <- slidingWindowTest(model, ETHform, ETHtrain, ETHeval, relearn.step=30)
signals <- factor(preds, levels=1:3, labels=c('s', 'h', 'b'))
date <- rownames(ETHeval)[1]
market <- ETH[paste(date, '/', sep='')][1:length(signals),]
trade.res <- trading.simulator(market, signals, policy.func = 'pol2')
plot(trade.res, market, theme='white', name='ETH final test')

# get returns
rets <- Return.calculate(trade.res@trading$Equity, method = "discrete")
ETHrets <- na.omit(rets)
ETHrets <- as.timeSeries(ETHrets)
colnames(ETHrets) <- 'ETHret'

chart.CumReturns(ETHrets, legend.loc="topleft", main="Cumulative Daily Returns for ETH")

RTRN <- cbind(RTRN, ETHrets)

################################################################################
BNB <- xts(BNB[,2:15], order.by=BNB$Date)

data.model <- specifyModel(T.ind(BNB) ~ Delt(Cl(BNB), k=1:10) +
                             + myATR(BNB) + mySMI(BNB) +
                             + myADX(BNB) + myBB(BNB) + 
                             + myChaikinVol(BNB) + 
                             + myMACD(BNB) + mySAR(BNB))
set.seed(1234)
rf <- buildModel(data.model, method='randomForest',
                 training.per=c(start(BNB), index(BNB["2022-03-05"])),
                 ntree=50, importance=T)
varImpPlot(rf@fitted.model, type=1)
imp <- importance(rf@fitted.model, type=1)
rownames(imp)[which(imp > 10)]
# labiausiai tinkami myADX, myMACD parametrai
rownames(imp)[which(imp > 8)]
# 5 geriausi - mySMI, myADX, myMACD, mySAR
# juos paliekam galutiniam modeliui

# Final model:
BNBmodel <- specifyModel(T.ind(BNB) ~ mySMI(BNB) + 
                           + myADX(BNB) +
                           + myMACD(BNB) + mySAR(BNB))

BNBtrain <- as.data.frame(modelData(BNBmodel,
                                    data.window=c('2018-03-05', '2022-03-02')))
BNBeval <- na.omit(as.data.frame(modelData(BNBmodel,
                                           data.window=c('2022-03-03', '2023-03-02'))))
BNBform <- as.formula('T.ind.BNB ~ .')

singleModelSelect(BNB, BNBform, BNBtrain, BNBeval)

slideModelSelect(BNB, BNBform, BNBtrain, BNBeval)

growModelSelect(BNB, BNBform, BNBtrain, BNBeval)

# Grow, svmC, pol2, 120
model <- learner("MC.svmC", list(maxit=100, linout=T, trace=F, size=10, decay=0.001))
preds <- growingWindowTest(model, BNBform, BNBtrain, BNBeval, relearn.step=120)
signals <- factor(preds, levels=1:3, labels=c('s', 'h', 'b'))
date <- rownames(BNBeval)[1]
market <- BNB[paste(date, '/', sep='')][1:length(signals),]
trade.res <- trading.simulator(market, signals, policy.func = 'pol2')
plot(trade.res, market, theme='white', name='BNB final test')

# get returns
rets <- Return.calculate(trade.res@trading$Equity, method = "discrete")
BNBrets <- na.omit(rets)
BNBrets <- as.timeSeries(BNBrets)
colnames(BNBrets) <- 'BNBret'

chart.CumReturns(BNBrets, legend.loc="topleft", main="Cumulative Daily Returns for BNB")

RTRN <- cbind(RTRN, BNBrets)

################################################################################
XRP <- xts(XRP[,2:15], order.by=XRP$Date)

data.model <- specifyModel(T.ind(XRP) ~ Delt(Cl(XRP), k=1:10) +
                             + myATR(XRP) + mySMI(XRP) +
                             + myADX(XRP) + myBB(XRP) + 
                             + myChaikinVol(XRP) + 
                             + myMACD(XRP) + mySAR(XRP))
set.seed(1234)
rf <- buildModel(data.model, method='randomForest',
                 training.per=c(start(XRP), index(XRP["2022-03-05"])),
                 ntree=50, importance=T)
varImpPlot(rf@fitted.model, type=1)
imp <- importance(rf@fitted.model, type=1)
rownames(imp)[which(imp > 10)]
# labiausiai tinkami myATR, mySMI, mySAR parametrai
rownames(imp)[which(imp > 8)]
# 5 geriausi - mySMI, myADX, myMACD, mySAR
# juos paliekam galutiniam modeliui

# Final model:
XRPmodel <- specifyModel(T.ind(XRP) ~ mySMI(XRP) + 
                           + myADX(XRP) +
                           + myMACD(XRP) + mySAR(XRP))

XRPtrain <- as.data.frame(modelData(XRPmodel,
                                    data.window=c('2018-03-05', '2022-03-02')))
XRPeval <- na.omit(as.data.frame(modelData(XRPmodel,
                                           data.window=c('2022-03-03', '2023-03-02'))))
XRPform <- as.formula('T.ind.XRP ~ .')

singleModelSelect(XRP, XRPform, XRPtrain, XRPeval)

slideModelSelect(XRP, XRPform, XRPtrain, XRPeval)

growModelSelect(XRP, XRPform, XRPtrain, XRPeval)

# Grow, svmC, pol2, 120

model <- learner("MC.svmC", list(maxit=100, linout=T, trace=F, size=10, decay=0.001))
preds <- growingWindowTest(model, XRPform, XRPtrain, XRPeval, relearn.step=120)
signals <- factor(preds, levels=1:3, labels=c('s', 'h', 'b'))
date <- rownames(XRPeval)[1]
market <- XRP[paste(date, '/', sep='')][1:length(signals),]
trade.res <- trading.simulator(market, signals, policy.func = 'pol2')
plot(trade.res, market, theme='white', name='XRP final test')

# get returns
rets <- Return.calculate(trade.res@trading$Equity, method = "discrete")
XRPrets <- na.omit(rets)
XRPrets <- as.timeSeries(XRPrets)
colnames(XRPrets) <- 'XRPret'

chart.CumReturns(XRPrets, legend.loc="topleft", main="Cumulative Daily Returns for XRP")

RTRN <- cbind(RTRN, XRPrets)

################################################################################
ADA <- xts(ADA[,2:16], order.by=ADA$Date)

data.model <- specifyModel(T.ind(ADA) ~ Delt(Cl(ADA), k=1:10) +
                             + myATR(ADA) + mySMI(ADA) +
                             + myADX(ADA) + myBB(ADA) + 
                             + myChaikinVol(ADA) + 
                             + myMACD(ADA) + mySAR(ADA))
set.seed(1234)
rf <- buildModel(data.model, method='randomForest',
                 training.per=c(start(ADA), index(ADA["2022-03-05"])),
                 ntree=50, importance=T)
varImpPlot(rf@fitted.model, type=1)
imp <- importance(rf@fitted.model, type=1)
rownames(imp)[which(imp > 10)]
# labiausiai tinkami myATR, mySMI, mySAR parametrai
rownames(imp)[which(imp > 8)]
# 5 geriausi - myATR, mySMI, myADX, myMACD
# juos paliekam galutiniam modeliui

# Final model:
ADAmodel <- specifyModel(T.ind(ADA) ~ myATR(ADA) + 
                           + mySMI(ADA) +
                           + myADX(ADA) + myMACD(ADA))

ADAtrain <- as.data.frame(modelData(ADAmodel,
                                    data.window=c('2018-03-05', '2022-03-02')))
ADAeval <- na.omit(as.data.frame(modelData(ADAmodel,
                                           data.window=c('2022-03-03', '2023-03-02'))))
ADAform <- as.formula('T.ind.ADA ~ .')

singleModelSelect(ADA, ADAform, ADAtrain, ADAeval)

slideModelSelect(ADA, ADAform, ADAtrain, ADAeval)

growModelSelect(ADA, ADAform, ADAtrain, ADAeval)

# Slide, earth, pol2, 120
model <- learner("MC.earth")
preds <- slidingWindowTest(model, ADAform, ADAtrain, ADAeval, relearn.step=120)
signals <- factor(preds, levels=1:3, labels=c('s', 'h', 'b'))
date <- rownames(ADAeval)[1]
market <- ADA[paste(date, '/', sep='')][1:length(signals),]
trade.res <- trading.simulator(market, signals, policy.func = 'pol1')
plot(trade.res, market, theme='white', name='ADA final test')

# get returns
rets <- Return.calculate(trade.res@trading$Equity, method = "discrete")
ADArets <- na.omit(rets)
ADArets <- as.timeSeries(ADArets)
colnames(ADArets) <- 'ADAret'

chart.CumReturns(ADArets, legend.loc="topleft", main="Cumulative Daily Returns for ADA")

RTRN <- cbind(RTRN, ADArets)

################################################################################
DOGE <- xts(DOGE[,2:16], order.by=DOGE$Date)

data.model <- specifyModel(T.ind(DOGE) ~ Delt(Cl(DOGE), k=1:10) + 
                             + myATR(DOGE) + mySMI(DOGE) +
                             + myADX(DOGE) + myBB(DOGE) + 
                             + myChaikinVol(DOGE) + 
                             + myMACD(DOGE) + mySAR(DOGE))
set.seed(1234)
rf <- buildModel(data.model, method='randomForest',
                 training.per=c(start(DOGE), index(DOGE["2022-03-05"])),
                 ntree=50, importance=T)
varImpPlot(rf@fitted.model, type=1)
imp <- importance(rf@fitted.model, type=1)
rownames(imp)[which(imp > 10)]
# labiausiai tinkami myATR, mySMI, mySAR parametrai
rownames(imp)[which(imp > 5)]
# 5 geriausi - myATR, mySMI, myBB
# juos paliekam galutiniam modeliui

# Final model:
DOGEmodel <- specifyModel(T.ind(DOGE) ~ myATR(DOGE) + 
                            + mySMI(DOGE) + myBB(DOGE))

DOGEtrain <- as.data.frame(modelData(DOGEmodel,
                                    data.window=c('2018-03-05', '2022-03-02')))
DOGEeval <- na.omit(as.data.frame(modelData(DOGEmodel,
                                           data.window=c('2022-03-03', '2023-03-02'))))
DOGEform <- as.formula('T.ind.DOGE ~ .')

singleModelSelect(DOGE, DOGEform, DOGEtrain, DOGEeval)

slideModelSelect(DOGE, DOGEform, DOGEtrain, DOGEeval)

growModelSelect(DOGE, DOGEform, DOGEtrain, DOGEeval)

# Grow, svmR, pol2, 120
model <- learner("MC.svmR", list(maxit=100, linout=T, trace=F, size=10, decay=0.001))
preds <- growingWindowTest(model, DOGEform, DOGEtrain, DOGEeval, relearn.step=120)
signals <- factor(preds, levels=1:3, labels=c('s', 'h', 'b'))
date <- rownames(DOGEeval)[1]
market <- DOGE[paste(date, '/', sep='')][1:length(signals),]
trade.res <- trading.simulator(market, signals, policy.func = 'pol2')
plot(trade.res, market, theme='white', name='DOGE final test')

# get returns
rets <- Return.calculate(trade.res@trading$Equity, method = "discrete")
DOGErets <- na.omit(rets)
DOGErets <- as.timeSeries(DOGErets)
colnames(DOGErets) <- 'DOGEret'

chart.CumReturns(DOGErets, legend.loc="topleft", main="Cumulative Daily Returns for DOGE")

RTRN <- cbind(RTRN, DOGErets)

################################################################################
LTC <- xts(LTC[,2:16], order.by=LTC$Date)

data.model <- specifyModel(T.ind(LTC) ~ Delt(Cl(LTC), k=1:10) +
                             + myATR(LTC) + mySMI(LTC) +
                             + myADX(LTC) + myBB(LTC) + 
                             + myChaikinVol(LTC) + 
                             + myMACD(LTC) + mySAR(LTC))
set.seed(1234)
rf <- buildModel(data.model, method='randomForest',
                 training.per=c(start(LTC), index(LTC["2022-03-05"])),
                 ntree=50, importance=T)
varImpPlot(rf@fitted.model, type=1)
imp <- importance(rf@fitted.model, type=1)
rownames(imp)[which(imp > 10)]
# labiausiai tinkami myATR, mySMI, mySAR parametrai
rownames(imp)[which(imp > 8)]
# 5 geriausi - myATR, mySMI, myADX, myMACD, mySAR
# juos paliekam galutiniam modeliui

# Final model:
LTCmodel <- specifyModel(T.ind(LTC) ~ myATR(LTC) + 
                           + mySMI(LTC) + myADX(LTC) + 
                           + myMACD(LTC) + mySAR(LTC))

LTCtrain <- as.data.frame(modelData(LTCmodel,
                                     data.window=c('2018-03-05', '2022-03-02')))
LTCeval <- na.omit(as.data.frame(modelData(LTCmodel,
                                            data.window=c('2022-03-03', '2023-03-02'))))
LTCform <- as.formula('T.ind.LTC ~ .')

singleModelSelect(LTC, LTCform, LTCtrain, LTCeval)

slideModelSelect(LTC, LTCform, LTCtrain, LTCeval)

growModelSelect(LTC, LTCform, LTCtrain, LTCeval)

# Slide, earth, pol2, 120
model <- learner("MC.earth")
preds <- slidingWindowTest(model, LTCform, LTCtrain, LTCeval, relearn.step=120)
signals <- factor(preds, levels=1:3, labels=c('s', 'h', 'b'))
date <- rownames(LTCeval)[1]
market <- LTC[paste(date, '/', sep='')][1:length(signals),]
trade.res <- trading.simulator(market, signals, policy.func = 'pol2')
plot(trade.res, market, theme='white', name='LTC final test')

# get returns
rets <- Return.calculate(trade.res@trading$Equity, method = "discrete")
LTCrets <- na.omit(rets)
LTCrets <- as.timeSeries(LTCrets)
colnames(LTCrets) <- 'LTCret'

chart.CumReturns(LTCrets, legend.loc="topleft", main="Cumulative Daily Returns for LTC")

RTRN <- cbind(RTRN, LTCrets)

################################################################################
TRX <- xts(TRX[,2:14], order.by=TRX$Date)

data.model <- specifyModel(T.ind(TRX) ~ Delt(Cl(TRX), k=1:10) +
                             + myATR(TRX) + mySMI(TRX) +
                             + myADX(TRX) + myBB(TRX) + 
                             + myChaikinVol(TRX) + 
                             + myMACD(TRX) + mySAR(TRX))
set.seed(1234)
rf <- buildModel(data.model, method='randomForest',
                 training.per=c(start(TRX), index(TRX["2022-03-05"])),
                 ntree=50, importance=T)
varImpPlot(rf@fitted.model, type=1)
imp <- importance(rf@fitted.model, type=1)
rownames(imp)[which(imp > 10)]
# labiausiai tinkami myATR, mySMI, mySAR parametrai
rownames(imp)[which(imp > 8)]
# 5 geriausi - myATR, mySMI, myADX, myMACD, mySAR
# juos paliekam galutiniam modeliui

# Final model:
TRXmodel <- specifyModel(T.ind(TRX) ~ myATR(TRX) + 
                           + mySMI(TRX) + myADX(TRX) + 
                           + myMACD(TRX) + mySAR(TRX))

TRXtrain <- as.data.frame(modelData(TRXmodel,
                                    data.window=c('2018-03-05', '2022-03-02')))
TRXeval <- na.omit(as.data.frame(modelData(TRXmodel,
                                           data.window=c('2022-03-03', '2023-03-02'))))
TRXform <- as.formula('T.ind.TRX ~ .')

singleModelSelect(TRX, TRXform, TRXtrain, TRXeval)

slideModelSelect(TRX, TRXform, TRXtrain, TRXeval)

growModelSelect(TRX, TRXform, TRXtrain, TRXeval)

# Slide, earth, pol2, 120
model <- learner("MC.earth")
preds <- slidingWindowTest(model, TRXform, TRXtrain, TRXeval, relearn.step=120)
signals <- factor(preds, levels=1:3, labels=c('s', 'h', 'b'))
date <- rownames(TRXeval)[1]
market <- TRX[paste(date, '/', sep='')][1:length(signals),]
trade.res <- trading.simulator(market, signals, policy.func = 'pol2')
plot(trade.res, market, theme='white', name='TRX final test')

# get returns
rets <- Return.calculate(trade.res@trading$Equity, method = "discrete")
TRXrets <- na.omit(rets)
TRXrets <- as.timeSeries(TRXrets)
colnames(TRXrets) <- 'TRXret'

chart.CumReturns(TRXrets, legend.loc="topleft", main="Cumulative Daily Returns for TRX")

RTRN <- cbind(RTRN, TRXrets)

################################################################################
LINK <- xts(LINK[,2:14], order.by=LINK$Date)

data.model <- specifyModel(T.ind(LINK) ~ Delt(Cl(LINK), k=1:10) +
                             + myATR(LINK) + mySMI(LINK) +
                             + myADX(LINK) + myBB(LINK) + 
                             + myChaikinVol(LINK) + 
                             + myMACD(LINK) + mySAR(LINK))
set.seed(1234)
rf <- buildModel(data.model, method='randomForest',
                 training.per=c(start(LINK), index(LINK["2022-03-05"])),
                 ntree=50, importance=T)
varImpPlot(rf@fitted.model, type=1)
imp <- importance(rf@fitted.model, type=1)
rownames(imp)[which(imp > 10)]
# labiausiai tinkami myATR, mySMI, mySAR parametrai
rownames(imp)[which(imp > 8)]
# 5 geriausi - myATR, mySMI, myADX, myMACD, mySAR
# juos paliekam galutiniam modeliui

# Final model:
LINKmodel <- specifyModel(T.ind(LINK) ~ myATR(LINK) + 
                            + mySMI(LINK) + myADX(LINK) + 
                            + myMACD(LINK) + mySAR(LINK))

LINKtrain <- as.data.frame(modelData(LINKmodel,
                                    data.window=c('2018-03-05', '2022-03-02')))
LINKeval <- na.omit(as.data.frame(modelData(LINKmodel,
                                           data.window=c('2022-03-03', '2023-03-02'))))
LINKform <- as.formula('T.ind.LINK ~ .')

singleModelSelect(LINK, LINKform, LINKtrain, LINKeval)

slideModelSelect(LINK, LINKform, LINKtrain, LINKeval)

growModelSelect(LINK, LINKform, LINKtrain, LINKeval)

# Grow, earth, pol2, 120
model <- learner("MC.earth")
preds <- growingWindowTest(model, LINKform, LINKtrain, LINKeval, relearn.step=120)
signals <- factor(preds, levels=1:3, labels=c('s', 'h', 'b'))
date <- rownames(LINKeval)[1]
market <- LINK[paste(date, '/', sep='')][1:length(signals),]
trade.res <- trading.simulator(market, signals, policy.func = 'pol2')
plot(trade.res, market, theme='white', name='LINK final test')

# get returns
rets <- Return.calculate(trade.res@trading$Equity, method = "discrete")
LINKrets <- na.omit(rets)
LINKrets <- as.timeSeries(LINKrets)
colnames(LINKrets) <- 'LINKret'

chart.CumReturns(LINKrets, legend.loc="topleft", main="Cumulative Daily Returns for LINK")

RTRN <- cbind(RTRN, LINKrets)

################################################################################
BCH <- xts(BCH[,2:14], order.by=BCH$Date)

data.model <- specifyModel(T.ind(BCH) ~ Delt(Cl(BCH), k=1:10) +
                             + myATR(BCH) + mySMI(BCH) +
                             + myADX(BCH) + myBB(BCH) + 
                             + myChaikinVol(BCH) + 
                             + myMACD(BCH) + mySAR(BCH))
set.seed(1234)
rf <- buildModel(data.model, method='randomForest',
                 training.per=c(start(BCH), index(BCH["2022-03-05"])),
                 ntree=50, importance=T)
varImpPlot(rf@fitted.model, type=1)
imp <- importance(rf@fitted.model, type=1)
rownames(imp)[which(imp > 10)]
# labiausiai tinkami myATR, mySMI, mySAR parametrai
rownames(imp)[which(imp > 8)]
# 5 geriausi - myATR, mySMI, myMACD, mySAR
# juos paliekam galutiniam modeliui

# Final model:
BCHmodel <- specifyModel(T.ind(BCH) ~ myATR(BCH) + 
                           + mySMI(BCH) + 
                           + myMACD(BCH) + mySAR(BCH))

BCHtrain <- as.data.frame(modelData(BCHmodel,
                                     data.window=c('2018-03-05', '2022-03-02')))
BCHeval <- na.omit(as.data.frame(modelData(BCHmodel,
                                            data.window=c('2022-03-03', '2023-03-02'))))
BCHform <- as.formula('T.ind.BCH ~ .')

singleModelSelect(BCH, BCHform, BCHtrain, BCHeval)

slideModelSelect(BCH, BCHform, BCHtrain, BCHeval)

growModelSelect(BCH, BCHform, BCHtrain, BCHeval)

# Slide, svmC, pol2, 30
model <- learner("MC.svmC")
preds <- slidingWindowTest(model, BCHform, BCHtrain, BCHeval, relearn.step=30)
signals <- factor(preds, levels=1:3, labels=c('s', 'h', 'b'))
date <- rownames(BCHeval)[1]
market <- BCH[paste(date, '/', sep='')][1:length(signals),]
trade.res <- trading.simulator(market, signals, policy.func = 'pol2')
plot(trade.res, market, theme='white', name='BCH final test')

# get returns
rets <- Return.calculate(trade.res@trading$Equity, method = "discrete")
BCHrets <- na.omit(rets)
BCHrets <- as.timeSeries(BCHrets)
colnames(BCHrets) <- 'BCHret'

chart.CumReturns(BCHrets, legend.loc="topleft", main="Cumulative Daily Returns for BCH")

RTRN <- cbind(RTRN, BCHrets)

################################################################################
EOS <- xts(EOS[,2:13], order.by=EOS$Date)

data.model <- specifyModel(T.ind(EOS) ~ Delt(Cl(EOS), k=1:10) +
                             + myATR(EOS) + mySMI(EOS) +
                             + myADX(EOS) + myBB(EOS) + 
                             + myChaikinVol(EOS) + 
                             + myMACD(EOS) + mySAR(EOS))
set.seed(1234)
rf <- buildModel(data.model, method='randomForest',
                 training.per=c(start(EOS), index(EOS["2022-03-05"])),
                 ntree=50, importance=T)
varImpPlot(rf@fitted.model, type=1)
imp <- importance(rf@fitted.model, type=1)
rownames(imp)[which(imp > 10)]
# labiausiai tinkami myATR, mySMI, mySAR parametrai
rownames(imp)[which(imp > 8)]
# 5 geriausi - myATR, mySMI, myADX, myMACD, mySAR
# juos paliekam galutiniam modeliui

# Final model:
EOSmodel <- specifyModel(T.ind(EOS) ~ myATR(EOS) + 
                           + mySMI(EOS) + myADX(EOS)
                         + myMACD(EOS) + mySAR(EOS))

EOStrain <- as.data.frame(modelData(EOSmodel,
                                    data.window=c('2018-03-05', '2022-03-02')))
EOSeval <- na.omit(as.data.frame(modelData(EOSmodel,
                                           data.window=c('2022-03-03', '2023-03-02'))))
EOSform <- as.formula('T.ind.EOS ~ .')

singleModelSelect(EOS, EOSform, EOStrain, EOSeval)

slideModelSelect(EOS, EOSform, EOStrain, EOSeval)

growModelSelect(EOS, EOSform, EOStrain, EOSeval)

# Grow, svmC, pol1, 120
model <- learner("MC.svmC", list(maxit=100, linout=T, trace=F, size=10, decay=0.001))
preds <- growingWindowTest(model, EOSform, EOStrain, EOSeval, relearn.step=120)
signals <- factor(preds, levels=1:3, labels=c('s', 'h', 'b'))
date <- rownames(EOSeval)[1]
market <- EOS[paste(date, '/', sep='')][1:length(signals),]
trade.res <- trading.simulator(market, signals, policy.func = 'pol1')
plot(trade.res, market, theme='white', name='EOS final test')

# get returns
rets <- Return.calculate(trade.res@trading$Equity, method = "discrete")
EOSrets <- na.omit(rets)
EOSrets <- as.timeSeries(EOSrets)
colnames(EOSrets) <- 'EOSret'

chart.CumReturns(EOSrets, legend.loc="topleft", main="Cumulative Daily Returns for EOS")

RTRN <- cbind(RTRN, EOSrets)

################################################################################
OMG <- xts(OMG[,2:15], order.by=OMG$Date)

data.model <- specifyModel(T.ind(OMG) ~ Delt(Cl(OMG), k=1:10) +
                             + myATR(OMG) + mySMI(OMG) +
                             + myADX(OMG) + myBB(OMG) + 
                             + myChaikinVol(OMG) + 
                             + myMACD(OMG) + mySAR(OMG))
set.seed(1234)
rf <- buildModel(data.model, method='randomForest',
                 training.per=c(start(OMG), index(OMG["2022-03-05"])),
                 ntree=50, importance=T)
varImpPlot(rf@fitted.model, type=1)
imp <- importance(rf@fitted.model, type=1)
rownames(imp)[which(imp > 10)]
# labiausiai tinkami myATR, mySMI, mySAR parametrai
rownames(imp)[which(imp > 6)]
# 5 geriausi - mySMI, myADX, myMACD, mySAR
# juos paliekam galutiniam modeliui

# Final model:
OMGmodel <- specifyModel(T.ind(OMG) ~ mySMI(OMG) + myADX(OMG)
                         + myMACD(OMG) + mySAR(OMG))

OMGtrain <- as.data.frame(modelData(OMGmodel,
                                    data.window=c('2018-03-05', '2022-03-02')))
OMGeval <- na.omit(as.data.frame(modelData(OMGmodel,
                                           data.window=c('2022-03-03', '2023-03-02'))))
OMGform <- as.formula('T.ind.OMG ~ .')

singleModelSelect(OMG, OMGform, OMGtrain, OMGeval)

slideModelSelect(OMG, OMGform, OMGtrain, OMGeval)

growModelSelect(OMG, OMGform, OMGtrain, OMGeval)

# Grow, earth, pol2, 60
model <- learner("MC.earth")
preds <- growingWindowTest(model, OMGform, OMGtrain, OMGeval, relearn.step=60)
signals <- factor(preds, levels=1:3, labels=c('s', 'h', 'b'))
date <- rownames(OMGeval)[1]
market <- OMG[paste(date, '/', sep='')][1:length(signals),]
trade.res <- trading.simulator(market, signals, policy.func = 'pol2')
plot(trade.res, market, theme='white', name='OMG final test')

# get returns
rets <- Return.calculate(trade.res@trading$Equity, method = "discrete")
OMGrets <- na.omit(rets)
OMGrets <- as.timeSeries(OMGrets)
colnames(OMGrets) <- 'OMGret'

chart.CumReturns(OMGrets, legend.loc="topleft", main="Cumulative Daily Returns for OMG")

RTRN <- cbind(RTRN, OMGrets)

################################################################################
XMR <- xts(XMR[,2:12], order.by=XMR$Date)

data.model <- specifyModel(T.ind(XMR) ~ Delt(Cl(XMR), k=1:10) +
                             + myATR(XMR) + mySMI(XMR) +
                             + myADX(XMR) + myBB(XMR) + 
                             + myChaikinVol(XMR) + 
                             + myMACD(XMR) + mySAR(XMR))
set.seed(1234)
rf <- buildModel(data.model, method='randomForest',
                 training.per=c(start(XMR), index(XMR["2022-03-05"])),
                 ntree=50, importance=T)
varImpPlot(rf@fitted.model, type=1)
imp <- importance(rf@fitted.model, type=1)
rownames(imp)[which(imp > 10)]
# labiausiai tinkami myATR, mySMI, mySAR parametrai
rownames(imp)[which(imp > 5)]
# 5 geriausi - myADX, mySAR
# juos paliekam galutiniam modeliui

# Final model:
XMRmodel <- specifyModel(T.ind(XMR) ~ myADX(XMR)
                         + mySAR(XMR))


XMRtrain <- as.data.frame(modelData(XMRmodel,
                                    data.window=c('2018-03-05', '2022-03-02')))
XMReval <- na.omit(as.data.frame(modelData(XMRmodel,
                                           data.window=c('2022-03-03', '2023-03-02'))))
XMRform <- as.formula('T.ind.XMR ~ .')

singleModelSelect(XMR, XMRform, XMRtrain, XMReval)

slideModelSelect(XMR, XMRform, XMRtrain, XMReval)

growModelSelect(XMR, XMRform, XMRtrain, XMReval)

# Grow, svmR, pol1, 120
model <- learner("MC.svmR", list(maxit=100, linout=T, trace=F, size=10, decay=0.001))
preds <- growingWindowTest(model, EOSform, EOStrain, EOSeval, relearn.step=120)
signals <- factor(preds, levels=1:3, labels=c('s', 'h', 'b'))
date <- rownames(XMReval)[1]
market <- XMR[paste(date, '/', sep='')][1:length(signals),]
trade.res <- trading.simulator(market, signals, policy.func = 'pol1')
plot(trade.res, market, theme='white', name='XMR final test')

# get returns
rets <- Return.calculate(trade.res@trading$Equity, method = "discrete")
XMRrets <- na.omit(rets)
XMRrets <- as.timeSeries(XMRrets)
colnames(XMRrets) <- 'XMRret'

chart.CumReturns(XMRrets, legend.loc="topleft", main="Cumulative Daily Returns for XMR")

RTRN <- cbind(RTRN, XMRrets)

################################################################################
ZEC <- xts(ZEC[,2:15], order.by=ZEC$Date)

data.model <- specifyModel(T.ind(ZEC) ~ Delt(Cl(ZEC), k=1:10) +
                             + myATR(ZEC) + mySMI(ZEC) +
                             + myADX(ZEC) + myBB(ZEC) + 
                             + myChaikinVol(ZEC) + 
                             + myMACD(ZEC) + mySAR(ZEC))
set.seed(1234)
rf <- buildModel(data.model, method='randomForest',
                 training.per=c(start(ZEC), index(ZEC["2022-03-05"])),
                 ntree=50, importance=T)
varImpPlot(rf@fitted.model, type=1)
imp <- importance(rf@fitted.model, type=1)
rownames(imp)[which(imp > 10)]
# labiausiai tinkami myATR, mySMI, myADX, myMACD, mySAR parametrai
rownames(imp)[which(imp > 8)]
# 5 geriausi - myADX, mySAR
# juos paliekam galutiniam modeliui

# Final model:
ZECmodel <- specifyModel(T.ind(ZEC) ~ myATR(ZEC) + 
                           + mySMI(ZEC) + myADX(ZEC)
                         + myMACD(ZEC) + mySAR(ZEC))

ZECtrain <- as.data.frame(modelData(ZECmodel,
                                    data.window=c('2018-03-05', '2022-03-02')))
ZECeval <- na.omit(as.data.frame(modelData(ZECmodel,
                                           data.window=c('2022-03-03', '2023-03-02'))))
ZECform <- as.formula('T.ind.ZEC ~ .')

singleModelSelect(ZEC, ZECform, ZECtrain, ZECeval)

slideModelSelect(ZEC, ZECform, ZECtrain, ZECeval)

growModelSelect(ZEC, ZECform, ZECtrain, ZECeval)

# Grow, svmC, pol2, 120
model <- learner("MC.svmC", list(maxit=100, linout=T, trace=F, size=10, decay=0.001))
preds <- growingWindowTest(model, ZECform, ZECtrain, ZECeval, relearn.step=120)
signals <- factor(preds, levels=1:3, labels=c('s', 'h', 'b'))
date <- rownames(ZECeval)[1]
market <- ZEC[paste(date, '/', sep='')][1:length(signals),]
trade.res <- trading.simulator(market, signals, policy.func = 'pol2')
plot(trade.res, market, theme='white', name='ZEC final test')

# get returns
rets <- Return.calculate(trade.res@trading$Equity, method = "discrete")
ZECrets <- na.omit(rets)
ZECrets <- as.timeSeries(ZECrets)
colnames(ZECrets) <- 'ZECret'

chart.CumReturns(ZECrets, legend.loc="topleft", main="Cumulative Daily Returns for ZEC")

RTRN <- cbind(RTRN, ZECrets)

RTRN <- xts(RTRN[,2:15], order.by = RTRN$Date)
RTRN <- as.timeSeries(RTRN)
portRetData <- portfolioData(data = RTRN, spec = portfolioSpec())

##############################################################

# PORTFOLIO OPTIMIZATION #

# First portfolio #
names <- c("BTC", "ETH", "BNB", "XRP", "ADA", "DOGE", "LTC", "TRX", "LINK", "BCH", "EOS", "OMG", "XMR", "ZEC")

portf1 <- portfolio.spec(names)
#portf1 <- add.constraint(portf1, type="full_investment")
portf1 <- add.constraint(portf1, type="weight_sum", 
                         min_sum=0.99, max_sum=1.01)
portf1 <- add.constraint(portf1, type="box", min=0, max=0.5)
portf1 <- add.constraint(portf1, type="position_limit", max_pos=14)

rp1 <- random_portfolios(portf1, 100, "sample")
opt1 <- optimize.portfolio(RTRN, portfolio=portf1, 
                           optimize_method="random", rp=rp1,
                           trace=TRUE)
print(opt1)

w1 <- c(rep(0.07142857, 14))
port.ret1 <- Return.portfolio(RTRN, opt1$weights)
print(port.ret1) # returns are sometimes negative
chart.Weights(opt1)

charts.PerformanceSummary(port.ret1)

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

# Third portfolio - not used later#

portf3 <- portfolio.spec(names)
portf3 <- add.constraint(portf3, type="weight_sum", 
                         min_sum=0.99, max_sum=1.01)
portf3 <- add.constraint(portf3, type="box", min=0, max=0.5)
portf3 <- add.constraint(portf3, type="position_limit", max_pos=14)
portf3 <- add.objective(portf3, type="return", name="mean",
                        target=0.0015)
portf3 <- add.objective(portf3, type="risk", name="StdDev",
                        target=0.02)
rp3 <- random_portfolios(portf3, 20, "sample")
opt3 <- optimize.portfolio(RTRN, portfolio=portf3,
                           optimize_method="random", rp=rp3,
                           trace=TRUE)
print(opt3)

port.ret3 <- Return.portfolio(RTRN, opt3$weights)
print(port.ret3) # returns are positive but very small
chart.Weights(opt3)
# up to 15% returns
charts.PerformanceSummary(port.ret3)

port.ret4 <- Return.portfolio(RTRN, opt3$weights, wealth.index = TRUE)
port.ret4

port.ret5 <- Return.portfolio(RTRN, opt3$weights, verbose = TRUE)
port.ret5

chart.CumReturns(port.ret5$returns)
chart.StackedBar(port.ret5$BOP.Weight)
chart.StackedBar(port.ret5$BOP.Value)

opt.dn.reb <- optimize.portfolio.rebalancing(RTRN, portfolio = portf.dn,
                                             optimize_method = "DEoptim",
                                             search_size = 50, trace = FALSE,
                                             rebalance_on = "years",
                                             rp = rp)
chart.Weights(opt.dn.reb, main="CRRA Weights", col=bluemono)
chart.RiskReward(opt.dn, risk.col = "StdDev")
charts.PerformanceSummary(port.returns)
