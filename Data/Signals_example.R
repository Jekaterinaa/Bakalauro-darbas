library(tseries)
library(quantmod)
library(DMwR)

####################
T.ind <- function(quotes, tgt.margin = 0.025, n.days = 10) {
  v <- apply(HLC(quotes), 1, mean)
  r <- matrix(NA, ncol = n.days, nrow = NROW(quotes))
  for (x in 1:n.days) r[, x] <- Next(Delt(v, k = x), x)
  x <- apply(r, 1, function(x) sum(x[x > tgt.margin | x < (-tgt.margin)]))
  if (is.xts(quotes))
    xts(x, time(quotes))
  else x
}

data(GSPC)
candleChart(last(GSPC,"3 months"), theme="white", TA=NULL)
avgPrice <- function(p) apply(HLC(p), 1, mean)
addAvgPrice <- newTA(FUN = avgPrice, col = 1, legend = "AvgPrice")
addT.ind <- newTA(FUN = T.ind, col = "red", legend = "tgtRet")
addAvgPrice(on = 1)
addT.ind()

myATR <- function(x) ATR(HLC(x))[, "atr"]
mySMI <- function(x) SMI(HLC(x))[, "SMI"]
myADX <- function(x) ADX(HLC(x))[, "ADX"]
myAroon <- function(x) aroon(x[, c("High", "Low")])$oscillator
myBB <- function(x) BBands(HLC(x))[, "pctB"]
myChaikinVol <- function(x) Delt(chaikinVolatility(x[, c("High","Low")]))[, 1]
myCLV <- function(x) EMA(CLV(HLC(x)))[, 1]
myEMV <- function(x) EMV(x[, c("High", "Low")], x[, "Volume"])[,2]
myMACD <- function(x) MACD(Cl(x))[, 2]
myMFI <- function(x) MFI(x[, c("High", "Low", "Close")],x[, "Volume"])
mySAR <- function(x) SAR(x[, c("High", "Close")])[, 1]
myVolat <- function(x) volatility(OHLC(x), calc = "garman")[,1]

data(GSPC)
library(randomForest)
data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1:10) +
                               +        myATR(GSPC) + mySMI(GSPC) + myADX(GSPC) + myAroon(GSPC) +
                               +        myBB(GSPC)  + myChaikinVol(GSPC) + myCLV(GSPC) +
                               +        CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) +
                               +        myVolat(GSPC)  + myMACD(GSPC) + myMFI(GSPC) + RSI(Cl(GSPC)) +
                               +        mySAR(GSPC) + runMean(Cl(GSPC)) + runSD(Cl(GSPC)))
set.seed(1234)
rf <- buildModel(data.model,method='randomForest', training.per=c(start(GSPC),index(GSPC["1999-12-31"])), ntree=50, importance=T)
ex.model <- specifyModel(T.ind(IBM) ~ Delt(Cl(IBM), k = 1:3))
data <- modelData(ex.model, data.window = c("2009-01-01","2009-08-10"))


data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC), k = 1) +
                          +myATR(GSPC) + myADX(GSPC) + myEMV(GSPC) + myVolat(GSPC) +
                          +myMACD(GSPC) + mySAR(GSPC) + runMean(Cl(GSPC)))

Tdata.train <- as.data.frame(modelData(data.model,data.window=c('1970-01-02','1999-12-31')))
Tdata.eval <- na.omit(as.data.frame(modelData(data.model, data.window=c('2000-01-01', '2009-09-15'))))
Tform <- as.formula('T.ind.GSPC~.')

# ANN
set.seed(1234)  
library(nnet)  

norm.data <- scale(Tdata.train)  
nn <- nnet(Tform, norm.data[1:1000,], size=10, decay=0.01, maxit=1000, linout=T, trace=F)
norm.preds <- predict(nn, norm.data[1001:2000,])  
preds <- unscale(norm.preds, norm.data)

sigs.nn <- trading.signals(preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000, "T.ind.GSPC"], 0.1, -0.1)
sigs.PR(sigs.nn, true.sigs)

Tdata.train <- na.omit(Tdata.train)

# SVM
library(e1071)
sv <- svm(Tform, Tdata.train[1:1000, ], gamma = 0.001, cost = 100)
s.preds <- predict(sv, Tdata.train[1001:2000, ])
sigs.svm <- trading.signals(s.preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000, "T.ind.GSPC"], 0.1, -0.1)
sigs.PR(sigs.svm, true.sigs)
