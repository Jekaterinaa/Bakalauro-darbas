library(ggplot2)
library(corrplot)

#################################################################################

read_data <- function(ticker) {
  filename <- paste(ticker, '_daily.csv', sep='')
  df <- read.csv(filename, header=TRUE, sep=';')
  df$Date <- as.Date(df$Date, format="%Y-%m-%d")
  
  return (df)
}

#################################################################################

BTC <- read_data("BTC")
ETH <- read_data("ETH")
BNB <- read_data("BNB")

pl1 <- price_plot("BTC", BTC)
pl2 <- price_plot("ETH", ETH)
pl3 <- price_plot("BNB", BNB)
grid.arrange(pl1, pl2, pl3, nrow=3)

XRP <- read_data("XRP")
ADA <- read_data("ADA")
DOGE <- read_data("DOGE")

pl4 <- price_plot("XRP", XRP)
pl5 <- price_plot("ADA", ADA)
pl6 <- price_plot("DOGE", DOGE)
grid.arrange(pl4, pl5, pl6, nrow=3)

LTC <- read_data("LTC")
TRX <- read_data("TRX")
LINK <- read_data("LINK")

pl7 <- price_plot("LTC", LTC)
pl8 <- price_plot("TRX", TRX)
pl9 <- price_plot("LINK", LINK)
grid.arrange(pl7, pl8, pl9, nrow=3)

BCH <- read_data("BCH")
EOS <- read_data("EOS")
OMG <- read_data("OMG")

pl10 <- price_plot("BCH", BCH)
pl11 <- price_plot("EOS", EOS)
pl12 <- price_plot("OMG", OMG)
grid.arrange(pl10, pl11, pl12, nrow=3)

XMR <- read_data("XMR")
ZEC <- read_data("ZEC")

pl13 <- price_plot("XMR", XMR)
pl14 <- price_plot("ZEC", ZEC)
grid.arrange(pl13, pl14, nrow=3)

#################################################################################

prices <- as.data.frame(BTC$Price)
prices <- cbind(prices, ETH$Price)
prices <- cbind(prices, BNB$Price)
prices <- cbind(prices, XRP$Price)
prices <- cbind(prices, ADA$Price)
prices <- cbind(prices, DOGE$Price)
prices <- cbind(prices, LTC$Price)
prices <- cbind(prices, TRX$Price)
prices <- cbind(prices, LINK$Price)
prices <- cbind(prices, BCH$Price)
prices <- cbind(prices, EOS$Price)
prices <- cbind(prices, OMG$Price)
prices <- cbind(prices, XMR$Price)
prices <- cbind(prices, ZEC$Price)

colnames(prices) <- c("BTC", "ETH", "BNB", "XRP", "ADA", "DOGE", "LTC", "TRX", "LINK", "BCH", "EOS", "OMG", "XMR", "ZEC")

pricesCor <- cor(prices)
pricesCor <- round(pricesCor, 2)

corrplot(pricesCor, method="circle")

corrplot(pricesCor, method="pie")

corrplot(pricesCor, method="color")

corrplot(pricesCor, method="number")
