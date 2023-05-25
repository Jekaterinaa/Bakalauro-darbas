# Example

splitAdjust <- function(prices,symbol) {
  len = length(prices)
  origFinalPrice = prices[len]
  for(j in 2:len) {
    split = 0
    #print(paste(prices[j-1],prices[j]))
    if(prices[j-1] >= 1.4*prices[j]) {
      split = +1.5 # a 3 for 2
      if(prices[j-1] >= 1.8*prices[j])
        split = +2 #At least a 2 for 1
      if(prices[j-1] >= 2.9*prices[j])
        split = +3 #Ah a 3 for 1
      if(prices[j-1] >= 3.9*prices[j])
        split = +4 #Ah a 3 for 1
      if(prices[j-1] >= 4.9*prices[j])
        stop(paste(symbol,'detected more than 4:1 split'))
      print(paste("split adjusting",symbol,split,
                  j,prices[j-1],prices[j])) 
      
    } #reverse splits: price increases so divide
    if(prices[j-1] <= prices[j]/1.4) {
      split = -1.5
      if(prices[j-1] <= prices[j]/1.9 &&
         prices[j-1] >= prices[j]/2.1)
        split = -2
      if(prices[j-1] <= prices[j]/2.9 &&
         prices[j-1] >= prices[j]/3.1)
        split = -3
      if(prices[j-1] <= prices[j]/5.8 &&
         prices[j-1] >= prices[j]/6.2)
        split = -6
      if((prices[j-1] <= prices[j]/7.7) &&
         (prices[j-1] >= prices[j]/8.3))
        split = -8
      if((prices[j-1] <= prices[j]/9.7) &&
         (prices[j-1] >= prices[j]/10.3))
        split = -10
      if((split == 0) && (prices[j-1] <= prices[j]/2.9))
        stop(paste(symbol,
                   'detected more than double reverse split'))
      print(paste("reverse split adjusting",j,symbol,j,
                  split,prices[j-1],prices[j]))
    }
    if(split != 0) {
      for(k in j:len) { #adjust all prices to right from j:len
        if(symbol=="C")
          prices[k] = prices[k]/10 #hard coded for Citi
        else if(split == +1.5)
          prices[k] = 1.5*prices[k] # 3 for 2
        else if(split == +2)
          prices[k] = 2*prices[k] # 2 to 1
        else if(split == +3)
          prices[k] = 3*prices[k] # 3 to 1
        else if(split == +4)
          prices[k] = 4*prices[k] # 4 to 1
        else if(split == -1.5)
          prices[k] = prices[k]/1.5 # 2 to 3 rev
        else if(split == -2)
          prices[k] = prices[k]/2 # 1 to 2 rev
        else if(split == -3)
          prices[k] = prices[k]/3 # 1 to 2 rev
        else if(split == -6)
          prices[k] = prices[k]/6 # 1 to 8 rev
        else if(split == -8)
          prices[k] = prices[k]/8 # 1 to 8 rev
        else if(split == -10)
          prices[k] = prices[k]/10 # 1 to 10 rev
        else stop('splitAdjust internal error')
      } }
  }
  finalPrice = prices[len]
  return(prices*origFinalPrice/finalPrice)
}


findR <- function(prices,isSplitAdjusted=TRUE) {#Find R: logrets:
  len = dim(prices)[1]
  D <<- dim(prices)[2]
  R   = matrix(nrow=(len-1),ncol=D)
  for(i in 1:D) {
    #print(i)
    if(!isSplitAdjusted) prices[,i] <<- splitAdjust(prices[,i],lab[i])
    R[,i] = 100*diff(log(prices[,i])) ###log rets
  }
  R 
}

displayCharts <- function(prices,lab,nrow=3,ncol=4,sleepSecs=4) {
  Dims=length(prices[1,])
  for(chartGrp in (1:ceiling(Dims/(nrow*ncol)))) {
    print(chartGrp)
    par(mar=c(3.82,1.82,1.82,0.82))
    par(mfrow=c(nrow,ncol))
    for(i in 1:(nrow*ncol)) {
      j = ((chartGrp-1)*nrow*ncol+i)
      if(j <= Dims) {
        print(paste(j,lab[j]))
        plot(prices[,j],type="l",xlab=paste(j,lab[j]))
      }
    }
    Sys.sleep(sleepSecs)
  }
}

findCovMat <- function(R) {
  meanv <- apply(R,2,mean)
  cov_mat <- cov(R)
  diag_cov_mat <- diag(cov_mat)
  sdevv <- sqrt(diag(cov_mat))
  list(meanv,cov_mat,diag_cov_mat,sdevv)
}

findWeights <- function(muP,cov_mat,Amat) {
  bvec = c(1,muP,rep(0,D)) #no short sales
  D <- dim(cov_mat)[1]
  result = solve.QP(Dmat=2*cov_mat,dvec=rep(0,D),
                    Amat=Amat,bvec=bvec,meq=2)
  result
}

opt <- function(lab,meanv,cov_mat,isShorting,Nruns=100) {
  if(isShorting) {            #set the constraints matrix
    Amat = cbind(rep(1,D),meanv)
  } else {
    Amat = cbind(rep(1,D),meanv,diag(1,nrow=D)) #no short sales
  }
  u = 1/2
  if(isShorting) {#set of Nruns possible target values
    #for expect portfolio return
    muP = seq(.05,.60,length=Nruns)
  } else {
    muP = seq(min(meanv)+.0001,max(meanv)-.0001,
              length=Nruns) #no short sales
  }
  muP
  sdP = muP # set up storage for sdev of port rets
  weights = matrix(0,nrow=Nruns,ncol=D) #store port weights
  W <- 4
  u <- 1/2
  # find the optimal portfolios for each target expected return
  for (i in 1:length(muP))
  {
    if(isShorting) {
      bvec = c(1,muP[i])  # constraint vector
    } else {
      bvec = c(1,muP[i],rep(0,D)) #no short sales
    }
    #print(paste(2*cov_mat,rep(0,D),Amat,bvec))  
    isPlot = TRUE
    result = solve.QP(Dmat=2*cov_mat,dvec=rep(0,D),
                      Amat=Amat,bvec=bvec,meq=2)
    sdP[i] = sqrt(result$value)
    #weights are contained in result solution
    weights[i,] = result$solution
    mufree = 1.3/daysPerYr # input value of risk-free int rate
    sharpe =(muP-mufree)/sdP # compute Sharpe Ratios
    ind = (sharpe == max(sharpe)) # Find maximum Sharpe Ratio
    if(isPlot && (i%%10)==0) {
      print(i)
      par(mar=c(3.82,2.82,2.82,0.82))
      par(mfrow=c(ceiling((min(10,D+3))/W),W)) #3 extra plots
      for(d in 1:min(49,D)) {
        plot(round(weights[,d],3),xlab=lab[d])
      }
      plot(weights[i,],xlab=paste("weights,i =",i))
      plot(sharpe[1:i],xlab="sharpe",xlim=c(1,Nruns))
      plot(muP[1:i],xlab="mu",xlim=c(1,Nruns))
      Sys.sleep(5*u)
    } }
  Sys.sleep(15*u)
  round(weights[ind,],6)
  
  for (i in 1:length(muP))
    w = vector(length=D)
  w[] = 0
  for(d in (1:D)){
    weight = round(weights[ind,d],3)
    if(weight > .001)
      w[d] = weight
    print(paste(lab[d],weight*100,"%"))
  }
  for(i in 1:Nruns) if(ind[i]) print(i)
  return(w)
}


elimSyms <- function(prices,lab,dir,isSubDir=TRUE) {
  len = dim(prices)[1]
  D = dim(prices)[2]
  #First find removal list in 3 files in each of NYSE and NASDAQ
  indInFile = as.vector(rep(FALSE,D))
  ifelse(isSubDir,subdirVec <- c("NYSE","NASDAQ"),subdirVec <- c(NA))
  for(subdir in subdirVec) {
    if(isSubDir)
      setwd(paste(homeuser,"/FinAnalytics/",dir,"/",subdir,sep=""))
    else
      setwd(paste(homeuser,"/FinAnalytics/",dir,sep=""))
    for(file in c("badsyms.txt","badcors.txt","badsharpes.txt")) {
      badlab = NA
      if(file.exists(file))
        badlab <- read.table(file) # badcors.txt badsharpes.txt")
      if(length(badlab)>1 || !is.na(badlab)) {
        for(l in badlab) {
          print(paste("elimSym",l))
          pos = match(l,lab)
          indInFile[pos] = TRUE
        } }
    } }
  indNAPrices = (is.na(prices[1,]))
  indNALab = (is.na(lab[1:D]))
  indTooBig = (prices[1,] > 1e5) | (prices[len,] > 1e5)
  #missing price or lab is NA or too big
  indUnion = indInFile | indNAPrices | indNALab | indTooBig
  #Create new prices matrix smaller for only NonNAs
  smallerSz = D - sum(indUnion)
  print(smallerSz)
  newPrices = matrix(rep(0,len*smallerSz),nrow=len,ncol=smallerSz)
  newLab = vector(length=smallerSz)
  e <- 1
  for(d in 1:D) {
    if(!indUnion[d]) {
      #print(paste("e",e,lab))
      newPrices[,e] <- prices[,d]
      newLab[e] <- lab[d]
      e <- e + 1
    } else {print(d)}
  }
  list(newPrices[,1:smallerSz],newLab)
}

library(quadprog)
library(tseries)

P <- 2*diag(c(1,2,4))
d <- c(1, 1, -5)
At <- matrix(0, nrow=3, ncol=3)
At[1,] <- c(-1, 0, -1)
At[2,] <- c(1, 0, 0)
At[3,] <- c(0, -1, 0) 
b0 <- c(-1, 5, 0)

xHat <- solve.QP(P, d, t(At), b0)$solution
xHat

library(huge)
data(stockdata)
len = length(stockdata$data[,1])
D = dim(stockdata$data)[2]
prices = stockdata$data[,1:D]
lab = stockdata$info[1:D,1]

isSplitAdjusted <- FALSE
daysPerYr <- 252
mufree=0
R <- findR(prices, isSplitAdjusted=isSplitAdjusted)
displayCharts(prices, lab, nrow=6, ncol=4, sleepSecs=5)
res <- findCovMat(R)
meanv <- res[[1]]
cov_mat <- res[[2]]
diag_cov_mat <- res[[3]]
sdevv <- res[[4]]

round(cov_mat[1:8, 1:8],4)
Sharpe <- (meanv-mufree)/sdevv
isSplitAdjusted <- TRUE
isPlot <- TRUE
isShorting <- FALSE
Amat <- cbind(rep(1,D), meanv, diag(1, nrow=D))
Amat[1:8,1:10]


res <- elimSyms(prices, lab, dir, isSubDir=FALSE)
prices <- res[[1]]
lab <- res[[2]]
R <- findR(prices)
w <- opt(lab, meanv, cov_mat, isShorting)



