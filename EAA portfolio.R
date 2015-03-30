require(quantmod)
require(PerformanceAnalytics)

symbols <- c("VTSMX", "FDIVX", "VEIEX", "VBMFX", "VFISX", "VGSIX", "QRAAX")

getSymbols(symbols, from="1990-01-01")
prices <- list()
for(i in 1:length(symbols)) {
  prices[[i]] <- Ad(get(symbols[i]))  
}
prices <- do.call(cbind, prices)
colnames(prices) <- gsub("\\.[A-z]*", "", colnames(prices))
ep <- endpoints(prices, "months")
prices <- prices[ep,]
prices <- prices["1997-03::"]

EAA <- function(monthlyPrices, wR=1, wV=0, wC=.5, wS=2, errorJitter=1e-6, 
                cashAsset=NULL, bestN=1+ceiling(sqrt(ncol(monthlyPrices))),
                enableCrashProtection = TRUE, returnWeights=FALSE) {
  returns <- Return.calculate(monthlyPrices)
  returns <- returns[-1,] #return calculation uses one observation
  
  if(is.null(cashAsset)) {
    returns$zeroes <- 0
    cashAsset <- "zeroes"
    warning("No cash security specified. Recommended to use one of: quandClean('CHRIS/CME_US'), SHY, or VFISX. 
            Using vector of zeroes instead.")
  }
  
  cashCol <- grep(cashAsset, colnames(returns))
  
  weights <- list()
  for(i in 1:(nrow(returns)-11)) {
    returnsData <- returns[i:(i+11),] #each chunk will be 12 months of returns data
    #per-month mean of cumulative returns of 1, 3, 6, and 12 month periods
    periodReturn <- ((returnsData[12,] + Return.cumulative(returnsData[10:12,]) + 
      Return.cumulative(returnsData[7:12,]) + Return.cumulative(returnsData)))/22
    vols <- StdDev.annualized(returnsData) 
    mktIndex <- xts(rowMeans(returnsData), order.by=index(returnsData)) #equal weight returns of universe
    cors <- cor(returnsData, mktIndex) #correlations to market index
    
    weightedRets <- periodReturn ^ wR
    weightedCors <- (1 - as.numeric(cors)) ^ wC
    weightedVols <- (vols + errorJitter) ^ wV
    wS <- wS + errorJitter
    
    z <- (weightedRets * weightedCors / weightedVols) ^ wS #compute z_i and zero out negative returns
    z[periodReturn < 0] <- 0
    crashProtection <- sum(z==0)/length(z) #compute crash protection cash cushion
    
    orderedZ <- sort(as.numeric(z), decreasing=TRUE)
    selectedSecurities <- z >= orderedZ[bestN]
    preNormalizedWeights <- z*selectedSecurities #select top N securities, keeping z_i scores
    periodWeights <- preNormalizedWeights/sum(preNormalizedWeights) #normalize
    if (enableCrashProtection) {
      periodWeights <- periodWeights * (1-crashProtection) #CP rule
    }
    weights[[i]] <- periodWeights
  }
  
  weights <- do.call(rbind, weights)
  weights[, cashCol] <- weights[, cashCol] + 1-rowSums(weights) #add to risk-free asset all non-invested weight
  strategyReturns <- Return.rebalancing(R = returns, weights = weights) #compute strategy returns
  if(returnWeights) {
    return(list(weights, strategyReturns))
  } else {
    return(strategyReturns)
  }
}

offensive <- EAA(prices, cashAsset="VBMFX", bestN=3)
defensive <- EAA(prices, cashAsset="VBMFX", bestN=3, wS=.5, wC=1)
compare <- cbind(offensive, defensive)
colnames(compare) <- c("Offensive", "Defensive")
stats <- rbind(Return.annualized(compare)*100, StdDev.annualized(compare)*100, maxDrawdown(compare)*100, SharpeRatio.annualized(compare))
rownames(stats)[3] <- "Worst Drawdown"
charts.PerformanceSummary(compare)