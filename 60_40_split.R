library(Quandl)                 # Quandl package
Quandl.auth('twVgpzZLpwNoDa4ywqRA')
AAA <- Quandl("ML/AAATRI",start_date="1990-01-01",end_date="2012-12-31",
              collapse='monthly')
TR <- Quandl("SANDP/MONRETS",start_date="1990-01-01",end_date="2012-12-31" )
class(AAA)
# [1] "data.frame"
class(AAA[,2])
# [1] "numeric"
class(AAA[,1])
# [1] "Date"
Time <- rev(TR[,1]) 
TR <- apply(TR[-1],2,rev) 
# We now pretend as if we invested 1 dollar in 1990.
bond <- rev(AAA[,2])/tail(AAA[,2],1) 
stock <- NULL
for (i in 1:NROW(TR)){
  stock[i] <- cumsum(prod((1+TR[1:i,11])))
}
# Graphical Parameters:
stockframe = data.frame(value=stock,Date=Time)
bondframe = data.frame(value=bond,Date=Time)
line.plot <- ggplot() +
  geom_line(data=stockframe, aes(x=Date, y=value, colour="Stocks")) +
  geom_line(data=bondframe, aes(x=Date, y=value, colour="Bonds")) +
  scale_colour_manual("", breaks = c("Stocks", "Bonds"), values = c("#29779f","#d8593b")) +
  theme(panel.background = element_rect(fill='#FFFFFF'), panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour='#3a4047', size=0.1), panel.grid.minor = element_line(colour='#3a4047', size=0.1)) +
  xlab("Date") + ylab("Returns") + ggtitle("Stocks and Bonds")
line.plot
# We convert to time series so later we can compute yearly returns.
AAAA <- ts(rev(AAA[,2]),start=c(1990,1),end=c(2012,1),frequency=12)
stockk <- xts(stock,order.by=Time)
yretb <- yearlyReturn(AAAA)
yrets <- yearlyReturn(stockk)
namarg <- substr(index(yrets),1,4)
df = data.frame(Date=namarg, value=as.numeric(yrets)*100)
df2 = data.frame(Date=namarg, value=as.numeric(yretb)*100)
bar.plot <- ggplot() +
  geom_bar(stat='identity', data=df, aes(x=Date, y=value, fill="Stocks")) +
  geom_bar(stat='identity', data=df2, aes(x=Date, y=value, fill="Bonds")) + 
  scale_fill_manual("", breaks = c("Stocks", "Bonds"), values = c("#29779f","#d8593b")) +
  coord_flip() +
  theme(panel.background = element_rect(fill='#FFFFFF'), panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour='#3a4047', size=0.1), panel.grid.minor = element_line(colour='#3a4047', size=0.1)) +
  xlab("Date") + ylab("%") + ggtitle("Yearly Returns")
bar.plot
TT <- length(yrets)
bondweight <- c(.2,.4,.6,.8)
l=length(bondweight)
stockweight <- 1-bondweight
Sharp <- m <- v <- NULL
st = 1 # 19 = 2008
for (j in 1:l){
  PortRet <- as.numeric(bondweight[j]*yretb[st:TT]) + as.numeric(stockweight[j]*yrets[st:TT])
  # you can play around with removing bad equity years.
  #PortRet <- as.numeric(bondweight[j]*yretb[-st]) + as.numeric(stockweight[j]*yrets[-st])
  m[j] <- mean(PortRet)
  v[j] <-sd(PortRet)
  Sharp[j] <- m[j]/v[j]
}
Sharp
# 0.666 0.804 1.014 1.260