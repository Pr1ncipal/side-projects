#option chain
setwd("~/algo-trading/oss/")
source("./oss_functions.R")
#packages
oss.general.loadPackages()

#initialize position framepos
pos <- as.data.frame(matrix(NA,2,6)) %>%
  `colnames<-`(c("Strategy","Struct1","ToPrice1","Struct2","ToPrice2","ToEachother"))
pos[1,] <- c("Long Strangle","Call",">","Put","<","Not Equal")
pos[2,] <- c("Long Straddle","Call","=","Put","=","Equal")
#long strangle
  #long call with strike > current
  #long put with strike < current
  #same underlying stock and expiration

stock <- 'AAPL'

for(i in seq(length(stock))) {
  
}
s <- 1
chain <- getOptionChain(stock,Exp = "2022-04-08")
p0 <- getQuote(stock)$Last
structOneList <- if(pos$Strategy[s]=="Long Strangle") {
  struct.one <- as.data.frame(
    chain$calls %>%
    filter(Strike > p0))
}
structTwoList <- if(pos$Strategy[s]=="Long Strangle") {
  struct.two <- as.data.frame(
    chain$puts %>%
      filter(Strike < p0))
}
structOneList
structTwoList
