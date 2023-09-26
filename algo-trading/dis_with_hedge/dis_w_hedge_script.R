source("./dis_w_hedge_functions.R")
#dis.finviz.insider.scraper("https://finviz.com/insidertrading.ashx")
dis.general.loadPackages()
screener <- dis.finviz.screener.scraper("","Overview",0,17)

setwd("~/algo-trading/dis_with_hedge/")
source("./dis_w_hedge_functions.R")
#dis.finviz.insider.scraper("https://finviz.com/insidertrading.ashx")
dis.general.loadPackages()
#upload to db screener
mydb <- dis.database.connect("local","dis_db")

# source("./dis_w_hedge_functions.R")
# dis.database.uploadScreener(mydb,screener)

#screener <- dbGetQuery(mydb,"SELECT * FROM DIS_Screener;")
#have update formula HERE

#simulate webpage inputs
user_id_in <- 1
inputs_id_in <- 1

#get from database
sectors <- c(unique(dbGetQuery(mydb,"SELECT Sector FROM DIS_Screener;")))
desired_sector <- dbGetQuery(mydb,
                             paste0(
                               "SELECT desired_sector_a FROM DIS_Inputs WHERE user_id=",
                               user_id_in," AND inputs_id=",inputs_id_in,";"))[1,1]
industries <- unique(c(dbGetQuery(mydb,
                        paste0(
                          "SELECT industry FROM DIS_Screener WHERE sector='",
                          desired_sector,"';"))$industry))
desired_industry <- dbGetQuery(mydb,
                             paste0(
                               "SELECT desired_industry_a FROM DIS_Inputs WHERE user_id=",
                               user_id_in," AND inputs_id=",inputs_id_in,";"))[1,1]

if(desired_industry == "All") {
  tickers <- unique(c(dbGetQuery(mydb,
                                    paste0(
                                      "SELECT ticker FROM DIS_Screener WHERE sector='",
                                      desired_sector,"';"))$ticker))
} else {
  tickers <- unique(c(dbGetQuery(mydb,
                                 paste0(
                                   "SELECT ticker FROM DIS_Screener WHERE sector='",
                                   desired_sector,"' AND industry='",desired_industry,"';"))$ticker))
}

#Get historical data for DIS
hd <- dis.historical.data(tickers,Sys.Date()-365,Sys.Date(),0)
prices <- hd$Historical
lookback <- 180
temp <- prices %>%
  select(Ticker,RefDate,RetClose)
hPrices <- dis.historical.turnHorizontal(temp) %>%
  filter(RefDate >= Sys.Date()-lookback)
hPrices.clean <- dis.historical.cleanMissing(hPrices)
temp <- dis.historical.restack(hPrices.clean,"RetClose")

summary <- temp %>%
  filter(RefDate >= Sys.Date()-lookback) %>%
  group_by(Ticker) %>%
  summarize(ReturnS = sum(RetClose),
            ReturnM = mean(RetClose),
            StdDevR = sd(RetClose)) %>%
  mutate(mean_ratio = ReturnM / StdDevR) %>%
  arrange(desc(mean_ratio))
numstks <- dbGetQuery(mydb,
                      paste0(
                        "SELECT num_of_stocks FROM DIS_Inputs WHERE user_id=",
                        user_id_in," AND inputs_id=",inputs_id_in,";"))[1,1]
                                          
picks <- summary[1:numstks,]

#set algo to read in other sector which will eventually be all stocks
other.tickers <- c(unique(dbGetQuery(mydb,"SELECT ticker FROM DIS_Screener WHERE industry='Biotechnology';")$ticker))
other <- dis.historical.data(other.tickers,Sys.Date()-365,Sys.Date(),0)
other.prices <- other$Historical
other.lookback <- 180
other.temp <- other.prices %>%
  select(Ticker,RefDate,RetClose)
other.hPrices <- dis.historical.turnHorizontal(other.temp) %>%
  filter(RefDate >= Sys.Date()-other.lookback)
other.hPrices.clean <- dis.historical.cleanMissing(other.hPrices)
other <- dis.historical.restack(other.hPrices.clean,"RetClose")

other.summary <- temp %>%
  filter(RefDate >= Sys.Date()-other.lookback) %>%
  group_by(Ticker) %>%
  summarize(ReturnS = sum(RetClose),
            ReturnM = mean(RetClose),
            StdDevR = sd(RetClose)) %>%
  mutate(mean_ratio = ReturnM / StdDevR) %>%
  arrange(desc(mean_ratio))



#make all combos
temp.corr.data <- temp %>%
  filter(Ticker %in% c(picks$Ticker)) %>%
  filter(RefDate >= Sys.Date()-other.lookback)
other.corr.data <- other %>%
  filter(RefDate >= Sys.Date()-other.lookback)

a <- c(unique(temp.corr.data$Ticker))
b <- c(unique(other.corr.data$Ticker))
corr.combos <- expand.grid(a,b) %>%
  `colnames<-`(c("StkA","StkB")) %>%
  rowwise() %>%
  mutate(Correlation = cor(temp.corr.data %>%
                             filter(Ticker == StkA) %>%
                             select(RetClose),
                           other.corr.data %>%
                             filter(Ticker == StkB) %>%
                             select(RetClose))[[1]]) %>%
  arrange(StkA,Correlation)


#get return lookback

#find correlation

#sort, rank 

#use risk choice to set weights

#create nominal dollar values

#simulate performance over time using backdating



