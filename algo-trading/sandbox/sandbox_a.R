sandbox.loadPackages <- function() {
  require(scales)
  require(AER)
  require(tidyverse)
  require(broom)
  require(quantmod)
  require(tseries)
  require(quantmod)
  require(BatchGetSymbols)
  require(rvest) 
  require(datetime)
  require(lubridate)
  require(expss)
  require(dplyr)
  require(knitr)
  require(bizdays)
  require(fpp2)
  require(zoo)
  require(tidyr) #for crossing
  require(httr)
  require(jsonlite)
  require(rameritrade)
  require(matrixStats)
  require(readxl)
  require(sys)
  require(splitstackshape)
  require(googledrive)
  library(randNames)
  library(udpipe)
  require(RMySQL)
  require(DBI)
  require(xfun)
  '%!in%' <- function(x,y)!('%in%'(x,y))
}

sandbox.loadPackages()

getQuote('TRVN')

?portfolio.optim()

input.sheet <- read_xlsx("./algo-trading/sample_settings_webpage.xlsx") %>%
  filter(!is.na(Prompt)) %>%
  select(Prompt,Input)

stocks_in <- str_trim(unlist(strsplit(input.sheet$Input[4],",")))

getQuote(c(stocks_in))$Last


tables[1]