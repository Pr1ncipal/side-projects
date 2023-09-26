dis.general.loadPackages <- function() {
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

dis.finviz.insider.scraper <- function(link) {
  #libraries. Install if necessary
  require(rvest)
  require(dplyr)
  
  #link
  url <- link
  page <- read_html(url)
  
  #code that pulls the numbers. put in brackets to make cleaner
  {
    #First line creates the variable, second line finds the ticker,
    #third line turns it into the text we actually need.
    columns <- page %>% 
      html_nodes(".table-top") %>%
      html_text()
    #View(columns)
    
    ticker <- page %>% 
      html_nodes(".cursor-pointer td:nth-child(1)") %>%
      html_text()
    #View(ticker)
    
    owner <- page %>% 
      html_nodes(".cursor-pointer td:nth-child(2)") %>%
      html_text()
    #View(owner)
    
    relationship <- page %>% 
      html_nodes(".cursor-pointer td:nth-child(3)") %>%
      html_text()
    #View(relationship)
    
    transaction_date <- page %>% 
      html_nodes(".cursor-pointer td:nth-child(4)") %>%
      html_text()
    #View(transaction_date)
    
    transaction <- page %>% 
      html_nodes(".cursor-pointer td:nth-child(5)") %>%
      html_text()
    #View(transaction)
    
    share_price <- page %>% 
      html_nodes(".cursor-pointer td:nth-child(6)") %>%
      html_text()
    #View(share_price)
    
    num_shares <- page %>% 
      html_nodes(".cursor-pointer td:nth-child(7)") %>%
      html_text()
    #View(num_shares)
    
    dollar_value <- page %>% 
      html_nodes(".cursor-pointer td:nth-child(8)") %>%
      html_text()
    #View(dollar_value)
    
    total_shares <- page %>% 
      html_nodes(".cursor-pointer td:nth-child(9)") %>%
      html_text()
    #View(total_shares)
    
    filing_date <- page %>% 
      html_nodes(".cursor-pointer td:nth-child(10)") %>%
      html_text()
    #View(filing_date)
    
    FinViz <- data.frame(
      ticker = ticker,
      owner = owner,
      relationship = relationship,
      transaction_date = transaction_date,
      transaction = transaction,
      share_price = share_price,
      num_shares = num_shares,
      dollar_value = dollar_value,
      total_shares = total_shares,
      filing_date = filing_date
    )
  }
  return(FinViz)
}

dis.finviz.screener.scraper <- function(filters,header,page,node) {
  #stop as a bool variable to stop loop
  stop <- FALSE
  #set iterator i to 1 to be used in read-in while loop
  i <- 1
  #set table hCodes as text-to-number key for proper numerical codes for link 
  # construction
  hCodes <- tribble(
    ~Header,~Code,
    "Overview",111,
    "Valuation",121,
    "Financial",161,
    "Ownership",131,
    "Performance",141,
    "Technical",171
  )
  #assign hCode as a numerical code used in link construction
  hCode <- hCodes$Code[grep(header,hCodes$Header)]
  #assign pCode as a numerical page counter in loop based on page argument
  # such that page==0 means all pages, otherwise the amount of pages to be 
  # read in
  pCode <- case_when(page==0 ~ 600, page!=0 ~ page)
  #while loop such that stop variable is FALSE and the iterator i is less than or
  # equal to pCode
  while(stop == FALSE & i <= pCode) {
    #assign url as html object such that a link is built and used as argument in a 
    # call to read_html()
    url <- read_html(
      paste("https://finviz.com/screener.ashx?",
            "v=",hCode,filters,"&r=",(((i-1)*20)+1),
            sep=""))
    #print status
    print(paste(header," | ",(((i-1)*20)+1)))
    #assign tables as html nodes
    tables <- html_nodes(url,"table")
    #assign screen as dataframe with proper node (11) selected from tables
    screen <- tables %>% html_nodes("table") %>% .[node] %>% 
      html_table(fill=TRUE) %>% data.frame()
    #set columnnames of screen as the first row from screen, as this is how data 
    # was brought in
    colnames(screen) <- screen[1,]
    #remove first row of screen as they are column names and real column names 
    # have been set
    screen <- screen[-1,]
    #clear rownames of screen
    rownames(screen) <- c()
    #if in first iteration
    if(i == 1) {
      #assign cScreener as screen
      cScreener <- screen
    } 
    if(nrow(screen)==20 & i != 1) {
      #if iteration is greater than one and screen is full, thus 20 rows long, 
      # combine screen onto existing cScreener and assign to cScreener
      cScreener <- rbind(cScreener,screen)
    }
    if(nrow(screen)!=20 & i != 1) {
      #if iteration is greater than one and screen is not full, thus not 20 rows 
      # long, combine screen onto existing cScreener and assign to cScreener, then 
      # assign value of TRUE to stop variable
      cScreener <- rbind(cScreener,screen)
      stop <- TRUE
    }
    #upgrade iterator i by 1
    i <- i+1
    Sys.sleep(0.25)
  }
  #if page wanted is Overview
  if(hCode == 111) {
    cScreener <- cScreener %>%
      select(Ticker:Volume) %>%
      `colnames<-`(
        c("Ticker","Company","Sector","Industry","Country",
          "MktCap","PE","Price","Change","Volume")) %>%
      transform(MktCap = case_when(
        MktCap == "-" ~ 0,
        grepl("B", MktCap, fixed=TRUE) ~ as.numeric(gsub("B",'',MktCap))*1000000000,
        grepl("M", MktCap, fixed=TRUE) ~ as.numeric(gsub("M",'',MktCap))*1000000,
        grepl("K", MktCap, fixed=TRUE) ~ as.numeric(gsub("K",'',MktCap))*1000),
        PE = case_when(PE == "-" ~ 0, PE != "-" ~ as.numeric(PE)),
        Price = as.numeric(Price),
        Change = as.numeric(gsub("%",'',Change)),
        Volume = as.numeric(gsub(",",'',Volume))
      ) %>%
      unique() %>%
      mutate(DateUpdated = Sys.Date())
  }
  #if page wanted is Valuation
  if(hCode == 121) {
    cScreener <- cScreener %>%
      select(Ticker:Volume) %>%
      `colnames<-`(
        c("Ticker","MktCap","PE","FwdPE","PEG","PS","PB","PC","PFCF","EPSthisY",
          "EPSnextY","EPSpast5Y","EPSnext5Y","SalesPast5Y","Price",
          "Change","Volume")) %>%
      transform(MktCap = case_when(
        MktCap == "-" ~ 0,
        grepl("B", MktCap, fixed=TRUE) ~ as.numeric(gsub("B",'',MktCap))*1000000000,
        grepl("M", MktCap, fixed=TRUE) ~ as.numeric(gsub("M",'',MktCap))*1000000,
        grepl("K", MktCap, fixed=TRUE) ~ as.numeric(gsub("K",'',MktCap))*1000),
        PE = case_when(PE == "-" ~ 0, PE != "-" ~ as.numeric(PE)),
        FwdPE = case_when(FwdPE == "-" ~ 0, FwdPE != "-" ~ as.numeric(FwdPE)),
        PEG = case_when(PEG == "-" ~ 0, PEG != "-" ~ as.numeric(PEG)),
        PS = case_when(PS == "-" ~ 0, PS != "-" ~ as.numeric(PS)),
        PB = case_when(PB == "-" ~ 0, PB != "-" ~ as.numeric(PB)),
        PC = case_when(PC == "-" ~ 0, PC != "-" ~ as.numeric(PC)),
        PFCF = case_when(PFCF == "-" ~ 0, PFCF != "-" ~ as.numeric(PFCF)),
        EPSthisY = case_when(EPSthisY == "-" ~ 0, 
                             EPSthisY != "-" ~ as.numeric(
                               gsub("%",'',EPSthisY))),
        EPSnextY = case_when(EPSnextY == "-" ~ 0, 
                             EPSnextY != "-" ~ as.numeric(
                               gsub("%",'',EPSnextY))),
        EPSpast5Y = case_when(EPSpast5Y == "-" ~ 0, 
                              EPSpast5Y != "-" ~ as.numeric(
                                gsub("%",'',EPSpast5Y))),
        EPSnext5Y = case_when(EPSnext5Y == "-" ~ 0, 
                              EPSnext5Y != "-" ~ as.numeric(
                                gsub("%",'',EPSnext5Y))),
        SalesPast5Y = case_when(SalesPast5Y == "-" ~ 0, 
                                SalesPast5Y != "-" ~ as.numeric(
                                  gsub("%",'',SalesPast5Y))),
        Price = as.numeric(Price),
        Change = as.numeric(gsub("%",'',Change)),
        Volume = as.numeric(gsub(",",'',Volume))
      ) %>%
      unique() %>%
      mutate(DateUpdated = Sys.Date())
  }
  #if page wanted is Financial
  if(hCode == 161) {
    cScreener <- cScreener %>%
      select(Ticker:Volume) %>%
      `colnames<-`(
        c("Ticker","MktCap","Dividend","ROA","ROE","ROI","CurrentRatio",
          "QuickRatio","LTDebtEq","DebtEq","GrossMarg","OperMarg",
          "ProfitMarg","Earnings","Price","Change","Volume")) %>%
      transform(MktCap = case_when(
        MktCap == "-" ~ 0,
        grepl("B", MktCap, fixed=TRUE) ~ as.numeric(gsub("B",'',MktCap))*1000000000,
        grepl("M", MktCap, fixed=TRUE) ~ as.numeric(gsub("M",'',MktCap))*1000000,
        grepl("K", MktCap, fixed=TRUE) ~ as.numeric(gsub("K",'',MktCap))*1000),
        Dividend = case_when(Dividend == "-" ~ 0, 
                             Dividend != "-" ~ as.numeric(
                               gsub("%",'',Dividend))),
        ROA = case_when(ROA == "-" ~ 0, 
                        ROA != "-" ~ as.numeric(
                          gsub("%",'',ROA))),
        ROE = case_when(ROE == "-" ~ 0, 
                        ROE != "-" ~ as.numeric(
                          gsub("%",'',ROE))),
        ROI = case_when(ROI == "-" ~ 0, 
                        ROI != "-" ~ as.numeric(
                          gsub("%",'',ROI))),
        CurrentRatio = case_when(CurrentRatio == "-" ~ 0, 
                                 CurrentRatio != "-" ~ as.numeric(
                                   CurrentRatio)),
        QuickRatio = case_when(QuickRatio == "-" ~ 0, 
                               QuickRatio != "-" ~ as.numeric(QuickRatio)),
        LTDebtEq = case_when(LTDebtEq == "-" ~ 0, 
                             LTDebtEq != "-" ~ as.numeric(LTDebtEq)),
        DebtEq = case_when(DebtEq == "-" ~ 0, 
                           DebtEq != "-" ~ as.numeric(DebtEq)),
        GrossMarg = case_when(GrossMarg == "-" ~ 0, 
                              GrossMarg != "-" ~ as.numeric(
                                gsub("%",'',GrossMarg))),
        OperMarg = case_when(OperMarg == "-" ~ 0, 
                             OperMarg != "-" ~ as.numeric(
                               gsub("%",'',OperMarg))),
        ProfitMarg = case_when(ProfitMarg == "-" ~ 0, 
                               ProfitMarg != "-" ~ as.numeric(
                                 gsub("%",'',ProfitMarg))),
        Price = as.numeric(Price),
        Change = as.numeric(gsub("%",'',Change)),
        Volume = as.numeric(gsub(",",'',Volume))
      ) %>%
      unique() %>%
      mutate(DateUpdated = Sys.Date())
  }
  #if page wanted is Ownership
  if(hCode == 131) {
    cScreener <- cScreener %>%
      select(Ticker:Volume) %>%
      `colnames<-`(
        c("Ticker","MktCap","Outstanding","Float","InsiderOwn","InsiderTrans",
          "InstOwn","InstTrans","FloatShort","ShortRatio","AvgVolume","Price",
          "Change","Volume")) %>%
      transform(MktCap = case_when(
        MktCap == "-" ~ 0,
        grepl("B", MktCap, fixed=TRUE) ~ as.numeric(gsub("B",'',MktCap))*1000000000,
        grepl("M", MktCap, fixed=TRUE) ~ as.numeric(gsub("M",'',MktCap))*1000000,
        grepl("K", MktCap, fixed=TRUE) ~ as.numeric(gsub("K",'',MktCap))*1000),
        Outstanding = case_when(Outstanding == "-" ~ 0,
                                grepl("B", Outstanding, fixed=TRUE) ~ 
                                  as.numeric(
                                    gsub("B",'',Outstanding))*1000000000,
                                grepl("M", Outstanding, fixed=TRUE) ~ 
                                  as.numeric(
                                    gsub("M",'',Outstanding))*1000000,
                                grepl("K", Outstanding, fixed=TRUE) ~ 
                                  as.numeric(
                                    gsub("K",'',Outstanding))*1000),
        Float = case_when(
          Float == "-" ~ 0,
          grepl("B", Float, fixed=TRUE) ~ as.numeric(
            gsub("B",'',Float))*1000000000,
          grepl("M", Float, fixed=TRUE) ~ as.numeric(
            gsub("M",'',Float))*1000000,
          grepl("K", Float, fixed=TRUE) ~ as.numeric(
            gsub("K",'',Float))*1000),
        InsiderOwn = case_when(
          InsiderOwn == "-" ~ 0, 
          InsiderOwn != "-" ~ as.numeric(gsub("%",'',InsiderOwn))),
        InsiderTrans = case_when(
          InsiderTrans == "-" ~ 0, 
          InsiderTrans != "-" ~ as.numeric(gsub("%",'',InsiderTrans))),
        InstOwn = case_when(
          InstOwn == "-" ~ 0, 
          InstOwn != "-" ~ as.numeric(gsub("%",'',InstOwn))),
        InstTrans = case_when(
          InstTrans == "-" ~ 0, 
          InstTrans != "-" ~ as.numeric(gsub("%",'',InstTrans))),
        FloatShort = case_when(
          FloatShort == "-" ~ 0, 
          FloatShort != "-" ~ as.numeric(gsub("%",'',FloatShort))),
        ShortRatio = case_when(
          ShortRatio == "-" ~ 0, 
          ShortRatio != "-" ~ as.numeric(ShortRatio)),
        AvgVolume = case_when(
          AvgVolume == "-" ~ 0,
          grepl("B", AvgVolume, fixed=TRUE) ~ as.numeric(
            gsub("B",'',AvgVolume))*1000000000,
          grepl("M", AvgVolume, fixed=TRUE) ~ as.numeric(
            gsub("M",'',AvgVolume))*1000000,
          grepl("K", AvgVolume, fixed=TRUE) ~ as.numeric(
            gsub("K",'',AvgVolume))*1000),
        Price = as.numeric(Price),
        Change = as.numeric(gsub("%",'',Change)),
        Volume = as.numeric(gsub(",",'',Volume))
      ) %>%
      unique() %>%
      mutate(DateUpdated = Sys.Date())
  }
  #if page wanted is Performance
  if(hCode == 141) {
    cScreener <- cScreener %>%
      select(Ticker:Volume) %>%
      `colnames<-`(
        c("Ticker","PerformW","PerformM","PerformQ","PerformH","PerformY",
          "PerformYTD","VolatilityW","VolatilityM","Recom","AvgVolume",
          "RelVolume","Price","Change","Volume")) %>%
      transform(PerformW = case_when(
        PerformW == "-" ~ 0, 
        PerformW != "-" ~ as.numeric(
          gsub("%",'',PerformW))),
        PerformM = case_when(
          PerformM == "-" ~ 0, 
          PerformM != "-" ~ as.numeric(
            gsub("%",'',PerformM))),
        PerformQ = case_when(
          PerformQ == "-" ~ 0, 
          PerformQ != "-" ~ as.numeric(
            gsub("%",'',PerformQ))),
        PerformH = case_when(
          PerformH == "-" ~ 0,
          PerformH != "-" ~ as.numeric(
            gsub("%",'',PerformH))),
        PerformY = case_when(
          PerformY == "-" ~ 0, 
          PerformY != "-" ~ as.numeric(
            gsub("%",'',PerformY))),
        PerformYTD = case_when(
          PerformYTD == "-" ~ 0, 
          PerformYTD != "-" ~ as.numeric(
            gsub("%",'',PerformYTD))),
        VolatilityW = case_when(
          VolatilityW == "-" ~ 0, 
          VolatilityW != "-" ~ as.numeric(
            gsub("%",'',VolatilityW))),
        VolatilityM = case_when(
          VolatilityM == "-" ~ 0, 
          VolatilityM != "-" ~ as.numeric(
            gsub("%",'',VolatilityM))),
        Recom = case_when(
          Recom == "-" ~ 0, 
          Recom != "-" ~ as.numeric(Recom)),
        AvgVolume = case_when(
          AvgVolume == "-" ~ 0,
          grepl("B", AvgVolume, fixed=TRUE) ~ as.numeric(
            gsub("B",'',AvgVolume))*1000000000,
          grepl("M", AvgVolume, fixed=TRUE) ~ as.numeric(
            gsub("M",'',AvgVolume))*1000000,
          grepl("K", AvgVolume, fixed=TRUE) ~ as.numeric(
            gsub("K",'',AvgVolume))*1000),
        RelVolume = case_when(
          RelVolume == "-" ~ 0,
          RelVolume != "-" ~ as.numeric(RelVolume)),
        Price = as.numeric(Price),
        Change = as.numeric(gsub("%",'',Change)),
        Volume = as.numeric(gsub(",",'',Volume))
      ) %>%
      unique() %>%
      mutate(DateUpdated = Sys.Date())
  }
  #if page wanted is Technical
  if(hCode == 171) {
    cScreener <- cScreener %>%
      select(Ticker:Volume) %>%
      `colnames<-`(
        c("Ticker","Beta","ATR","SMA20","SMA50","SMA200","High52W",
          "Low52W","RSI","Price","Change","fromOpen","Gap","Volume")) %>%
      transform(Beta = case_when(
        Beta == "-" ~ 0, 
        Beta != "-" ~ as.numeric(Beta)),
        ATR = case_when(
          ATR == "-" ~ 0, 
          ATR != "-" ~ as.numeric(ATR)),
        SMA20 = case_when(
          SMA20 == "-" ~ 0, 
          SMA20 != "-" ~ as.numeric(
            gsub("%",'',SMA20))),
        SMA50 = case_when(
          SMA50 == "-" ~ 0, 
          SMA50 != "-" ~ as.numeric(
            gsub("%",'',SMA50))),
        SMA200 = case_when(
          SMA200 == "-" ~ 0, 
          SMA200 != "-" ~ as.numeric(
            gsub("%",'',SMA200))),
        High52W = case_when(
          High52W == "-" ~ 0, 
          High52W != "-" ~ as.numeric(
            gsub("%",'',High52W))),
        Low52W = case_when(
          Low52W == "-" ~ 0, 
          Low52W != "-" ~ as.numeric(
            gsub("%",'',Low52W))),
        RSI = case_when(
          RSI == "-" ~ 0, 
          RSI != "-" ~ as.numeric(RSI)),
        Price = as.numeric(Price),
        Change = as.numeric(gsub("%",'',Change)),
        fromOpen = case_when(
          fromOpen == "-" ~ 0, 
          fromOpen != "-" ~ as.numeric(
            gsub("%",'',fromOpen))),
        Gap = case_when(
          Gap == "-" ~ 0, 
          Gap != "-" ~ as.numeric(
            gsub("%",'',Gap))),
        Volume = as.numeric(gsub(",",'',Volume))
      ) %>%
      unique() %>%
      mutate(DateUpdated = Sys.Date())
  }
  #return cScreener
  return(cScreener)
}

dis.historical.data <- function(tickers,startDate,endDate,tbd) {
  #require BatchGetSymbols for historical data import
  require(BatchGetSymbols)
  #call BatchGetSymbols with arguments, returns list of $df.control and $df.tickers
  rawData <- BatchGetSymbols(tickers,first.date = startDate, last.date = endDate, 
                             thresh.bad.data = tbd)
  #reassign rawData$df.tickers such that column names are clean, uniformed, & 
  # reordered, and missing return observations are filtered out
  rawData$df.tickers <- rawData$df.tickers %>%
    `colnames<-`(c("Open","High","Low","Close","Volume","Adjusted","RefDate",
                   "Ticker","RetAdj","RetClose")) %>%
    select(RefDate,Ticker,Open,High,Low,Close,RetClose,Adjusted,RetAdj,Volume) %>%
    filter(is.na(RetClose) == FALSE)
  #reassign rawData$df.control such that column names are clean and uniform
  rawData$df.control <- rawData$df.control %>%
    `colnames<-`(c("Ticker","SRC","Status","Obs","Perc","Decision"))
  #call TercVentures.gen.removeDuplicateDates to remove any duplicate date 
  # observations, returns list of $Control and $Historical
  rawData <- dis.historical.scrubDates(rawData)
  #return rawData as list of $Control and $Historical
  return(rawData)
}

dis.historical.scrubDates <- function(rawData) {
  #assign tLength as numerical length such that it is the amount of tickers in 
  # the dataset
  tLength <- length(c(rawData$df.control$Ticker))
  #for loop between 1 and the amount of tickers in the dataset
  for(i in seq(rawData$df.control$Ticker)) {
    #print status and wipe console
    cat(paste("Searching for duplicates in ",rawData$df.control$Ticker[i],
              " : ",round(i/tLength*100,digits=2),"%",sep=""))
    Sys.sleep(0.005)
    cat("\014")
    #if duplicated dates are found within the current ticker element
    if(TRUE %in% c(
      duplicated(
        (
          rawData$df.tickers%>%filter(Ticker==rawData$df.control$Ticker[i])
        )$RefDate
      )
    )
    ) {
      #action to remove
      #assign findDate as date where the duplicated element is found in 
      # rawData$df.tickers
      findDate <- (rawData$df.tickers%>%filter(Ticker==rawData$df.control$Ticker[i]) 
                   %>% filter(duplicated(RefDate)==TRUE))$RefDate
      #assign indexes as a vector of the intersection between locations in 
      # rawData$df.tickers where the duplicated date exists and locations of the 
      #current ticker element, thus the currect duplicated date locations
      indexes <- c(intersect(grep(findDate,rawData$df.tickers$RefDate),
                             grep(rawData$df.control$Ticker[i],
                                  rawData$df.tickers$Ticker)))
      #assign newLine as single line of data which is the condensed multiple lines 
      # of duplicated dates
      #subset the currect rows of duplicated dates, group by Ticker, properly 
      #condense, reorder columns
      newLine <- rawData$df.tickers[indexes,] %>% 
        group_by(Ticker) %>% 
        summarize(RefDate = max(RefDate),
                  Open = min(Open),
                  High = max(High),
                  Low = min(Low),
                  Close = max(Close),
                  RetClose = max(RetClose),
                  Adjusted = max(Adjusted),
                  RetAdj = max(RetAdj),
                  Volume = max(Volume)
        ) %>%
        select(RefDate,Ticker,Open,High,Low,Close,RetClose,Adjusted,RetAdj,Volume)
      #reassign newLine to rawData$df.tickers at maximum row of duplicated rows
      rawData$df.tickers[indexes[length(indexes)],] <- newLine
      #reassign all indexes that are not the maximum row of duplicated rows 
      # (which was just used) to indexes, for removal in next step
      indexes <- c(indexes[!indexes %in% indexes[length(indexes)]])
      #remove rows from rawData$df.tickers defined by indexes and reassign to 
      # rawData$df.tickers
      rawData$df.tickers <- rawData$df.tickers[-indexes,]
      #adjust number of observations in rawData$df.control by subtracting indexes, 
      # thus the amount of deleted rows
      rawData$df.control$Obs[i] <- rawData$df.control$Obs[i]-length(indexes)
      #adjust the Perc column in rawData$df.control to be the proper percentage of 
      # each row's total obs, such that each rows number of observations is divided 
      # by the maximum instance of rows in the dataset
      rawData$df.control <- rawData$df.control %>%
        transform(Perc = Obs / max(Obs))
      #set rownames of rawData$df.tickers to NULL
      rownames(rawData$df.tickers) <- c()
    } else {
    }
  }
  #set names in rawData list
  names(rawData) <- c("Control","Historical")
  #return the list of $Control and $Historical as rawData
  return(rawData)
}

dis.historical.turnHorizontal <- function(df) {
  #df is a three column dataframe. Column 1 is grouping (column definition), 
  # Column 2 is the y axis (row defintion), Column 3 is the populating data
  #sort df by instances to find max as baseline, assign grouped as vector of 
  # characters such  that it is the grouping variable, typically tickers
  grouped <- c((df %>% group_by_at(1) 
                %>% summarise(n = n()) 
                %>% arrange(desc(n),Ticker))$Ticker)
  #assign gLength as numerical such that it is the length of grouped
  gLength <- length(grouped)
  #for loop between 1 and the length grouped, thus gLength
  for(i in seq(gLength)) {
    #print status   
    cat(paste("Turning ",grouped[i]," to Horizontal: ",
              round(i/gLength*100,digits=2),"%",sep=""))
    Sys.sleep(0.005)
    cat("\014")
    #if iteration is on first loop
    if(i == 1) {
      #assign to Frame1 such that column 1 of df is equal to the current grouping 
      # element and select columns 2 and 3, thus the x and y axis information, then 
      # rename columns
      Frame1 <- ((df %>%
                    filter(df[,1] == grouped[i]))[,c(2,3)]) %>%
        `colnames<-`(c("RefDate",gsub(" ",'',grouped[i])))
    } else {
      #iterator is past fist loop and is ready for new frames to be joined onto 
      # Frame1
      #assign to Frame2 such that column 1 of df is equal to the current grouping 
      # element
      # and subset columns 2 and 3, thus the x and y axis information, then rename 
      # columns
      Frame2 <- ((df %>%
                    filter(df[,1] == grouped[i]))[,c(2,3)]) %>%
        `colnames<-`(c("RefDate",grouped[i]))
      #assign to Frame1 such that Frame2 is left joined onto Frame1 by 'RefDate'
      Frame1 <- left_join(Frame1,Frame2,by=c("RefDate"="RefDate"))
    }
  }
  return(Frame1)
}

dis.historical.cleanMissing <- function(horzData) {
  #for loop between 1 and number of columns in horzData frame
  for(i in seq(ncol(horzData))) {
    #if on first iteration, thus i==1
    if(i == 1) {
      #initialize null vector of column indexes for removal
      indexes <- c()
      #if there are any null values in the current column element, append to
      # indexes vector for later removal
      if(any(is.na(horzData[,i]))) {
        indexes <- append(indexes,i)
      }
    }
    #if iterator is past first loop, only search for NA's. No initialization.
    if(i > 1) {
      #if there are any null values in the current column element, append to
      # indexes vector for later removal
      if(any(is.na(horzData[,i]))) {
        indexes <- append(indexes,i)
      }
    }
  }
  #remove the columns from horzData whose column indexes are in the 
  # vector 'indexes'
  if(length(indexes)>0) {
    horzData <- horzData[,-indexes]
  }
  #return the scrubbed horizontal dataframe
  return(horzData)
}

dis.historical.restack <- function(cleanHorz,specColumnName) {
  #for loop between 1 and number of columns in cleanHorz
  gLength <- ncol(cleanHorz)-1
  for(i in seq(ncol(cleanHorz))) {
    #if on first iteration, thus RefDate column
    if(i == 1) {
      #initialize blank table to be added to
      stacked <- tribble(
        ~RefDate,
        ~Value,
        ~Ticker
      )
    } else {
      #if not on first iteration
      #get current stock ticker for Ticker column
      curStk <- colnames(cleanHorz)[i]
      #get current column element and RefDate column, create
      #Ticker column, then add onto stacked dataframe
      stacked <- stacked %>%
        rbind(
          cleanHorz[,c(1,i)] %>%
            mutate(Ticker = curStk) %>%
            `colnames<-`(c("RefDate",specColumnName,"Ticker"))
        )
    }
  }
  #copy value column onto end of dataframe, then remove it from
  # middle for ideal ordering of columns
  stacked <- (stacked %>%
                cbind(stacked[2]))[,-2]
  #return stacked frame
  return(stacked)
}

dis.database.connect <- function(type,dbName) {
  conn <- NA
  if(type == "local") {
    #establish SQL connection
    mydb = dbConnect(MySQL(), 
                     user='root', 
                     password='Rangers2014!', 
                     dbname=dbName, 
                     host='localhost')
    conn <- mydb
  } else {
    
  }
  return (conn)
}

dis.database.uploadScreener <- function(dbconn,screener) {
  s <- screener %>%
    transform(Company = gsub("'","*",Company)) %>%
    transform(Sector = gsub("'","*",Sector)) %>%
    transform(Industry = gsub("'","*",Industry)) %>%
    transform(Country = gsub("'","*",Country)) %>%
    select(Ticker,Company,Sector,Industry,Country,DateUpdated) %>%
    mutate(Insert = paste0("(",
                           paste0("'",Ticker,"'"),",",
                           paste0("'",Company,"'"),",",
                           paste0("'",Sector,"'"),",",
                           paste0("'",Industry,"'"),",",
                           paste0("'",Country,"'"),",",
                           paste0("'",DateUpdated,"'"),")"))
  dbGetQuery(dbconn,paste0("INSERT INTO DIS_Screener (",
                       paste(dbListFields(dbconn,"DIS_Screener"),collapse = ","),") VALUES ",
                       paste(s$Insert,collapse = ","),";"))
  return('Complete.')
}

dis.database.updateScreener <- function() {}

