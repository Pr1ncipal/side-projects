source("./dis_w_hedge_functions.R")
#load R packages
dis.general.loadPackages()

#read screener in from finviz
  #build in try-catch iteration for node
screener <- dis.finviz.screener.scraper("","Overview",0,17)

#build temporary list
temp <- screener %>%
  #isolate Sector and Industry with Sector first, Industries within Sector
  select(Sector,Industry) %>%
  #order each alphabetically
  arrange(Sector,Industry)

#build lists of sectors
sectorList <- temp[!duplicated(temp),] %>%
  #null out rownames
  `rownames<-`(c())

#get list of sectors for iteration by removing duplicates
sectors <- c(unique(sectorList$Sector))

#iterate through each sector
for(i in seq(sectors)) {
  #build list of industries in the sector
  ind <- sectorList %>%
    #filter sector and industry list to show only the current Sector
    filter(Sector == sectors[i]) %>%
    #select only the Industry column, as Sector is specified in column header
    select(Industry) %>%
    #name it with Sector specification
    `colnames<-`(c(paste0("Sector: ",sectors[i])))
  write_csv(ind,paste0("./sector_industry_lists/",sectors[i],".csv"))
}
