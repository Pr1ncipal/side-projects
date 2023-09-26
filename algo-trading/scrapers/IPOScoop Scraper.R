#NOTE: SCOOP rating is ONLY available to subscribers,
#so it doesn't return anything right now.

#libraries. Install if necessary
library(rvest)
library(dplyr)

#Insider trading URL
url <- "https://www.iposcoop.com/ipo-calendar/"
page <- read_html(url)

#code that pulls the numbers. put in brackets to make cleaner
{
  #First line creates the variable, second line finds the ticker,
  #third line turns it into the text we actually need.
  columns <- page %>% 
    html_nodes("th") %>%
    html_text()
  #View(columns)
}
#company
{
  company <- page %>% 
    html_nodes("td:nth-child(1) a") %>%
    html_text()
  #View(company)
}
#symbol
{
  symbol <- page %>% 
    html_nodes("td:nth-child(2)") %>%
    html_text()
  #View(symbol)
}
#lead managers
{
  lead_managers <- page %>% 
    html_nodes("td:nth-child(3)") %>%
    html_text()
  #View(lead_managers)
}
#shares (millions)
{
  shares_millions <- page %>% 
    html_nodes("td:nth-child(4)") %>%
    html_text()
  #View(shares_millions)
}
#price (low)
{
  price_low <- page %>% 
    html_nodes("td:nth-child(5)") %>%
    html_text()
  #View(price_low)
}
#price (high)
{
  price_high <- page %>% 
    html_nodes(".hide-sm+ td.hide-sm") %>%
    html_text()
  #View(price_high)
}
#estimated dollar volume
{
  est_dollar_volume <- page %>% 
    html_nodes("td:nth-child(7)") %>%
    html_text()
  #View(est_dollar_value)
}
#expected trade date
{
  expected_trade_date <- page %>% 
    html_nodes("td:nth-child(8)") %>%
    html_text()
  #View(expected_trade_date)
}
#SCOOP rating
{
  SCOOP_rating <- page %>% 
    html_nodes("td:nth-child(9)") %>%
    html_text()
  #View(SCOOP_rating)
}
#rating change
{
  rating_change <- page %>% 
    html_nodes("td:nth-child(10)") %>%
    html_text()
  #View(rating_change)
}

IPOScoop <- data.frame(
  company = company,
  symbol = symbol,
  lead_managers = lead_managers,
  shares_millions = shares_millions,
  price_low = price_low,
  price_high = price_high,
  est_dollar_volume = est_dollar_volume,
  expected_trade_date = expected_trade_date,
  SCOOP_rating = SCOOP_rating,
  rating_change = rating_change
)
View(IPOScoop)
