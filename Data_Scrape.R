
############################################
#----- set working directory -----
############################################

setwd("/Users/thomasfalconer//Desktop/University/Energy Data Analysis - BENV0091/Project")
getwd()

############################################
#----- enabling packages -----
############################################

library(httr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(mclust)

############################################
#----- parameter setting -----
############################################

# carbon intensity API service
c.service.type <- "intensity"
# api key for elexon portal
e.api.key <- "g1ig3sf00x6cphp"
# generation output per unit service
e.service.actual <- "B1610"
# return all settlement periods
e.settle.period <- "*"
# return csv file type
e.service.type <- "csv"
# setting required date range
from.date <- as.Date("31-12-18", format="%d-%m-%y")
to.date  <- as.Date("31-12-18", format="%d-%m-%y")

############################################
#----- get request - unit generation -----
############################################

# date index for while loop (start at from.date)
current.date <- from.date
# empty dataframe for data binding
generation.data <- data.frame()
# get request from elexon portal
while(current.date <= to.date){
  # capture url request
  e.url <- capture.output(cat("https://api.bmreports.com/BMRS/",
                            e.service.actual,
                            "/v2?APIKey=", e.api.key,
                            "&SettlementDate=", format(current.date),
                            "&Period=", e.settle.period,
                            "&ServiceType=", e.service.type,
                            sep=""))
  # GET request
  generation.raw <- GET(url = e.url)
  # convert raw data to character data
  generation.char <- rawToChar(generation.raw$content)
  # tidy to table format
  generation.clean <- read.table(text = generation.char, sep=',', fill = TRUE)
  # remove unwanted rows
  generation.clean <- generation.clean[-(1:2),]
  # set date format
  generation.clean$V9 <- ymd(generation.clean$V9)
  # reset row numbers
  rownames(generation.clean) <- NULL
  # bind with dataframe
  generation.data <- rbind(generation.data, generation.clean) 
  # increase date index by 1 day
  current.date <- current.date + 1
}

############################################
#----- get request - carbon intensity -----
############################################

# reset date index for while loop (start at from.date)
current.date <- from.date
# empty dataframe for data binding
carbon.data <- data.frame()
# get request from carbon intensity
while(current.date <= to.date){
  # capture url request
  c.url <- capture.output(cat("https://api.carbonintensity.org.uk/",
                            c.service.type,
                            "/date/", format(current.date),
                            sep=""))
  # GET request
  carbon.raw <- GET(url = c.url)
  # convert raw data to json data
  carbon.json <- rawToChar(carbon.raw$content)
  # convert json data to list format
  carbon.clean <- jsonlite::fromJSON(carbon.json)
  # from lust to dataframe
  carbon.clean <- carbon.clean$data
  # format intensity
  carbon.clean <- data.frame(carbon.clean$from, carbon.clean$to,carbon.clean$intensity$forecast, carbon.clean$intensity$actual, carbon.clean$intensity$index)
  # add column for settlement period
  carbon.clean$period <- 1:48
  # add column for date
  carbon.clean$date <- date(ymd_hm(carbon.clean$carbon.clean.from))
  # remove redundant date/time columns
  carbon.clean <- carbon.clean[,-c(1,2)]
  # reset row numbers
  rownames(carbon.clean) <- NULL
  # bind with dataframe
  carbon.data <- rbind(carbon.data, carbon.clean) 
  # increase date index by 1 day
  current.date <- current.date + 1
}

############################################
#----- merging data sets -----
############################################

# setting column labels for generation data
colnames(generation.data) <- c("Time Series", "Reg EIC", "BM", "NGC BM", "PSR", "Mkt EIC", "Mkt BM", "Mkt NGC BM", "Date", "Period", "MW")
# setting column labels for carbon intensity data
colnames(carbon.data) <- c("Forecast", "Actual", "Index", "Period", "Date" )
# convert factor to numeric
generation.data$Period <- as.numeric(as.character(generation.data$Period))
# join data sets by date and period
joined.data <- left_join(generation.data, carbon.data, by=c("Date", "Period"))

############################################
#----- EIC codes -----
############################################

# ready tidy EIC dataset
EIC.raw <- read.csv("EIC_tidy.csv", stringsAsFactors = F)
# remove unwanted columns
EIC.clean <- EIC.raw[,c(1,2,5,6,7,9,14,16,17,18)]
# setting column labels
colnames(EIC.clean) <- c("Display Name", "Station", "Reg EIC", "Function", "Asset Name", "Party", "BMU Type", "Fuel Type", "Max Capacity", "Node")
# convert factor to character
joined.data$`Reg EIC` <- as.character(joined.data$`Reg EIC`)
# join data sets by EIC code
complete.data <- left_join(joined.data, EIC.clean, by=("Reg EIC"))
# remove unwanted columns
complete.data <- complete.data[,c("Date", "Period", "Forecast", "Actual", "Index", "Reg EIC", "Asset Name", "Party", "MW", "Max Capacity", "Fuel Type")]

############################################
#----- output -----
############################################

# write csv file for output
write.csv(complete.data, "generation_carbon_intensity.csv", row.names=FALSE)


