

############################################
#----- set working directory -----
############################################

setwd("/Users/thomasfalconer//Desktop/University/Energy Data Analysis - BENV0091/Project")
getwd()

############################################
#----- enabling packages -----
############################################

library(lubridate)
library(httr)
library(dplyr)
library(tidyverse)

############################################
#----- parameter setting -----
############################################

# api key for elexon portal
e.api.key <- "g1ig3sf00x6cphp"
# generation output per unit service
e.service.actual <- "B1610"
# return all settlement periods
e.settle.period <- "*"
# return csv file type
e.service.type <- "csv"
# setting required date range
from.date <- as.Date("01-01-17", format="%d-%m-%y")
to.date  <- as.Date("31-12-17", format="%d-%m-%y")

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
  # print url to check index
  print(e.url)
}

############################################
#----- bsuos charge -----
############################################

bsuos.raw <- read.csv("bsuos_2017.csv", stringsAsFactors = F)
bsuos.raw$Date <- dmy(bsuos.raw$Date)

############################################
#----- merging data sets -----
############################################

# setting column labels for generation data
colnames(generation.data) <- c("Time Series", "Reg EIC", "BM", "NGC BM", "PSR", "Mkt EIC", "Mkt BM", "Mkt NGC BM", "Date", "Period", "MW")
# convert factor to numeric
generation.data$Period <- as.numeric(as.character(generation.data$Period))
# join data sets by date and period
joined.data <- left_join(generation.data, bsuos.raw, by=c("Date", "Period"))

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
complete.data <- complete.data[,c("Date", "Period", "BSUoS", "Reg EIC", "Asset Name", "Party", "MW", "Max Capacity", "Fuel Type")]

############################################
#----- output -----
############################################

# write csv file for output
write.csv(complete.data, "generation_bsuos.csv", row.names=FALSE)

