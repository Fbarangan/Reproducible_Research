# Course Assignment 2
# data is from   https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2
#
# info https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf
#
# FAQ https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf
#
# Title
#
install.packages("rio")
library(rio)

if (!file.exists("stormData")) {dir.create("stormData")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destfile <- "./stormData/storm_data.zip"

download.file(fileUrl, destfile = destfile)
dateDownloaded <- date()

rawStormData <- read.csv("./stormData/storm_data", header = TRUE)

# Change to DPLYR
rawStormDataDF_ <- tbl_df(rawStormData)
rawStormDataDF_$EVTYPE <- tolower(rawStormDataDF_$EVTYPE)
# Select Variable

# Storm Surge
rawStormDataDF_stormSurge <- rawStormDataDF_ %>%
        select(State = STATE, Event_Type = EVTYPE, Fatalities = FATALITIES, Injuries = INJURIES) %>%
        filter(Event_Type == "storm surge")

# Flood
rawStormDataDF_flood <- rawStormDataDF_ %>%
        select(State = STATE, Event_Type = EVTYPE, Fatalities = FATALITIES, Injuries = INJURIES) %>%
        filter(Event_Type == "flood")

# Tornado
rawStormDataDF_tornado <- rawStormDataDF_ %>%
        select(State = STATE, Event_Type = EVTYPE, Fatalities = FATALITIES, Injuries = INJURIES) %>%
        filter(Event_Type == "tornado")

# Wintry
rawStormDataDF_wintry <- rawStormDataDF_ %>%
        select(State = STATE, Event_Type = EVTYPE, Fatalities = FATALITIES, Injuries = INJURIES) %>%
        filter(Event_Type %in% c("snow","ice","wintry","freez","blizzard","cold|winter"))



