# Course Assignment 2
# data is from   https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2
#
# info https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf
#
# FAQ https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf
#
# Title
#
install.packages("dplyr")
library(dplyr)
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
        filter(Event_Type %in% c("snow","ice","wintry","freeze","blizzard","cold","winter"))

# Rain
rawStormDataDF_Rain <- rawStormDataDF_ %>%
        select(State = STATE, Event_Type = EVTYPE, Fatalities = FATALITIES, Injuries = INJURIES) %>%
        filter(Event_Type %in% c("rain","shower"))

# Wind
rawStormDataDF_wind <- rawStormDataDF_ %>%
        select(State = STATE, Event_Type = EVTYPE, Fatalities = FATALITIES, Injuries = INJURIES) %>%
        filter(Event_Type %in% c("wind"))

# Hurricane
rawStormDataDF_Hurricane <- rawStormDataDF_ %>%
        select(State = STATE, Event_Type = EVTYPE, Fatalities = FATALITIES, Injuries = INJURIES) %>%
        filter(Event_Type %in% c("hurricane","tropical","typhoon"))

# Dry
rawStormDataDF_Dry_weather <- rawStormDataDF_ %>%
        select(State = STATE, Event_Type = EVTYPE, Fatalities = FATALITIES, Injuries = INJURIES) %>%
        filter(Event_Type %in% c("dry","drought"))

# Heat
rawStormDataDF_Heat <- rawStormDataDF_ %>%
        select(State = STATE, Event_Type = EVTYPE, Fatalities = FATALITIES, Injuries = INJURIES) %>%
        filter(Event_Type %in% c("heat","warm"))

# Hail
rawStormDataDF_Hail <- rawStormDataDF_ %>%
        select(State = STATE, Event_Type = EVTYPE, Fatalities = FATALITIES, Injuries = INJURIES) %>%
        filter(Event_Type %in% c("hail"))

# Fire
rawStormDataDF_Fire <- rawStormDataDF_ %>%
        select(State = STATE, Event_Type = EVTYPE, Fatalities = FATALITIES, Injuries = INJURIES) %>%
        filter(Event_Type %in% c("fire"))
# Others
rawStormDataDF_Others <- rawStormDataDF_ %>%
        select(State = STATE, Event_Type = EVTYPE, Fatalities = FATALITIES, Injuries = INJURIES) %>%
        filter(Event_Type != "storm surge",
               Event_Type != "flood",
               Event_Type != "tornado",
               Event_Type != "snow",
               Event_Type != "ice",
               Event_Type != "wintry",
               Event_Type != "freeze",
               Event_Type != "blizzard",
               Event_Type != "cold",
               Event_Type != "winter",
               Event_Type != "rain",
               Event_Type != "shower",
               Event_Type != "wind",
               Event_Type != "hurricane",
               Event_Type != "tropical",
               Event_Type != "typhoon",
               Event_Type != "dry",
               Event_Type != "drought",
               Event_Type != "heat",
               Event_Type != "warm",
               Event_Type != "hail",
               Event_Type != "fire")

