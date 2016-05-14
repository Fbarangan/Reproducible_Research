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


# Select only those fatalities and Injury > 0

StormDataDF_Fatalities_Injury <- rawStormDataDF_ %>%
select(State = STATE, Event_Type = EVTYPE, Fatalities = FATALITIES, Injuries = INJURIES) %>%
        filter( Fatalities > 0 & Injuries > 0)

# Storm Surge
rawStormDataDF_stormSurge <- StormDataDF_Fatalities_Injury %>%
        select(State, Event_Type, Fatalities, Injuries) %>%
        filter(Event_Type %in% c ("storm surge","coastal storm","dust storm","tropical storm"))

# Flood
rawStormDataDF_flood <- StormDataDF_Fatalities_Injury %>%
        select(State, Event_Type, Fatalities, Injuries ) %>%
        filter(Event_Type %in% c("flooding","flood","flash flood","flood/flash flood"))

# Tornado
rawStormDataDF_tornado <- StormDataDF_Fatalities_Injury %>%
        select(State, Event_Type, Fatalities, Injuries) %>%
        filter(Event_Type  %in% c("tornado","waterspout/tornado"))

# Wintry
rawStormDataDF_wintry <- StormDataDF_Fatalities_Injury %>%
        select(State, Event_Type, Fatalities, Injuries) %>%
        filter(Event_Type %in% c("ice storm","ice","black ice","winter storm","winter storm high winds","winter weather/mix","cold","thundersnow","heavy snow","blowing snow","snow","rain/snow","light snow","freezing rain","extreme windchill"))

# Rain
rawStormDataDF_Rain <- StormDataDF_Fatalities_Injury %>%
        select(State, Event_Type, Fatalities, Injuries) %>%
        filter(Event_Type %in% c("rain","heavy rain","excessive rainfall"))

# Lightning
rawStormDataDF_lightning <- StormDataDF_Fatalities_Injury %>%
        select(State , Event_Type , Fatalities , Injuries) %>%
        filter(Event_Type %in% c("lightning"))


# Wind
rawStormDataDF_wind <- StormDataDF_Fatalities_Injury %>%
        select(State , Event_Type , Fatalities , Injuries ) %>%
        filter(Event_Type %in% c("wind","tstm wind","high wind","high winds/snow","strong wind","thunderstorm winds"))

# Hurricane
rawStormDataDF_Hurricane <- StormDataDF_Fatalities_Injury %>%
        select(State , Event_Type , Fatalities , Injuries ) %>%
        filter(Event_Type %in% c("hurricane","tropical","typhoon","hurricane/typhoon"))

# Dry
rawStormDataDF_Dry_weather <- StormDataDF_Fatalities_Injury %>%
        select(State , Event_Type , Fatalities , Injuries ) %>%
        filter(Event_Type %in% c("dry","drought"))

# Heat
rawStormDataDF_Heat <- StormDataDF_Fatalities_Injury %>%
        select(State , Event_Type , Fatalities , Injuries ) %>%
        filter(Event_Type %in% c("heat","warm","excessive heat"))

# Hail
rawStormDataDF_Hail <- StormDataDF_Fatalities_Injury %>%
        select(State , Event_Type , Fatalities , Injuries ) %>%
        filter(Event_Type %in% c("hail","tstm wind/hail"))

# Fire
rawStormDataDF_Fire <- StormDataDF_Fatalities_Injury %>%
        select(State, Event_Type, Fatalities, Injuries) %>%
        filter(Event_Type %in% c("fire","wild fires","wild/forest fire","wildfire"))
# Others
rawStormDataDF_Others <- StormDataDF_Fatalities_Injury %>%
        select(State , Event_Type , Fatalities , Injuries ) %>%
        filter( Event_Type != "tornado",
                Event_Type != "waterspout/tornado",
                Event_Type !=  "storm surge",
                Event_Type != "tropical storm",
                Event_Type !=  "coastal storm",
                Event_Type !=  "dust storm",
                Event_Type != "flooding",
                Event_Type != "flood",
                Event_Type != "flash flood",
                Event_Type != "flood/flash flood",
                Event_Type != "ice storm",
                Event_Type != "ice",
                Event_Type != "black ice",
                Event_Type != "winter storm",
                Event_Type != "winter storm high winds",
                Event_Type != "winter weather/mix",
                Event_Type != "cold",
                Event_Type != "thundersnow",
                Event_Type != "heavy snow",
                Event_Type != "blowing snow",
                Event_Type != "snow",
                Event_Type !="extreme windchill",
                Event_Type != "rain/snow",
                Event_Type != "light snow",
                Event_Type != "freezing rain",
                Event_Type != "rain",
                Event_Type != "heavy rain",
                Event_Type != "excessive rainfall",
                Event_Type != "lightning",
                Event_Type !="wind",
                Event_Type !="tstm wind",
                Event_Type !="high wind",
                Event_Type !="high winds/snow",
                Event_Type != "strong wind",
                Event_Type != "thunderstorm winds")

