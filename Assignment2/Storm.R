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

# 1 Storm Surge
rawStormDataDF_stormSurge <- StormDataDF_Fatalities_Injury %>%
        select(State, Event_Type, Fatalities, Injuries) %>%
        filter(Event_Type %in% c ("storm surge","coastal storm","dust storm","tropical storm"))

# rename Event Type to Storm Surge
rawStormDataDF_stormSurge$Event_Type <- "Storm Surge"

# 2 Flood
rawStormDataDF_flood <- StormDataDF_Fatalities_Injury %>%
        select(State, Event_Type, Fatalities, Injuries ) %>%
        filter(Event_Type %in% c("flooding","flood","flash flood","flood/flash flood","urban/sml stream fld"))

# rename Event_Type to Flood
rawStormDataDF_flood$Event_Type <- "Flood"

# 3 Tornado
rawStormDataDF_tornado <- StormDataDF_Fatalities_Injury %>%
        select(State, Event_Type, Fatalities, Injuries) %>%
        filter(Event_Type  %in% c("tornado","waterspout/tornado"))
# Rename Event Type to tornado
rawStormDataDF_tornado$Event_Type <- "Tornado"

# 4 Wintry
rawStormDataDF_wintry <- StormDataDF_Fatalities_Injury %>%
        select(State, Event_Type, Fatalities, Injuries) %>%
        filter(Event_Type %in% c("ice storm","ice","black ice","winter storm","winter storms","winter storm high winds","winter weather/mix","cold","thundersnow","heavy snow","blowing snow","snow","rain/snow","light snow","freezing rain","extreme windchill","blizzard","extreme cold","icy roads","avalanche","winter weather","extreme cold/wind chill","fog and cold temperatures","freezing drizzle","cold/wind chill"))

# Rename Event Type to Wintry
rawStormDataDF_wintry$Event_Type <- Wintry


# 5 Rain
rawStormDataDF_Rain <- StormDataDF_Fatalities_Injury %>%
        select(State, Event_Type, Fatalities, Injuries) %>%
        filter(Event_Type %in% c("rain","heavy rain","excessive rainfall","dense fog","fog"))
# Rename Event Type to rain
rawStormDataDF_Rain$Event_Type <- "Rain"


#  6 Lightning
rawStormDataDF_lightning <- StormDataDF_Fatalities_Injury %>%
        select(State , Event_Type , Fatalities , Injuries) %>%
        filter(Event_Type %in% c("lightning"))

# Rename Event Type to Lightning
rawStormDataDF_lightning$Event_Type <- "Lightning"

# 7 Wind
rawStormDataDF_wind <- StormDataDF_Fatalities_Injury %>%
        select(State , Event_Type , Fatalities , Injuries ) %>%
        filter(Event_Type %in% c("wind","tstm wind","high wind","high winds/snow","strong wind","strong winds","thunderstorm winds","marine strong wind","high winds","thunderstorm wind"))

# Rename Event Type to Wind
rawStormDataDF_wind$Event_Type


# 8 Hurricane
rawStormDataDF_Hurricane <- StormDataDF_Fatalities_Injury %>%
        select(State , Event_Type , Fatalities , Injuries ) %>%
        filter(Event_Type %in% c("hurricane","typhoon","hurricane/typhoon","tropical storm gordon"))

# Rename Event Type to Hurricane
rawStormDataDF_Hurricane$Event_Type <- "Hurricane"


# 9 Heat
rawStormDataDF_Heat <- StormDataDF_Fatalities_Injury %>%
        select(State , Event_Type , Fatalities , Injuries ) %>%
        filter(Event_Type %in% c("heat","heat wave","excessive heat","heat wave drought"))

# Rename Event type to Heat
rawStormDataDF_Heat$Event_Type <- "Heat"


# 10 Hail
rawStormDataDF_Hail <- StormDataDF_Fatalities_Injury %>%
        select(State , Event_Type , Fatalities , Injuries ) %>%
        filter(Event_Type %in% c("hail","tstm wind/hail"))
# Rename Event Type to Hail
rawStormDataDF_Hail$Event_Type <- "Hail"

# 11 Fire
rawStormDataDF_Fire <- StormDataDF_Fatalities_Injury %>%
        select(State, Event_Type, Fatalities, Injuries) %>%
        filter(Event_Type %in% c("wild fires","wild/forest fire","wildfire"))
# rename Event Type Fire
rawStormDataDF_Fire$Event_Type <- "Fire"


# 12 Sea Mishap
rawStormDataDF_Sea <- StormDataDF_Fatalities_Injury %>%
        select(State, Event_Type, Fatalities, Injuries) %>%
        filter(Event_Type %in% c("rip current","rough surf","strong winds","tsunami","high surf","marine accident","marine thunderstorm wind","marine mishap","rip currents","high wind and seas","heavy surf","heavy surf/high surf"))

# Rename Event type too Sea Mishap
rawStormDataDF_Sea$Event_Type <- "Sea Mishap"


# 13 Others
rawStormDataDF_Others <- StormDataDF_Fatalities_Injury %>%
        select(State , Event_Type , Fatalities , Injuries ) %>%
        filter( Event_Type != "storm surge",
                Event_Type != "coastal storm",
                Event_Type != "dust storm",
                Event_Type != "tropical storm",
                Event_Type != "flooding",
                Event_Type != "flood",
                Event_Type != "flash flood",
                Event_Type != "flood/flash flood",
                Event_Type != "urban/sml stream fld",
                Event_Type != "tornado",
                Event_Type != "waterspout/tornado",
                Event_Type != "tornado",
                Event_Type != "waterspout/tornado",
                Event_Type != "rain",
                Event_Type != "heavy rain",
                Event_Type != "excessive rainfall",
                Event_Type != "dense fog",
                Event_Type != "fog",
                Event_Type != "lightning",
                Event_Type != "wind",
                Event_Type != "tstm wind",
                Event_Type != "high wind",
                Event_Type != "high winds/snow",
                Event_Type != "strong wind",
                Event_Type != "strong winds",
                Event_Type != "thunderstorm winds",
                Event_Type != "marine strong wind",
                Event_Type != "high winds",
                Event_Type != "thunderstorm wind",
                Event_Type != "hurricane",
                Event_Type != "typhoon",
                Event_Type != "hurricane/typhoon",
                Event_Type != "tropical storm gordon",
                Event_Type != "heat",
                Event_Type != "heat wave",
                Event_Type != "excessive heat",
                Event_Type != "heat wave drought",
                Event_Type != "hail",
                Event_Type != "tstm wind/hail",
                Event_Type != "wild fires",
                Event_Type != "wild/forest fire",
                Event_Type != "wildfire",
                Event_Type != "rip current",
                Event_Type != "rough surf",
                Event_Type != "strong winds",
                Event_Type != "tsunami",
                Event_Type != "high surf",
                Event_Type != "marine accident",
                Event_Type != "marine thunderstorm wind",
                Event_Type != "marine mishap",
                Event_Type != "rip currents",
                Event_Type != "high wind and seas",
                Event_Type != "heavy surf",
                Event_Type != "heavy surf/high surf")

# rename Event type to Other
rawStormDataDF_Others$Event_Type <- "Others"

#Combine all cleaned tables
stormData <- rbind(rawStormDataDF_stormSurge,
                   rawStormDataDF_flood,
                   rawStormDataDF_tornado,
                   rawStormDataDF_wintry,
                   rawStormDataDF_Rain,
                   rawStormDataDF_lightning,
                   rawStormDataDF_wind,
                   rawStormDataDF_Hurricane,
                   rawStormDataDF_Heat,
                   rawStormDataDF_Hail,
                   rawStormDataDF_Fire,
                   rawStormDataDF_Sea,
                   rawStormDataDF_Others)
