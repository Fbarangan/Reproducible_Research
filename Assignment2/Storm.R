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

# rename Event Type to Storm Surge
rawStormDataDF_stormSurge$Event_Type <- "Storm Surge"

# Flood
rawStormDataDF_flood <- StormDataDF_Fatalities_Injury %>%
        select(State, Event_Type, Fatalities, Injuries ) %>%
        filter(Event_Type %in% c("flooding","flood","flash flood","flood/flash flood","urban/sml stream fld"))

# rename Event_Type to Flood
rawStormDataDF_flood$Event_Type <- "Flood"

# Tornado
rawStormDataDF_tornado <- StormDataDF_Fatalities_Injury %>%
        select(State, Event_Type, Fatalities, Injuries) %>%
        filter(Event_Type  %in% c("tornado","waterspout/tornado"))
# Rename Event Type to tornado
rawStormDataDF_tornado$Event_Type <- "Tornado"

# Wintry
rawStormDataDF_wintry <- StormDataDF_Fatalities_Injury %>%
        select(State, Event_Type, Fatalities, Injuries) %>%
        filter(Event_Type %in% c("ice storm","ice","black ice","winter storm","winter storms","winter storm high winds","winter weather/mix","cold","thundersnow","heavy snow","blowing snow","snow","rain/snow","light snow","freezing rain","extreme windchill","blizzard","extreme cold","icy roads","avalanche","winter weather","extreme cold/wind chill","fog and cold temperatures","freezing drizzle","cold/wind chill"))

# Rename Event Type to Wintry
rawStormDataDF_wintry$Event_Type <- Wintry


# Rain
rawStormDataDF_Rain <- StormDataDF_Fatalities_Injury %>%
        select(State, Event_Type, Fatalities, Injuries) %>%
        filter(Event_Type %in% c("rain","heavy rain","excessive rainfall","dense fog","fog"))
# Rename Event Type to rain
rawStormDataDF_Rain <- "Rain"


# Lightning
rawStormDataDF_lightning <- StormDataDF_Fatalities_Injury %>%
        select(State , Event_Type , Fatalities , Injuries) %>%
        filter(Event_Type %in% c("lightning"))

# Rename Event Type to Lightning
rawStormDataDF_lightning <- "Lightning"

# Wind
rawStormDataDF_wind <- StormDataDF_Fatalities_Injury %>%
        select(State , Event_Type , Fatalities , Injuries ) %>%
        filter(Event_Type %in% c("wind","tstm wind","high wind","high winds/snow","strong wind","strong winds","thunderstorm winds","marine strong wind","high winds","thunderstorm wind"))

# Rename Event Type to Wind
rawStormDataDF_wind$Event_Type


# Hurricane
rawStormDataDF_Hurricane <- StormDataDF_Fatalities_Injury %>%
        select(State , Event_Type , Fatalities , Injuries ) %>%
        filter(Event_Type %in% c("hurricane","typhoon","hurricane/typhoon","tropical storm gordon"))

# Rename Event Type to Hurricane
rawStormDataDF_Hurricane$Event_Type <- "Hurricane"


# Heat
rawStormDataDF_Heat <- StormDataDF_Fatalities_Injury %>%
        select(State , Event_Type , Fatalities , Injuries ) %>%
        filter(Event_Type %in% c("heat","heat wave","excessive heat","heat wave drought"))

# Rename Event type to Heat
rawStormDataDF_Heat$Event_Type <- "Heat"


# Hail
rawStormDataDF_Hail <- StormDataDF_Fatalities_Injury %>%
        select(State , Event_Type , Fatalities , Injuries ) %>%
        filter(Event_Type %in% c("hail","tstm wind/hail"))
# Rename Event Type to Hail
rawStormDataDF_Hail$Event_Type <- "Hail"

# Fire
rawStormDataDF_Fire <- StormDataDF_Fatalities_Injury %>%
        select(State, Event_Type, Fatalities, Injuries) %>%
        filter(Event_Type %in% c("wild fires","wild/forest fire","wildfire"))
# rename Event Type Fire
rawStormDataDF_Fire$Event_Type <- "Fire"


# Sea Mishap
rawStormDataDF_Sea <- StormDataDF_Fatalities_Injury %>%
        select(State, Event_Type, Fatalities, Injuries) %>%
        filter(Event_Type %in% c("rip current","rough surf","strong winds","tsunami","high surf","marine accident","marine thunderstorm wind","marine mishap","rip currents","high wind and seas","heavy surf","heavy surf/high surf"))

# Rename Event type too Sea Mishap
rawStormDataDF_Sea$Event_Type <- "Sea Mishap"


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
                Event_Type != "urban/sml stream fld",
                Event_Type != "flood/flash flood",
                Event_Type != "ice storm",
                Event_Type != "ice",
                Event_Type != "black ice",
                Event_Type != "winter storm",
                Event_Type != "winter storms",
                Event_Type != "winter storm high winds",
                Event_Type != "winter weather/mix",
                Event_Type != "cold",
                Event_Type != "cold/wind chill",
                Event_Type != "fog and cold temperatures",
                Event_Type != "freezing drizzle",
                Event_Type != "thundersnow",
                Event_Type != "heavy snow",
                Event_Type != "blowing snow",
                Event_Type != "snow",
                Event_Type != "extreme windchill",
                Event_Type != "rain/snow",
                Event_Type != "light snow",
                Event_Type != "blizzard",
                Event_Type != "extreme cold",
                Event_Type != "icy roads",
                Event_Type != "avalanche",
                Event_Type != "freezing rain",
                Event_Type != "winter weather",
                Event_Type !="extreme cold/wind chill",
                Event_Type != "rain",
                Event_Type != "heavy rain",
                Event_Type != "dense fog",
                Event_Type != "fog",
                Event_Type != "excessive rainfall",
                Event_Type != "lightning",
                Event_Type != "wind",
                Event_Type != "tstm wind",
                Event_Type != "high wind",
                Event_Type != "high winds",
                Event_Type !="thunderstorm wind",
                Event_Type != "high winds/snow",
                Event_Type != "marine strong wind",
                Event_Type != "strong wind",
                Event_Type != "thunderstorm winds",
                Event_Type != "thunderstorm winds",
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
                Event_Type !=  "wild fires",
                Event_Type !=  "wild/forest fire",
                Event_Type != "wildfire",
                Event_Type != "rip current",
                Event_Type != "rough surf",
                Event_Type != "strong winds",
                Event_Type != "tsunami",
                Event_Type != "high surf",
                Event_Type != "rip currents",
                Event_Type != "high wind and seas",
                Event_Type != "heavy surf",
                Event_Type != "heavy surf/high surf",
                Event_Type != "marine accident",
                Event_Type != "marine thunderstorm wind",
                Event_Type != "marine mishap")

# rename Event type to Other
rawStormDataDF_Others$Event_Type <- "Others"
