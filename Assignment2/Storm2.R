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
library(ggplot2)

getwd()
# Set correct working directory

if (!file.exists("stormData")) {dir.create("stormData")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destfile <- "./stormData/storm_data.zip"


download.file(fileUrl, destfile = destfile)
dateDownloaded <- date()

# Locate file and unzip
rawStormData <- read.csv("./stormData/storm_data", header = TRUE)

# Set correct working directory
# Change to DPLYR
rawStormDataDF_ <- tbl_df(rawStormData)
rawStormDataDF_$EVTYPE <- tolower(rawStormDataDF_$EVTYPE)
# Select Variable

# Variables selected
rawStormDataDF_ <- rawStormDataDF_ %>%
        select(State = STATE, Event_Type = EVTYPE, Fatalities = FATALITIES, Injuries = INJURIES,
               Property_Damage = PROPDMG, Property_Expo =  (PROPDMGEXP), Crop_Damage = CROPDMG,
               Crop_Expo = (CROPDMGEXP))

# 1 Storm Surge
rawStormDataDF_stormSurge <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type %in% c ("storm surge","coastal storm","dust storm","tropical storm"))

# List of Event type : "Storm Surge"
Event_Type_Storm_Surge <- as.vector(rawStormDataDF_stormSurge$Event_Type)
Event_Type_Storm_Surge

# rename Event Type to Storm Surge
rawStormDataDF_stormSurge$Event_Type <- "Storm Surge"

# 2 Flood
rawStormDataDF_flood <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type %in% c("flooding","flood","flash flood","flood/flash flood","urban/sml stream fld"))

# List of Event type : "Flood"
Event_Type_Flood <- as.vector(rawStormDataDF_flood$Event_Type)
Event_Type_Flood

# rename Event_Type to Flood
rawStormDataDF_flood$Event_Type <- "Flood"

# 3 Tornado
rawStormDataDF_tornado <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type  %in% c("tornado","waterspout/tornado"))

# List of Event type : "Tornado"
Event_Type_Tornado <-  as.vector(rawStormDataDF_tornado$Event_Type)
Event_Type_Tornado

# Rename Event Type to tornado
rawStormDataDF_tornado$Event_Type <- "Tornado"

# 4 Wintry
rawStormDataDF_wintry <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type %in% c("ice storm","ice","black ice","winter storm","winter storms","winter storm high winds","winter weather/mix","cold","thundersnow","heavy snow","blowing snow","snow","rain/snow","light snow","freezing rain","extreme windchill","blizzard","extreme cold","icy roads","avalanche","winter weather","extreme cold/wind chill","fog and cold temperatures","freezing drizzle","cold/wind chill"))

# List of Event type : "Wintry"
Event_Type_Wintry <- as.vector(rawStormDataDF_wintry$Event_Type)
Event_Type_Wintry

# Rename Event Type to Wintry
rawStormDataDF_wintry$Event_Type <- "Wintry"


# 5 Rain
rawStormDataDF_Rain <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type %in% c("rain","heavy rain","excessive rainfall","dense fog","fog"))
# Rename Event Type to rain
rawStormDataDF_Rain$Event_Type <- "Rain"

# List of Event type : "Rain"
Event_Type_Rain <- as.vector(rawStormDataDF_Rain$Event_Type)
Event_Type_Rain
        
#  6 Lightning
rawStormDataDF_lightning <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type %in% c("lightning"))

# List of Event type : "Lightning"
Event_Type_Lightning <- as.vector(rawStormDataDF_lightning$Event_Type)
Event_Type_Lightning

# Rename Event Type to Lightning
rawStormDataDF_lightning$Event_Type <- "Lightning"

# 7 Wind
rawStormDataDF_wind <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type %in% c("wind","tstm wind","high wind","high winds/snow","strong wind","strong winds","thunderstorm winds","marine strong wind","high winds","thunderstorm wind"))

# List of Event type : "Wind"
Event_Type_Wind <- as.vector(rawStormDataDF_wind$Event_Type)
Event_Type_Wind

# Rename Event Type to Wind
rawStormDataDF_wind$Event_Type <- "Wind"


# 8 Hurricane
rawStormDataDF_Hurricane <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type %in% c("hurricane","typhoon","hurricane/typhoon","tropical storm gordon"))

# List of Event type : "Hurricane"
Event_Type_Hurricane <- as.vector(rawStormDataDF_Hurricane$Event_Type)
Event_Type_Hurricane

# Rename Event Type to Hurricane
rawStormDataDF_Hurricane$Event_Type <- "Hurricane"


# 9 Heat
rawStormDataDF_Heat <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type %in% c("heat","heat wave","excessive heat","heat wave drought"))

# List of Event type : "Heat"
Event_Type_Heat <- as.vector(rawStormDataDF_Heat$Event_Type)
Event_Type_Heat

# Rename Event type to Heat
rawStormDataDF_Heat$Event_Type <- "Heat"


# 10 Hail
rawStormDataDF_Hail <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type %in% c("hail","tstm wind/hail"))
# Rename Event Type to Hail
rawStormDataDF_Hail$Event_Type <- "Hail"

# List of Event type : "Hail"
Event_Type_Hail <- as.vector(rawStormDataDF_Hail$Event_Type)
Event_Type_Hail

# 11 Fire
rawStormDataDF_Fire <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type %in% c("wild fires","wild/forest fire","wildfire"))
# rename Event Type Fire
rawStormDataDF_Fire$Event_Type <- "Fire"

# List of Event type : "Fire"
Event_Type_Fire <- as.vector(rawStormDataDF_Fire$Event_Type)
Event_Type_Fire

# 12 Sea Mishap
rawStormDataDF_Sea <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type %in% c("rip current","rough surf","tsunami","high surf","marine accident","marine thunderstorm wind","marine mishap","rip currents","high wind and seas","heavy surf","heavy surf/high surf"))

# List of Event type : "Sea Mishap"
Event_Type_Sea_Mishap <- as.vector(rawStormDataDF_Sea$Event_Type)
Event_Type_Sea_Mishap

# Rename Event type too Sea Mishap
rawStormDataDF_Sea$Event_Type <- "Sea Mishap"

# 13 Others
rawStormDataDF_Others <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter( Event_Type != "storm surge", Event_Type != "coastal storm", Event_Type != "dust storm",
                Event_Type != "tropical storm",

                Event_Type != "flooding", Event_Type != "flood", Event_Type != "flash flood",
                Event_Type != "flood/flash flood", Event_Type != "urban/sml stream fld",

                Event_Type != "tornado", Event_Type != "waterspout/tornado",

                Event_Type !=  "ice storm",Event_Type !=  "ice",Event_Type !=  "black ice",
                Event_Type !=  "winter storm",Event_Type !=  "winter storms",Event_Type !=  "winter storm high winds",
                Event_Type !=  "winter weather/mix",Event_Type !=  "cold",Event_Type !=  "thundersnow",
                Event_Type !=  "heavy snow",Event_Type !=  "blowing snow",Event_Type != "snow",
                Event_Type !=  "rain/snow",Event_Type !=  "light snow",Event_Type !=  "freezing rain",
                Event_Type !=  "extreme windchill",Event_Type !=  "blizzard",Event_Type !=  "extreme cold",
                Event_Type !=  "icy roads", Event_Type !=  "avalanche",Event_Type !=  "winter weather",
                Event_Type !=  "extreme cold/wind chill",Event_Type !=  "fog and cold temperatures",
                Event_Type !=  "freezing drizzle",Event_Type !=  "cold/wind chill",

                Event_Type != "rain",Event_Type != "heavy rain",Event_Type != "excessive rainfall",
                Event_Type != "dense fog",Event_Type != "fog",

                Event_Type != "lightning",

                Event_Type != "wind",Event_Type != "tstm wind",Event_Type != "high wind",
                Event_Type != "high winds/snow",Event_Type != "strong wind",Event_Type != "strong winds",
                Event_Type != "thunderstorm winds",Event_Type != "marine strong wind",Event_Type != "high winds",
                Event_Type != "thunderstorm wind",

                Event_Type != "hurricane",Event_Type != "typhoon",Event_Type != "hurricane/typhoon",
                Event_Type != "tropical storm gordon",

                Event_Type != "heat",Event_Type != "heat wave",Event_Type != "excessive heat",
                Event_Type != "heat wave drought",

                Event_Type != "hail",Event_Type != "tstm wind/hail",

                Event_Type != "wild fires",Event_Type != "wild/forest fire",Event_Type != "wildfire",

                Event_Type != "rip current",Event_Type != "rough surf",Event_Type != "strong winds",
                Event_Type != "tsunami",Event_Type != "high surf",Event_Type != "marine accident",
                Event_Type != "marine thunderstorm wind",Event_Type != "marine mishap",
                Event_Type != "rip currents",Event_Type != "high wind and seas",
                Event_Type != "heavy surf",Event_Type != "heavy surf/high surf")

# List of Event Type categorize as "Others

Event_Type_Others <- as.vector(rawStormDataDF_Others$Event_Type)
Event_Type_Others

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


#  0 observations for Fatalities and Injuries removed
StormDataDF_Fatalities_Injury <- stormData %>%
        select(State = State, Event_Type = Event_Type, Fatalities = Fatalities, Injuries = Injuries) %>%
        filter( Fatalities > 0 & Injuries > 0)


# Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

Population_Health_Fatalities <- StormDataDF_Fatalities_Injury %>%
                        group_by(Event_Type) %>%
                        summarise(Fatalities = sum(Fatalities), n = n()) %>%
                        arrange(Fatalities = desc(Fatalities))

# top 10
top_Population_Health_Fatalities <- Population_Health_Fatalities[c(1:10),]


Population_Health_Injuries <- StormDataDF_Fatalities_Injury %>%
                        group_by(Event_Type) %>%
                        summarise(Injuries = sum(Injuries), n = n()) %>%
                        arrange(Injuries = desc(Injuries))
# top 10
top_Population_Health_Injuries <- Population_Health_Injuries[c(1:10),]


# Question 2
# Across the United States, which types of events have the greatest economic consequences?

# Select Variable exluding thse with 0 value for Property Damage and Crop Damage
StormDataDF_Economic_ <- stormData %>%
                                select (State, Event_Type, Property_Damage = as.integer(Property_Damage),
                                       Property_Expo, Crop_Damage = as.integer(Crop_Damage), Crop_Expo) %>%
                                filter( Property_Damage > 0 & Crop_Damage > 0)


StormDataDF_Economic_$Property_Expo <- as.character(StormDataDF_Economic_$Property_Expo)
StormDataDF_Economic_$Crop_Expo <- as.character(StormDataDF_Economic_$Crop_Expo)


# Change Exponent to all uppercase
StormDataDF_Economic_$Property_Expo <- toupper(StormDataDF_Economic_$Property_Expo)
StormDataDF_Economic_$Crop_Expo <- toupper(StormDataDF_Economic_$Crop_Expo )


#  selected and Replace Property damge Expo "K", "M", and "B" with 1,000, ; 1,000,000 and 1,000,000,000 respectively
StormDataDF_Economic_Property_Values_K <- StormDataDF_Economic_ %>%
                                        filter (Property_Expo = (Property_Expo == "K")) %>%
                                        select(State, Event_Type, Property_Damage, Property_Expo,
                                               Crop_Damage, Crop_Expo ) %>%
                                        mutate(Property_Expo = 10^3 )


StormDataDF_Economic_Property_Values_M <- StormDataDF_Economic_ %>%
                                        filter (Property_Expo = (Property_Expo == "M")) %>%
                                        select(State,Event_Type, Property_Damage, Property_Expo,
                                                Crop_Damage, Crop_Expo ) %>%
                                        mutate(Property_Expo = 10^6 )

StormDataDF_Economic_Property_Values_B <- StormDataDF_Economic_ %>%
                                        filter (Property_Expo = (Property_Expo == "B")) %>%
                                        select(State, Event_Type, Property_Damage, Property_Expo,
                                               Crop_Damage, Crop_Expo ) %>%
                                        mutate(Property_Expo = 10^9 )


# select columns excluding those with 1,1000, 1000000, 1000000000
StormDataDF_Economic_Property_Values_others <- StormDataDF_Economic_ %>%
                                        filter (Property_Expo != "",
                                                Property_Expo != "K",
                                                Property_Expo != "M",
                                                Property_Expo != "B") %>%
                                        select(State, Event_Type, Property_Damage, Property_Expo,
                                                Crop_Damage, Crop_Expo )

# Combined all
stormProperty_value_Cleaned <- rbind ( StormDataDF_Economic_Property_Values_K,
                                       StormDataDF_Economic_Property_Values_M,
                                       StormDataDF_Economic_Property_Values_B,
                                    #   StormDataDF_Economic_Property_Values_Special_Char,
                                       StormDataDF_Economic_Property_Values_others)

stormProperty_value_Cleaned$Property_Expo <-  as.numeric(stormProperty_value_Cleaned$Property_Expo)


#  selected and Replace Crop damge Expo "K", "M", and "B" with 1,000, ; 1,000,000 and 1,000,000,000 respectively
StormDataDF_Economic_Crop_Values_K <- stormProperty_value_Cleaned %>%
                                        filter (Crop_Expo = (Crop_Expo == "K")) %>%
                                        select(State, Event_Type, Property_Damage, Property_Expo,
                                        Crop_Damage, Crop_Expo ) %>%
                                        mutate(Crop_Expo = 10^3 )

StormDataDF_Economic_Crop_Values_M <- stormProperty_value_Cleaned %>%
                                        filter (Crop_Expo = (Crop_Expo == "M")) %>%
                                        select(State,Event_Type, Property_Damage, Property_Expo,
                                        Crop_Damage, Crop_Expo ) %>%
                                        mutate(Crop_Expo = 10^6 )

StormDataDF_Economic_Crop_Values_B <- stormProperty_value_Cleaned %>%
                                        filter (Crop_Expo = (Crop_Expo == "B")) %>%
                                        select(State, Event_Type, Property_Damage, Property_Expo,
                                        Crop_Damage, Crop_Expo ) %>%
                                        mutate(Crop_Expo = 10^9 )


# select columns excluding those with 1,1000, 1000000, 1000000000
StormDataDF_Economic_Crop_Values_others <- stormProperty_value_Cleaned %>%
                                        filter (Crop_Expo != "K",
                                                Crop_Expo != "M",
                                                Crop_Expo != "B") %>%
                                        select(State, Event_Type, Property_Damage, Property_Expo,
                                        Crop_Damage, Crop_Expo )

# Combined all
stormProperty_Crop_Value_ <- rbind ( StormDataDF_Economic_Crop_Values_K,
                                       StormDataDF_Economic_Crop_Values_M,
                                       StormDataDF_Economic_Crop_Values_B,
                                       StormDataDF_Economic_Crop_Values_others)
# force   Crop_Expo to numeric
stormProperty_Crop_Value_$Crop_Expo <- as.numeric(stormProperty_Crop_Value_$Crop_Expo)

#  Top Events that affected Property and Crop Damage

Property_Damage <- stormProperty_Crop_Value_ %>%
                                group_by(Event_Type) %>%
                                mutate(Property_Value =  Property_Damage * Property_Expo) %>%
                                summarise(Property_Value = sum(Property_Value), n = n()) %>%
                                arrange(Propert_Value = desc(Property_Value))

# Select top 10
top_Property_Damage <- Property_Damage[c(1:10),]


Crop_Damage <- stormProperty_Crop_Value_ %>%
                                group_by(Event_Type) %>%
                                mutate(Crop_Value = Crop_Damage * Crop_Expo) %>%
                                summarise(Crop_Value = sum(Crop_Value), n = n()) %>%
                                arrange(Crop_Value = desc(Crop_Value))

# Select top 10
top_Crop_Damage <-  Crop_Damage[c(1:10),]

