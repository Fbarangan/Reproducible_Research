# Storm_Data
Felix Barangan  
May 7, 2016  
        
```r
# Libraries loading...
library(dplyr)
library(ggplot2)
library(knitr)
library(reshape2)
library(xtable)
```

## Summary Title
### Synopsis
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

## Data Transformation
Please set R and Rstudio to the correct and desired directory.

```r
# Please point your R/Rstudio to the desired Directory
getwd()
```

```
## [1] "/Users/kfcbarangan/Desktop/DataScienceCourse/Reproducible_Research/Assignment2/Reproducible_Research/Assignment2"
```
                
### Data extraction
* Data downloaded and extracted to the designated file folder location. for OSX user, it is advisable to set method="curl" when downloading https. Otherwise knit may not work. 
                

```r
if (!file.exists("stormData")) {
        dir.create("stormData")
}              
                
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destfile <- "./stormData/storm_data.zip"
                
download.file( url = fileUrl, destfile = destfile, method = "curl")
```

* Locate the file from your machine and unzip downloaded file       

```r
rawStormData <- read.csv("./stormData/storm_data", header = TRUE)    
```

### Data Processing
* Set correct working directory. Then change/convert file to tbl_df using DPLYR package for ease of data manipulation. The event type were selected and converted to all lower case.   

```r
rawStormDataDF_ <- tbl_df(rawStormData)
rawStormDataDF_$EVTYPE <- tolower(rawStormDataDF_$EVTYPE)
```

* Select Variables from the raw data which includes:

```r
rawStormDataDF_ <- rawStormDataDF_ %>%
        select(State = STATE, Event_Type = EVTYPE, Fatalities = FATALITIES, Injuries = INJURIES,
               Property_Damage = PROPDMG, Property_Expo =  (PROPDMGEXP), Crop_Damage = CROPDMG,
               Crop_Expo = (CROPDMGEXP))
       
names(rawStormDataDF_)               
```

```
## [1] "State"           "Event_Type"      "Fatalities"      "Injuries"       
## [5] "Property_Damage" "Property_Expo"   "Crop_Damage"     "Crop_Expo"
```
                
        
* The Event_Type where group together in order to summarize data effectively. A total of 13 groups were created. Group [Others] includes other minor event type (see Appendix). The method use to group the event type employs temporarily creating a table by filtering possible event type combinations. These were then coded with the name of the group (e.g. "flood"). Afterwards, theses group were added back together using the rbind function.

```r
# 1 Storm Surge
rawStormDataDF_stormSurge <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type %in% c ("storm surge","coastal storm","dust storm","tropical storm"))

# List of Event type : "Storm Surge"
Event_Type_Storm_Surge <- as.data.frame(c("storm surge","coastal storm","dust storm","tropical storm"))
colnames(Event_Type_Storm_Surge)[1] <- "Storm Surge Category"

# rename Event Type to Storm Surge
rawStormDataDF_stormSurge$Event_Type <- "Storm Surge"

# 2 Flood
rawStormDataDF_flood <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type %in% c("flooding","flood","flash flood","flood/flash flood","urban/sml stream fld"))

# List of Event type : "Flood"
Event_Type_Flood <-  as.data.frame(c("flooding","flood","flash flood","flood/flash flood","urban/sml stream fld"))
colnames(Event_Type_Flood)[1] <- "Flood Category"

# rename Event_Type to Flood
rawStormDataDF_flood$Event_Type <- "Flood"

# 3 Tornado
rawStormDataDF_tornado <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type  %in% c("tornado","waterspout/tornado"))

# List of Event type : "Tornado"
Event_Type_Tornado <-  as.data.frame(c("tornado","waterspout/tornado"))
colnames(Event_Type_Tornado)[1] <- "Tornado Category"

# Rename Event Type to tornado
rawStormDataDF_tornado$Event_Type <- "Tornado"

# 4 Wintry
rawStormDataDF_wintry <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type %in% c("ice storm","ice","black ice","winter storm","winter storms","winter storm high winds","winter weather/mix","cold","thundersnow","heavy snow","blowing snow","snow","rain/snow","light snow","freezing rain","extreme windchill","blizzard","extreme cold","icy roads","avalanche","winter weather","extreme cold/wind chill","fog and cold temperatures","freezing drizzle","cold/wind chill"))

# List of Event type : "Wintry"
Event_Type_Wintry <- as.data.frame(c("ice storm","ice","black ice","winter storm","winter storms","winter storm high winds","winter weather/mix","cold","thundersnow","heavy snow","blowing snow","snow","rain/snow","light snow","freezing rain","extreme windchill","blizzard","extreme cold","icy roads","avalanche","winter weather","extreme cold/wind chill","fog and cold temperatures","freezing drizzle","cold/wind chill"))

colnames(Event_Type_Wintry)[1] <- "Wintry Category"

# Rename Event Type to Wintry
rawStormDataDF_wintry$Event_Type <- "Wintry"

# 5 Rain
rawStormDataDF_Rain <- rawStormDataDF_ %>%
                select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
                filter(Event_Type %in% c("rain","heavy rain","excessive rainfall","dense fog","fog"))

# List of Event type : "Rain"
Event_Type_Rain <- as.data.frame(c("rain","heavy rain","excessive rainfall","dense fog","fog"))
colnames(Event_Type_Rain)[1] <- "Rain Category"

# Rename Event Type to rain
rawStormDataDF_Rain$Event_Type <- "Rain"

#  6 Lightning
rawStormDataDF_lightning <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type %in% c("lightning"))

# List of Event type : "Lightning"
Event_Type_Lightning <- as.data.frame(c("lightning"))
colnames(Event_Type_Lightning)[1] <- "Lightning Category"

# Rename Event Type to Lightning
rawStormDataDF_lightning$Event_Type <- "Lightning"

# 7 Wind
rawStormDataDF_wind <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type %in% c("wind","tstm wind","high wind","high winds/snow","strong wind","strong winds","thunderstorm winds","marine strong wind","high winds","thunderstorm wind"))

# List of Event type : "Wind"
Event_Type_Wind <- as.data.frame(c("wind","tstm wind","high wind","high winds/snow","strong wind","strong winds","thunderstorm winds","marine strong wind","high winds","thunderstorm wind"))
colnames(Event_Type_Wind)[1] <- "Wind Category"

# Rename Event Type to Wind
rawStormDataDF_wind$Event_Type <- "Wind"

# 8 Hurricane
rawStormDataDF_Hurricane <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type %in% c("hurricane","typhoon","hurricane/typhoon","tropical storm gordon"))

# List of Event type : "Hurricane"
Event_Type_Hurricane <- as.data.frame(c("hurricane","typhoon","hurricane/typhoon","tropical storm gordon"))
colnames(Event_Type_Hurricane)[1] <- "Hurricane category"

# Rename Event Type to Hurricane
rawStormDataDF_Hurricane$Event_Type <- "Hurricane"

# 9 Heat
rawStormDataDF_Heat <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type %in% c("heat","heat wave","excessive heat","heat wave drought"))

# List of Event type : "Heat"
Event_Type_Heat <- as.data.frame(c("heat","heat wave","excessive heat","heat wave drought"))
colnames(Event_Type_Heat)[1] <- "Heat Category"

# Rename Event type to Heat
rawStormDataDF_Heat$Event_Type <- "Heat"

# 10 Hail
rawStormDataDF_Hail <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type %in% c("hail","tstm wind/hail"))
        
# List of Event type : "Hail"
Event_Type_Hail <- as.data.frame(c("hail","tstm wind/hail"))
colnames(Event_Type_Hail)[1] <- "Hail Category"
        
# Rename Event Type to Hail
rawStormDataDF_Hail$Event_Type <- "Hail"

# 11 Fire
rawStormDataDF_Fire <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type %in% c("wild fires","wild/forest fire","wildfire"))

# List of Event type : "Fire"
Event_Type_Fire <- as.data.frame(c("wild fires","wild/forest fire","wildfire"))
colnames(Event_Type_Fire)[1] <- "Fire Category"

# rename Event Type Fire
rawStormDataDF_Fire$Event_Type <- "Fire"

# 12 Sea Mishap
rawStormDataDF_Sea <- rawStormDataDF_ %>%
        select(State, Event_Type, Fatalities, Injuries, Property_Damage, Property_Expo, Crop_Damage, Crop_Expo) %>%
        filter(Event_Type %in% c("rip current","rough surf","tsunami","high surf","marine accident","marine thunderstorm wind","marine mishap","rip currents","high wind and seas","heavy surf","heavy surf/high surf"))

# List of Event type : "Sea Mishap"
Event_Type_Sea_Mishap <- as.data.frame(c("rip current","rough surf","tsunami","high surf","marine accident","marine thunderstorm wind","marine mishap","rip currents","high wind and seas","heavy surf","heavy surf/high surf"))
colnames(Event_Type_Sea_Mishap)[1] <- "Sea Mishap Category"

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
Event_Type_Others <- rawStormDataDF_Others %>%
        group_by(Event_Type) %>%
        summarise(n=n()) %>%
        arrange(desc(n))
colnames(Event_Type_Others)[1] <- "Others category"

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
```
                

* To answer question 1, Fatalities and Injury where extracted and filtered to show all observations with values > 0. This will decrease the number of Event type that does not show any implication to affect the number of Fatalities and Injuries incurred.

```r
#  0 observations for Fatalities and Injuries removed
StormDataDF_Fatalities_Injury <- stormData %>%
        select(State = State, Event_Type = Event_Type, Fatalities = Fatalities, Injuries = Injuries) %>%
        filter( Fatalities > 0 & Injuries > 0)


# Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
```

* A separate tables were used to extract the Event type that affect Population Health in terms of Fatalities and Injuries    

```r
Population_Health_Fatalities <- StormDataDF_Fatalities_Injury %>%
                        group_by(Event_Type) %>%
                        summarise(Fatalities = sum(Fatalities), n = n()) %>%
                        arrange(Fatalities = desc(Fatalities))

Population_Health_Injuries <- StormDataDF_Fatalities_Injury %>%
                        group_by(Event_Type) %>%
                        summarise(Injuries = sum(Injuries), n = n()) %>%
                        arrange(Injuries = desc(Injuries))
```

* To answer question 2, Property damages and Crop damages where extracted and filtered to show all observations with values > 0. This will decrease the number of Event type that does not show any implication to affect the number of Property Damages and Crop damages incurred.

```r
# Question 2
# Across the United States, which types of events have the greatest economic consequences?

# Select Variable exluding thse with 0 value for Property Damage and Crop Damage
StormDataDF_Economic_ <- stormData %>%
                                select (State, Event_Type, Property_Damage = as.integer(Property_Damage),
                                       Property_Expo, Crop_Damage = as.integer(Crop_Damage), Crop_Expo) %>%
                                filter( Property_Damage > 0 & Crop_Damage > 0)
```

* Since the Property and Crop damages are in terms of amount value, a variables are extracted to include the exponential values (e.g. K = thousands, M = Million, and B = Billion). In order to accomplish this, the Property and Crop expo variables were converted to character, then to numeric which were eventually converted to its corresponding exponential values. similar method were employed where exponents extracted temporarily in a table which were eventually combined together using  the rbind function.
* The Property damage table was cleaned first. Afterward this table was use as a base table to clean the crop damages table. The result is a FINAL table: stormProperty_Crop_Value_. This table was used to extract the top 10 Event Type that caused the most expensive Property and Crop damages. 


```r
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

Crop_Damage <- stormProperty_Crop_Value_ %>%
                                group_by(Event_Type) %>%
                                mutate(Crop_Value = Crop_Damage * Crop_Expo) %>%
                                summarise(Crop_Value = sum(Crop_Value), n = n()) %>%
                                arrange(Crop_Value = desc(Crop_Value))
```


***

## Results
                
### Discussions
The (NOAA) storm database originally consist of 902297 rows with 8 variables. Of these 8, only the following were extracted to answer the question presented on this analysis:

* The following were the variables/columns:


```
## [1] "State"           "Event_Type"      "Fatalities"      "Injuries"       
## [5] "Property_Damage" "Property_Expo"   "Crop_Damage"     "Crop_Expo"
```

1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

* top 10  Event type that caused the most Fatalities and Injuries were stored in a table separately. This decision is made in order to show more detailed effect of these event type to both Fatalities and Injuries. 
        
- Top 10 Event Type that caused the most Fatalities from 1950 to 2011:

```r
top_Population_Health_Fatalities <- Population_Health_Fatalities[c(1:10),]
top_Population_Health_Fatalities
```

```
## Source: local data frame [10 x 3]
## 
##    Event_Type Fatalities     n
##         (chr)      (dbl) (int)
## 1     Tornado       5230  1379
## 2        Heat        501    85
## 3        Wind        435   317
## 4      Wintry        387   232
## 5       Flood        289   138
## 6   Lightning        283   263
## 7  Sea Mishap        166   112
## 8        Rain         93    52
## 9        Fire         65    21
## 10  Hurricane         54    10
```

- Top 10 Event Type that caused the most injuries from 1950 to 2011:

```r
top_Population_Health_Injuries <- Population_Health_Injuries[c(1:10),]
top_Population_Health_Injuries
```

```
## Source: local data frame [10 x 3]
## 
##     Event_Type Injuries     n
##          (chr)    (dbl) (int)
## 1      Tornado    60226  1379
## 2         Heat     6495    85
## 3       Wintry     3952   232
## 4        Flood     3358   138
## 5    Hurricane     1280    10
## 6         Wind     1257   317
## 7    Lightning      649   263
## 8         Rain      527    52
## 9         Fire      427    21
## 10 Storm Surge      383    18
```
        
2. Across the United States, which types of events have the greatest economic consequences?
  
* top 10  Event type that caused the most amount of Property Damages and Crop damages were stored in a table separately. This decision is made in order to show more detailed effect of these event type to both Property Damages and Crop damages. 

- Top 10 Event type that caused the most Property damages from 1950 to 2011:

```r
top_Property_Damage <- Property_Damage[c(1:10),]
top_Property_Damage
```

```
## Source: local data frame [10 x 3]
## 
##     Event_Type Property_Value     n
##          (chr)          (dbl) (int)
## 1        Flood   125030972692  3577
## 2    Hurricane    34565803000    84
## 3       Others    10098371300   337
## 4         Wind     3234596960  4251
## 5      Tornado     1987231000  1171
## 6         Hail     1964381590  6494
## 7         Fire     1474023000    69
## 8  Storm Surge      865770400    56
## 9       Wintry      353230500    94
## 10        Rain      163087500    58
```

- Top 10 Event type that caused the most Crop damages from 1950 to 2011:

```r
top_Crop_Damage <-  Crop_Damage[c(1:10),]
top_Crop_Damage
```

```
## Source: local data frame [10 x 3]
## 
##     Event_Type Crop_Value     n
##          (chr)      (dbl) (int)
## 1       Others 6617216750   337
## 2        Flood 5562271400  3577
## 3    Hurricane 5298057800    84
## 4       Wintry 5296804700    94
## 5         Heat  493235000     7
## 6  Storm Surge  453265000    56
## 7      Tornado  398705950  1171
## 8         Fire  201631100    69
## 9         Rain   30975800    58
## 10   Lightning    4979150    38
```
        
### Plots
Show at least 3 figures Your analysis must have no more than three figures. Figures may have multiple plots in them (i.e. panel plots), but there cannot be more than three figures total.
        

```r
meltPopulation_Health <- melt(StormDataDF_Fatalities_Injury)
plot <- qplot(log(value), data = meltPopulation_Health, facets = Event_Type~., binwidth = 5, fill=State)
```
### Appendix
The following list of Event Type: categorized.

- Storm Surge

```r
Event_Type_Storm_Surge
```

```
##   Storm Surge Category
## 1          storm surge
## 2        coastal storm
## 3           dust storm
## 4       tropical storm
```
- Flood

```r
Event_Type_Flood
```

```
##         Flood Category
## 1             flooding
## 2                flood
## 3          flash flood
## 4    flood/flash flood
## 5 urban/sml stream fld
```
- Tornado

```r
Event_Type_Tornado
```

```
##     Tornado Category
## 1            tornado
## 2 waterspout/tornado
```
- Wintry

```r
Event_Type_Wintry
```

```
##              Wintry Category
## 1                  ice storm
## 2                        ice
## 3                  black ice
## 4               winter storm
## 5              winter storms
## 6    winter storm high winds
## 7         winter weather/mix
## 8                       cold
## 9                thundersnow
## 10                heavy snow
## 11              blowing snow
## 12                      snow
## 13                 rain/snow
## 14                light snow
## 15             freezing rain
## 16         extreme windchill
## 17                  blizzard
## 18              extreme cold
## 19                 icy roads
## 20                 avalanche
## 21            winter weather
## 22   extreme cold/wind chill
## 23 fog and cold temperatures
## 24          freezing drizzle
## 25           cold/wind chill
```
- Rain

```r
Event_Type_Rain
```

```
##        Rain Category
## 1               rain
## 2         heavy rain
## 3 excessive rainfall
## 4          dense fog
## 5                fog
```
- Lightning

```r
Event_Type_Lightning
```

```
##   Lightning Category
## 1          lightning
```
- Wind

```r
Event_Type_Wind
```

```
##         Wind Category
## 1                wind
## 2           tstm wind
## 3           high wind
## 4     high winds/snow
## 5         strong wind
## 6        strong winds
## 7  thunderstorm winds
## 8  marine strong wind
## 9          high winds
## 10  thunderstorm wind
```
- Hurricane

```r
Event_Type_Hurricane
```

```
##      Hurricane category
## 1             hurricane
## 2               typhoon
## 3     hurricane/typhoon
## 4 tropical storm gordon
```
- Heat

```r
Event_Type_Heat
```

```
##       Heat Category
## 1              heat
## 2         heat wave
## 3    excessive heat
## 4 heat wave drought
```
- Hail

```r
Event_Type_Hail
```

```
##    Hail Category
## 1           hail
## 2 tstm wind/hail
```
- Fire

```r
Event_Type_Fire
```

```
##      Fire Category
## 1       wild fires
## 2 wild/forest fire
## 3         wildfire
```
- Sea Mishap

```r
Event_Type_Sea_Mishap
```

```
##         Sea Mishap Category
## 1               rip current
## 2                rough surf
## 3                   tsunami
## 4                 high surf
## 5           marine accident
## 6  marine thunderstorm wind
## 7             marine mishap
## 8              rip currents
## 9        high wind and seas
## 10               heavy surf
## 11     heavy surf/high surf
```

- Others (top 10)

```r
Event_Type_Others
```

```
## Source: local data frame [822 x 2]
## 
##     Others category     n
##               (chr) (int)
## 1      funnel cloud  6844
## 2  marine tstm wind  6175
## 3        waterspout  3796
## 4           drought  2488
## 5      frost/freeze  1343
## 6    flash flooding   682
## 7     coastal flood   656
## 8  lake-effect snow   636
## 9         landslide   600
## 10      marine hail   442
## ..              ...   ...
```

