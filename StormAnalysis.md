Tornado are the event with most impact on USA
=============================================

#Synopsis

This report describes how Tornadoes have a direct damage impact in both, population and economics, in the USA.

Based on the *Storm Data* database, which is an official publication of the **National Oceanic and Atmospheric Administration (NOAA)**, this reports makes a preliminar analysis in order to know which type of storm events are the most harmful, with respect to population damage, and economic impact (property and crop damage).

The events contained in the dataset are from years 1950 to 2011, although the more recent years are the more complete and rich, due to the lack in early years for collect data.


#Data Processing

As mentioned earlier, this dataset is a publication of the NOAA, but this report used a special dataset from the Reproducible Research course on Coursera, which can be downloaded [here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2).

Once downloaded, the following code reads the dataset, and show the names of the variables contained in it:


```r
storm <- read.csv("repdata-data-StormData.csv.bz2")
names(storm)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```


The dataset is composed by 902297 observations and 37 variables from all the states of USA across the years 1950-2011.

A new column is added, which contains the year of the observation as a factor, for use in later calculations.


```r
storm$YEAR <- strptime(storm$BGN_DATE, "%m/%d/%Y %H:%M:%S")$year + 1900
storm$YEAR <- as.factor(storm$YEAR)
```


There are 985 types of storm events, and the top 10 with more occurrences are:


```r
sort(table(storm$EVTYPE), decreasing = TRUE)[1:10]
```

```
## 
##               HAIL          TSTM WIND  THUNDERSTORM WIND 
##             288661             219940              82563 
##            TORNADO        FLASH FLOOD              FLOOD 
##              60652              54277              25326 
## THUNDERSTORM WINDS          HIGH WIND          LIGHTNING 
##              20843              20212              15754 
##         HEAVY SNOW 
##              15708
```


Although the above events are the ones with more occurrences, this report analyzes which events have most impact in population and economic damage.


###Population Health Damage

In order to analyze the type of event with most impact, a subset of the data is store as a new dataset, and the variables **FATALITIES** and **INJURIES** are summarized and grouped by the type of event.


```r
# New dataset
populationDMG <- storm[, c("EVTYPE", "YEAR", "STATE", "FATALITIES", "INJURIES")]

# Summarized data
population.damage <- aggregate(cbind(populationDMG$FATALITIES, populationDMG$INJURIES, 
    (populationDMG$FATALITIES + populationDMG$INJURIES)) ~ populationDMG$EVTYPE, 
    FUN = sum)
population.damage <- population.damage[order(population.damage[, 4], decreasing = T), 
    ]
names(population.damage) <- c("EVENT", "FATALITIES", "INJURIES", "TOTAL")
population.damage <- population.damage[1:10, ]
population.damage$EVENT <- reorder(population.damage$EVENT, sort(population.damage$TOTAL, 
    decreasing = F))
```



###Economic Impact

As the *Storm Data* documentation describes, the economic damage are contained in two columns, one for the Property Damage (**PROPDMG**), and the other for the Crop Damage (**CROPDMG**).

The values in this columns are associated with other two variables (**PROPDMGEXP** and **CROPDMGEXP**), which specifies if the value are stored as thousands, millions or billions of dolars.

Although there are other categories in the above columns, the *Storm Data* documentation does not specifies these categories, so this report only analyzes the values for thousands (represented as "**K**" or "**k**"), millions (represented as "**M**" or "**m**"), and billions (represented as "**B**" or "**b**").

This code creates a new dataset with the columns specified above, and make the necessary changes to the values, in order to use the same scale on it.


```r
econDMG <- storm[, c("EVTYPE", "YEAR", "STATE", "PROPDMG", "PROPDMGEXP", "CROPDMG", 
    "CROPDMGEXP")]
econDMG <- subset(econDMG, econDMG$PROPDMGEXP %in% c("b", "B", "m", "M", "k", 
    "K") | econDMG$CROPDMGEXP %in% c("b", "B", "m", "M", "k", "K"))

econDMG$PROPDMG[toupper(econDMG$PROPDMGEXP) == "M"] <- econDMG$PROPDMG[toupper(econDMG$PROPDMGEXP) == 
    "M"]/1000
econDMG$PROPDMG[toupper(econDMG$PROPDMGEXP) == "K"] <- econDMG$PROPDMG[toupper(econDMG$PROPDMGEXP) == 
    "K"]/1e+06
econDMG$CROPDMG[toupper(econDMG$CROPDMGEXP) == "M"] <- econDMG$CROPDMG[toupper(econDMG$CROPDMGEXP) == 
    "M"]/1000
econDMG$CROPDMG[toupper(econDMG$CROPDMGEXP) == "K"] <- econDMG$CROPDMG[toupper(econDMG$CROPDMGEXP) == 
    "K"]/1e+06
```


