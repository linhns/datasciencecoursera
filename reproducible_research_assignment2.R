library(ggplot2)
library(tidyr)
library(dplyr)

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, destfile = "storm_data.csv")
storm_dt <- read.csv("storm_data.csv")
colnames(storm_dt)
cols_to_keep <- c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP",
                  "CROPDMG", "CROPDMGEXP")
storm_dt <- storm_dt[cols_to_keep]
storm_dt <- storm_dt %>% 
    filter(EVTYPE != "?" | FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 0) %>%
    mutate(CROPDMGEXP = toupper(CROPDMGEXP), PROPDMGEXP = toupper(PROPDMGEXP))

PROPDMGMultiplier <-  c("\"\"" = 10^0,
                 "-" = 10^0, 
                 "+" = 10^0,
                 "0" = 10^0,
                 "1" = 10^1,
                 "2" = 10^2,
                 "3" = 10^3,
                 "4" = 10^4,
                 "5" = 10^5,
                 "6" = 10^6,
                 "7" = 10^7,
                 "8" = 10^8,
                 "9" = 10^9,
                 "H" = 10^2,
                 "K" = 10^3,
                 "M" = 10^6,
                 "B" = 10^9)

CROPDMGMultiplier <-  c("\"\"" = 10^0,
                 "?" = 10^0, 
                 "0" = 10^0,
                 "K" = 10^3,
                 "M" = 10^6,
                 "B" = 10^9)
storm_dt <- storm_dt %>% 
    mutate(CROPDMGEXP = CROPDMGMultiplier[CROPDMGEXP]) %>%
    mutate(PROPDMGEXP = PROPDMGMultiplier[PROPDMGEXP])

storm_dt$CROPDMGEXP <- ifelse(is.na(storm_dt$CROPDMGEXP), 1, storm_dt$CROPDMGEXP)
storm_dt$PROPDMGEXP <- ifelse(is.na(storm_dt$PROPDMGEXP), 1, storm_dt$PROPDMGEXP)
storm_dt <- storm_dt %>%
    mutate(prop_cost = PROPDMG * PROPDMGEXP, crop_cost = CROPDMG * CROPDMGEXP)

economy_damage <- storm_dt %>% 
    group_by(EVTYPE = as.factor(EVTYPE)) %>%
    summarise(prop_cost = sum(prop_cost), crop_cost = sum(crop_cost), 
              total_cost = sum(prop_cost) + sum(crop_cost)) %>%
    arrange(desc(total_cost))

human_damage <- storm_dt %>% 
    group_by(EVTYPE = as.factor(EVTYPE)) %>%
    summarise(fatalities = sum(FATALITIES), injuries = sum(INJURIES), 
              totals = sum(FATALITIES) + sum(INJURIES)) %>%
    arrange(desc(totals))

health_suffering <- human_damage[1:10,]
econ_suffering <- economy_damage[1:10,]

health_suffering <- health_suffering %>% gather(key = type, value = value, - EVTYPE)
econ_suffering <- econ_suffering %>% gather(key = type, value = value, - EVTYPE)


health_suffering %>% 
    ggplot(aes(x = reorder(EVTYPE, -value), y = value)) + 
    geom_bar(stat = "identity", aes(fill = type), position = "dodge") +
    labs(title = "Top 10 fatal weather events", x = "Event", y = "Count") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          plot.title.position = "panel")

econ_suffering %>% 
    ggplot(aes(x = reorder(EVTYPE, -value), y = value)) + 
    geom_bar(stat = "identity", aes(fill = type), position = "dodge") +
    labs(title = "Top 10 costly weather events", x = "Event", y = "Cost") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          plot.title.position = "panel")