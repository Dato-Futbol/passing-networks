# loading or installing packages
if(!require(StatsBombR)){devtools::install_github("statsbomb/StatsBombR")} 
library(StatsBombR)
if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)

# get free Statsbomb data competitions
comps <- FreeCompetitions()

# get all free games data from Men World Cup Rusia 2018 (id = 43)
WC_Matches <- comps %>% 
        filter(competition_id == 43) %>%
        FreeMatches() %>%
        arrange(match_date)

# get all free events data from those games
WC_events <- StatsBombFreeEvents(MatchesDF = WC_Matches)

# apply clean functions from StatsbombR package to get parsed coordinates, possesion info, etc.
WC_events_clean <- WC_events %>%
                   allclean()

# save data into "data" folder in .RDS format
saveRDS(WC_events_clean, "data/WC_events_clean.RDS")