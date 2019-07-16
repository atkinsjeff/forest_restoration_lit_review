# ghigher order analysis

require(ggplot2)
require(plyr)
require(dplyr)
require(tidyverse)
require(RColorBrewer)
require(scales)

# importing data
# master <- read.csv("./data/lit_review_master_20190605.csv", skip = 3, header = TRUE)
# 
# master <- read.csv("./data/lit_review_master_20190611.csv", skip = 3, header = TRUE)

master <- read.csv("./data/lit_review_master_20190714.csv", header = TRUE)
#
unique(master$World.region)

# Restoration/Reforestation by region
master %>%
  select(World.region, Total.Area, Restoration.Start.Year, Country, 
         X.13..Afforestation..active.planting.in.non.forest.ecosystem.e.g..natural.grasslands.,
         master[30:44]) -> df


master[c(12, 25, 19, 13, 57, 30:44)] -> df
# change variable names because they are long
names(df)[1] <- paste("region")
names(df)[2] <- paste("area")
names(df)[3] <- paste("begin.year")
names(df)[4] <- paste("country")
names(df)[5] <- paste("afforestation")
names(df)[6] <- paste("Tropical Moist Broad")
names(df)[7] <- paste("Tropical Dry Broad")
names(df)[8] <- paste("Tropical Conifer")
names(df)[9] <- paste("Temperate Broadleaf")
names(df)[10] <- paste("Temperate Conifer")
names(df)[11] <- paste("Boreal")
names(df)[12] <- paste("Tropical Grassland or Savanna")
names(df)[13] <- paste("Temperate Grassland or Savanna")
names(df)[14] <- paste("Flooded Grassland or Savanna")
names(df)[15] <- paste("Montane Grassland or Shrubland")
names(df)[16] <- paste("Tundra")
names(df)[17] <- paste("Mediterranean")
names(df)[18] <- paste("Desert")
names(df)[19] <- paste("Mangroves")
names(df)[20] <- paste("Wetlands")

df %>%
  filter(country == "China" | country == "China and Mongolia") %>%
  filter(afforestation == 1) -> df.china

df %>%
  select(country, area, afforestation ) %>%
  filter(afforestation == 1) -> df.a

