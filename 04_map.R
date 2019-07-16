#=================
# INSTALL PACKAGES
#=================
library(tidyverse)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)

# import data
master <- read.csv("./data/lit_review_master_20190716.csv", header = TRUE)
#
unique(master$World.region)

# Restoration/Reforestation by region
master %>%
  select(World.region, Total.Area, Restoration.Start.Year, 
         X.13..Afforestation..active.planting.in.non.forest.ecosystem.e.g..natural.grasslands.,
         latitude, longitude) -> df

# change variable names because they are long
names(df)[1] <- paste("region")
names(df)[2] <- paste("area")
names(df)[3] <- paste("begin.year")
names(df)[4] <- paste("afforestation")

# descriptor
df$type[df$afforestation == 0 ] <- "Reforestation"
df$type[df$afforestation == 1 ] <- "Afforestation"


#==============
# GET WORLD MAP
#==============

map.world <- map_data("world")


# loading the required packages
library(ggplot2)
library(ggmap)



library(ggplot2)
library(dplyr)

WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify

# df <- data.frame(region=c('Hungary','Lithuania','Argentina'), 
#                  value=c(4,10,11), 
#                  stringsAsFactors=FALSE)
x11(width = 10, height = 6)
ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "#7f7f7f", size=0.5)+ 
  geom_point(data = df, 
             aes(x = longitude, y = latitude, size = area, color = type), alpha=I(0.7))+
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))+
  scale_y_continuous(breaks=c())+
  scale_x_continuous(breaks=c())+
  labs(fill="legend", title="", x="", y="", size = "Area (Ha)", color = " ")+
  theme_bw()
 
 