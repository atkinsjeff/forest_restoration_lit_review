# 20190605  by J. W. Atkins (jwatkins6@vcu.edu)
# This script imports a .csv file that has been exported from a Google Sheet

#libraries
require(ggplot2)
require(plyr)
require(dplyr)
require(tidyverse)

# importing data
master <- read.csv("./data/lit_review_master_20190605.csv", skip = 3, header = TRUE)

#
unique(master$World.region)

# Restoration/Reforestation by region
master %>%
  select(World.region, Total.Area..hectares.) -> df
  
# change variable names because they are long
names(df)[1] <- paste("region")
names(df)[2] <- paste("area")

#making a plot
x11() 
ggplot(df, aes(x = region, y = area))+
geom_boxplot(position = "dodge2")

# making summary stats
df %>%
  select(area, region) %>%
  group_by(region) %>%
  summarize(total_area = sum(area)) -> df.sums

x11()
ggplot(df.sums, aes(x = region, y = total_area))+
  geom_bar(stat = "bin")