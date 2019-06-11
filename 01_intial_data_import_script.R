# 20190605  by J. W. Atkins (jwatkins6@vcu.edu)
# This script imports a .csv file that has been exported from a Google Sheet

#libraries
require(ggplot2)
require(plyr)
require(dplyr)
require(tidyverse)
require(RColorBrewer)

# importing data
master <- read.csv("./data/lit_review_master_20190605.csv", skip = 3, header = TRUE)

master <- read.csv("./data/lit_review_master_20190611.csv", skip = 3, header = TRUE)

#
unique(master$World.region)

# Restoration/Reforestation by region
master %>%
  select(World.region, Total.Area..hectares., Restoration.Start.Year) -> df
  
# change variable names because they are long
names(df)[1] <- paste("region")
names(df)[2] <- paste("area")
names(df)[3] <- paste("begin.year")

#fixing
df$region.code <- sub(" .*", "", df$region )


######
# 2000 to 2019
# making summary stats
df %>%
  select(area, region.code) %>%
  group_by(region.code) %>%
  summarize(total_area = sum(area, na.rm = TRUE)) -> df.sums.total

df.sums.total$time.period <- "2000-2019"
df.sums.total$x <- as.factor("b")
print(df.sums.total)

# bar plot
x11()
ggplot(df.sums, aes(x = region.code, y = total_area))+
  geom_bar(stat = "identity", color = "black", fill = "dodgerblue")+
  theme_classic()+
  xlab("")+
  ylab("Increase in Forest Area (Ha) since 2000")

######
# 2000 to 2010
# making summary stats
df %>%
  select(area, region.code, begin.year) %>%
  filter(begin.year < 2011) %>%
  group_by(region.code) %>%
  summarize(total_area = sum(area, na.rm = TRUE)) -> df.sums.10

df.sums.10$time.period <- "2000-2010"
df.sums.10$x <- as.factor("a")
print(df.sums.10)


######
# 2011 to 2019
# making summary stats
df %>%
  select(area, region.code, begin.year) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  group_by(region.code) %>%
  summarize(total_area = sum(area, na.rm = TRUE)) -> df.sums.11

df.sums.11$time.period <- "2011-2013"
df.sums.11$x <- as.factor("a")
print(df.sums.11)

# 2014-2019
# making summary stats
df %>%
  select(area, region.code, begin.year) %>%
  filter(begin.year >= 2014) %>%
  group_by(region.code) %>%
  summarize(total_area = sum(area, na.rm = TRUE)) -> df.sums.14

df.sums.14$time.period <- "2014-2019"
df.sums.14$x <- as.factor("a")
print(df.sums.14)


####
# merging plots
df.sums <- rbind(df.sums.10, df.sums.11, df.sums.14, df.sums.total)
df.sums$time.period <- as.factor(df.sums$time.period)

# reordering factors
df.sums$time.period <- factor(df.sums$time.period, levels = c("2000-2010", "2011-2013", "2014-2019", "2000-2019"))
# bar plot
x11()
ggplot(df.sums, aes(x = region.code, y = total_area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  theme_bw()+
  xlab("")+
  ylab("Increase in Forest Area (Ha)")+
  theme(legend.position="none")+
  facet_grid(. ~ time.period)



#
# Stephanie Roe [10:17 AM]
# Hey Jeff, were you able to update the R code yet?
#   Besides the # of hectares by year, by region, we also need the info by restoration type and ecosystem type (where we have binary code)
colnames(master)[colnames(master) == "Total.Area..hectares."] <- "area"  
colnames(master)[colnames(master) == "Restoration.Start.Year"] <- "begin.year"  

# (1) Tropical and Subtropical Moist Broadleaf Forests	
names(master)[30]<-"TropicBroadMoist"
master %>%
  filter(TropicBroadMoist == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(TropicBroadMoist == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(TropicBroadMoist == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(2) Tropical and Subtropical Dry Broadleaf Forests	
names(master)[31]<-"TropicBroadDry"

master %>%
  filter(TropicBroadDry == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(TropicBroadDry == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(TropicBroadDry == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))


#(3) Tropical and Subtropical Coniferous Forests	
names(master)[32]<-"TropicConifer"

master %>%
  filter(TropicConifer == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(TropicConifer == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(TropicConifer == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(4) Temperate Broadleaf and Mixed Forests	
names(master)[33]<-"TempBroadMixed"

master %>%
  filter(TempBroadMixed == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(TempBroadMixed == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(TempBroadMixed == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(5) Temperate Coniferous Forests	
names(master)[34]<-"TempConifer"

master %>%
  filter(TempConifer == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(TempConifer == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(TempConifer == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(6) Boreal Forests/Taiga	
names(master)[35]<-"Boreal"
master %>%
  filter(Boreal == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(Boreal == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(Boreal == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(7) Tropical and subtropical grasslands, savannas, and shrublands	
names(master)[36]<-"TropicGrass"

master %>%
  filter(TropicGrass == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(TropicGrass == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(TropicGrass == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(8) Temperate Grasslands, Savannas, and Shrublands	
names(master)[37]<-"TempGrass"

master %>%
  filter(TempGrass == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(TempGrass == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(TempGrass == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(9) Flooded Grasslands and Savannas	
names(master)[38]<-"FloodedGrass"

master %>%
  filter(FloodedGrass == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(FloodedGrass == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(FloodedGrass == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(10) Montane Grasslands and Shrublands	
names(master)[39]<-"MontaneGrass"

master %>%
  filter(MontaneGrass == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(MontaneGrass == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(MontaneGrass == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(11) Tundra	
names(master)[40]<-"Tundra"

master %>%
  filter(Tundra == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(Tundra == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(Tundra == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(12) Mediterranean Forests, Woodlands, and Scrub	
names(master)[41]<-"Med"

master %>%
  filter(Med == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(Med == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(Med == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(13) Deserts and Xeric Shrublands	
names(master)[42]<-"Desert"

master %>%
  filter(Desert == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(Desert == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(Desert == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(14) Mangroves	
names(master)[43]<-"Mangroves"

master %>%
  filter(Mangroves == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(Mangroves == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(Mangroves == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(15) Wetlands
names(master)[44]<- "Wetlands"

master %>%
  filter(Wetlands == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(Wetlands == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(Wetlands == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

###### RESTORATION TYPE
# (1) Natural regeneration	
names(master)[45]<- "natural"

master %>%
  filter(natural == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(natural == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(natural == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))
#(2) Assisted regeneration	
names(master)[46]<- "assist"

master %>%
  filter(assist == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(assist == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(assist == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))
#(3) Regeneration unspecified	
names(master)[47]<- "unspec"

master %>%
  filter(unspec == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(unspec == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(unspec == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(4) Active ecological restoration	
names(master)[48]<- "active"

master %>%
  filter(active == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(active == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(active == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))
#(5) Mixed timber species plantation	
names(master)[49]<- "mixtimber"

master %>%
  filter(mixtimber == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(mixtimber == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(mixtimber == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))
#(6) Monoculture timber plantation	
names(master)[50]<- "monotimber"

master %>%
  filter(monotimber == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(monotimber == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(monotimber == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))
#(7) Plantation and/or woodlot unspecified	
names(master)[51]<- "woodlot"

master %>%
  filter(woodlot == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(woodlot == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(woodlot == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))
#(8) Agroforestry (fruit, nut, oil palm, etc)	
names(master)[52]<- "agro"

master %>%
  filter(agro == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(agro == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(agro == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))
#(9) Multistrata agroforestry (e.g. shade grown crops)	
names(master)[53]<- "agrostrata"

master %>%
  filter(agrostrata == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(agrostrata == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(agrostrata == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(10 Tree intercropping agroforestry (trees in croplands >30%)	
names(master)[54]<- "intercrop"

master %>%
  filter(intercrop == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(intercrop == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(intercrop == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(11) Silvopastoral systems (trees in pasture >30%)	
names(master)[55]<- "silvopastor"

master %>%
  filter(silvopastor == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(silvopastor == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(silvopastor == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))
            
#(12) Transitional land use (agroforestry as a transitional phase towards ecological restoration)	
names(master)[56]<- "trans"

master %>%
  filter(trans == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(trans == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(trans == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))
#(13) Afforestation (active planting in non-forest ecosystem e.g. natural grasslands)
names(master)[57]<- "afforest"

master %>%
  filter(afforest == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(afforest == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(afforest == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))       
                                                                                                                                                                                                                                                                                                                               