# 20190605  by J. W. Atkins (jwatkins6@vcu.edu)
# This script imports a .csv file that has been exported from a Google Sheet

#libraries
require(ggplot2)
require(plyr)
require(dplyr)
require(tidyverse)
require(RColorBrewer)
require(scales)

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

df.sums <- data.frame(df.sums)
#write.csv(df.sums, "./data/restoration_total_area.csv")


# reordering factors
df.sums <- read.csv("./data/restoration_total_area.csv")
df.sums$time.period <- factor(df.sums$time.period, levels = c("2000-2010", "2011-2013", "2014-2019", "2000-2019"))
df.sums$total_area <- df.sums$total_area / 1000000

df.sums$area <- round(df.sums$total_area, digits = 2)

# bar plot
x11(height = 6, width = 10)
ggplot(df.sums, aes(x = region.code, y = total_area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  theme_bw()+
  xlab("")+
  ylab("Increase in Forest Area (Millions of Ha)")+
  scale_y_continuous(breaks=seq(0, 80, 10))+
  theme(legend.position="none")+
  theme(axis.text.x  = element_text(angle=90, hjust=1, size = 10),
        axis.text.y = element_text(size = 12))+
  facet_grid(. ~ time.period)

#### subsetting to "2011-2013" and 2014-2019

df.sums %>%
  filter(time.period == "2011-2013") -> df.sums2

x11(width = 6, height = 6)
ggplot(df.sums2, aes(x = region.code, y = total_area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  geom_text(aes(label = area), hjust = -0.5) +
  theme_bw()+
  xlab("")+
  coord_flip(ylim = c(0, 2))+
  ylab("Increase in Forest Area (Millions of Ha)")+
  theme(axis.text.x  = element_text(angle = 90, hjust=1, size = 10),
        axis.text.y = element_text(size = 12))+
  theme(legend.position="none")

df.sums %>%
  filter(time.period == "2014-2019") -> df.sums3

x11(width = 6, height = 6)
ggplot(df.sums3, aes(x = region.code, y = total_area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  geom_text(aes(label = area), hjust = -0.5) +
  theme_bw()+
  xlab("")+
  coord_flip(ylim = c(0, 2))+
  # ylim(0,2)+
  ylab("Increase in Forest Area (Millions of Ha)")+
  theme(axis.text.x  = element_text(angle = 90, hjust=1, size = 10),
        axis.text.y = element_text(size = 12),
        axis.ticks = element_blank())+
  theme(legend.position="none")

df.sums %>%
  filter(time.period == "2000-2019") -> df.sums4

#######################################
## bringing in area by restoration type
df.restore <- read.csv("./data/area_totals_by_restoration_type.csv")

names(df.restore)[1] <- paste("restorationtype")
df.restore$time.period <- factor(df.restore$time.period, levels = c("2000-2010", "2011-2013", "2014-2019", "2000-2019"))
df.restore$area <- df.restore$area / 1000000

 df.restore$area <- round(df.restore$area, digits = 2)
# bar plot
x11(height = 6, width = 10)
ggplot(df.restore, aes(x = restorationtype, y = area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  theme_bw()+
  xlab("")+
  ylab("Increase in Forest Area (Millions of Ha)")+
  scale_y_continuous(breaks=seq(0, 80, 10))+
  theme(legend.position="none")+
  theme(axis.text.x  = element_text(angle=90, hjust=1, size = 10),
        axis.text.y = element_text(size = 12))+
  facet_grid(. ~ time.period)


#### subsetting to "2011-2013" and 2014-2019

df.restore %>%
  filter(time.period == "2011-2013") -> df.restore2

x11(width = 6, height = 6)
ggplot(df.restore2, aes(x = restorationtype, y = area, fill = x))+
  geom_bar(stat = "identity", color = "black", fill = "#00BFC4", size = 0.5)+geom_text(aes(label = area), hjust = -0.5) +
  theme_bw()+
  xlab("")+
  coord_flip(ylim = c(0, 2))+
  ylab("Increase in Forest Area (Millions of Ha)")+
  theme(axis.text.x  = element_text(angle = 90, hjust=1, size = 10),
        axis.text.y = element_text(size = 12))+
  theme(legend.position="none")

df.restore %>%
  filter(time.period == "2014-2019") -> df.restore3

x11(width = 6, height = 6)
ggplot(df.restore3, aes(x = restorationtype, y = area, fill = x))+
  geom_bar(stat = "identity", color = "black", fill = "#00BFC4", size = 0.5)+
  geom_text(aes(label = area), hjust = -0.5) +
  theme_bw()+
  xlab("")+
  coord_flip(ylim = c(0, 2))+
  # ylim(0,2)+
  ylab("Increase in Forest Area (Millions of Ha)")+
  theme(axis.text.x  = element_text(angle = 90, hjust=1, size = 10),
        axis.text.y = element_text(size = 12),
        axis.ticks = element_blank())+
  theme(legend.position="none")

############################################
## bringing in area by ECOSYSTEM
df.eco <- read.csv("./data/area_totals_by_ecosystem.csv")

df.eco$time.period <- factor(df.eco$time.period, levels = c("2000-2010", "2011-2013", "2014-2019", "2000-2019"))
df.eco$area <- df.eco$area / 1000000

df.eco$area <- round(df.eco$area, digits = 2)
# bar plot
x11(height = 6, width = 10)
ggplot(df.eco, aes(x = ecosystem, y = area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  theme_bw()+
  xlab("")+
  ylab("Increase in Forest Area (Millions of Ha)")+
  scale_y_continuous(breaks=seq(0, 80, 10))+
  theme(legend.position="none")+
  theme(axis.text.x  = element_text(angle=90, hjust=1, size = 10),
        axis.text.y = element_text(size = 12))+
  facet_grid(. ~ time.period)

#### subsetting to "2011-2013" and 2014-2019

df.eco %>%
  filter(time.period == "2011-2013") -> df.eco2

x11(width = 6, height = 6)
ggplot(df.eco2, aes(x = ecosystem, y = area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  geom_text(aes(label = area), hjust = -0.5) +
  theme_bw()+
  xlab("")+
  coord_flip(ylim = c(0, 2))+
  ylab("Increase in Forest Area (Millions of Ha)")+
  theme(axis.text.x  = element_text(angle = 90, hjust=1, size = 10),
        axis.text.y = element_text(size = 12))+
  theme(legend.position="none")

df.eco %>%
  filter(time.period == "2014-2019") -> df.eco3

x11(width = 6, height = 6)
ggplot(df.eco3, aes(x = ecosystem, y = area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  geom_text(aes(label = area), hjust = -0.5) +
  theme_bw()+
  xlab("")+
  coord_flip(ylim = c(0, 2))+
  # ylim(0,2)+
  ylab("Increase in Forest Area (Millions of Ha)")+
  theme(axis.text.x  = element_text(angle = 90, hjust=1, size = 10),
        axis.text.y = element_text(size = 12),
        axis.ticks = element_blank())+
  theme(legend.position="none")

############################################
## bringing in area by motive
df.mot <- read.csv("./data/area_totals_by_motive.csv")

df.mot$time.period <- factor(df.mot$time.period, levels = c("2000-2010", "2011-2013", "2014-2019", "2000-2019"))
df.mot$area <- df.mot$area / 1000000

df.mot$area <- round(df.mot$area, digits = 2)
# bar plot
x11(height = 6, width = 10)
ggplot(df.mot, aes(x = motive, y = area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  theme_bw()+
  xlab("")+
  ylab("Increase in Forest Area (Millions of Ha)")+
  scale_y_continuous(breaks=seq(0, 80, 10))+
  theme(legend.position="none")+
  theme(axis.text.x  = element_text(angle=90, hjust=1, size = 10),
        axis.text.y = element_text(size = 12))+
  facet_grid(. ~ time.period)

#### subsetting to "2011-2013" and 2014-2019

df.mot %>%
  filter(time.period == "2011-2013") -> df.mot2

x11(width = 6, height = 6)
ggplot(df.mot2, aes(x = motive, y = area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  geom_text(aes(label = area), hjust = -0.5) +
  theme_bw()+
  xlab("")+
  coord_flip(ylim = c(0, 2))+
  ylab("Increase in Forest Area (Millions of Ha)")+
  theme(axis.text.x  = element_text(angle = 90, hjust=1, size = 10),
        axis.text.y = element_text(size = 12))+
  theme(legend.position="none")

df.mot %>%
  filter(time.period == "2014-2019") -> df.mot3

x11(width = 6, height = 6)
ggplot(df.mot3, aes(x = motive, y = area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  geom_text(aes(label = area), hjust = -0.5) +
  theme_bw()+
  xlab("")+
  coord_flip(ylim = c(0, 2))+
  # ylim(0,2)+
  ylab("Increase in Forest Area (Millions of Ha)")+
  theme(axis.text.x  = element_text(angle = 90, hjust=1, size = 10),
        axis.text.y = element_text(size = 12),
        axis.ticks = element_blank())+
  theme(legend.position="none")


############################################
## bringing in area by prior
df.pr <- read.csv("./data/area_totals_by_prior.csv")

df.pr$time.period <- factor(df.pr$time.period, levels = c("2000-2010", "2011-2013", "2014-2019", "2000-2019"))
df.pr$area <- df.pr$area / 1000000

df.pr$area <- round(df.pr$area, digits = 2)
# bar plot
x11(height = 6, width = 10)
ggplot(df.pr, aes(x = prior, y = area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  theme_bw()+
  xlab("")+
  ylab("Increase in Forest Area (Millions of Ha)")+
  scale_y_continuous(breaks=seq(0, 80, 10))+
  theme(legend.position="none")+
  theme(axis.text.x  = element_text(angle=90, hjust=1, size = 10),
        axis.text.y = element_text(size = 12))+
  facet_grid(. ~ time.period)

#### subsetting to "2011-2013" and 2014-2019

df.pr %>%
  filter(time.period == "2011-2013") -> df.pr2

x11(width = 6, height = 6)
ggplot(df.pr2, aes(x = prior, y = area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  geom_text(aes(label = area), hjust = -0.5) +
  theme_bw()+
  xlab("")+
  coord_flip(ylim = c(0, 2))+
  ylab("Increase in Forest Area (Millions of Ha)")+
  theme(axis.text.x  = element_text(angle = 90, hjust=1, size = 10),
        axis.text.y = element_text(size = 12))+
  theme(legend.position="none")

df.pr %>%
  filter(time.period == "2014-2019") -> df.pr3

x11(width = 6, height = 6)
ggplot(df.pr3, aes(x = prior, y = area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  geom_text(aes(label = area), hjust = -0.5) +
  theme_bw()+
  xlab("")+
  coord_flip(ylim = c(0, 2))+
  # ylim(0,2)+
  ylab("Increase in Forest Area (Millions of Ha)")+
  theme(axis.text.x  = element_text(angle = 90, hjust=1, size = 10),
        axis.text.y = element_text(size = 12),
        axis.ticks = element_blank())+
  theme(legend.position="none")




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
                                             

######## Motivation for restoration
master[,77] #first column that has motivation info

#(1) Ecosystem function
names(master)[77]<- "ef"

master %>%
  filter(ef == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(ef == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(ef == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))  

#(2) Water provision/quality	
names(master)[78]<- "h2o"

master %>%
  filter(h2o == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(h2o == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(h2o == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE)) 

#(3) Biodiversity and habitat recovery	
names(master)[79]<- "bio"

master %>%
  filter(bio == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(bio == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(bio == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))  


#(4) Ecological connectivity	
names(master)[80]<- "con"

master %>%
  filter(con == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(con == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(con == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))  

#(5) Elimination of exotic/ invasive species	
names(master)[81]<- "exo"

master %>%
  filter(exo == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(exo == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(exo == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))  

#(6) Carbon sequestration	
names(master)[82]<- "carb"

master %>%
  filter(carb == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(carb == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(carb == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE)) 

# (7) Soil fertility	
names(master)[83]<- "soil"

master %>%
  filter(soil == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(soil == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(soil == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE)) 

#(8) Reduce risks (erosion, windbreaks, flood control)
names(master)[84]<- "risk"

master %>%
  filter(risk == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(risk == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(risk == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))  

#(9) Reclamation (clean up and repair after extraction activities)
names(master)[85]<- "rec"

master %>%
  filter(rec == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(rec == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(rec == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))  

#(10) Regulatory
names(master)[86]<- "reg"

master %>%
  filter(reg == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(reg == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(reg == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(11) Payment for ecosystem service
names(master)[87]<- "pay"

master %>%
  filter(pay == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(pay == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(pay == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))  

#(12) Agroforestry/ commercial	
names(master)[88]<- "ag"

master %>%
  filter(ag == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(ag == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(ag == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))  

#(13) Local employment and enhacing livelihoods	
names(master)[89]<- "local"

master %>%
  filter(local == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(local == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(local == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))  

#(14) Recreation/ eco-tourism	
names(master)[90]<- "recreation"

master %>%
  filter(recreation == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(recreation == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(recreation == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE)) 

#(15) Other
names(master)[91]<- "other"

master %>%
  filter(other == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(other == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(other == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE)) 

########### previous land cover or disturbance
#(1) Agriculture unspecified	
master[,62] # first on
names(master)[62]<- "prev.ag"

master %>%
  filter(prev.ag == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.ag == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.ag == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE)) 

#(2) Cropland
names(master)[63]<- "prev.crop"

master %>%
  filter(prev.crop == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.crop == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.crop == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE)) 

#(3) Pasture/ Grazing lands	
names(master)[64]<- "prev.past"

master %>%
  filter(prev.past == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.past == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.past == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE)) 

#(4) Agro-silvo-pastoral system	
names(master)[65]<- "prev.comboag"

master %>%
  filter(prev.comboag == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.comboag == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.comboag == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE)) 


#(5) Shifting cultivation/fallow	
names(master)[66]<- "prev.shift"

master %>%
  filter(prev.shift == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.shift == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.shift == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE)) 

#(6) Aquaculture	
names(master)[67]<- "prev.aqua"

master %>%
  filter(prev.aqua == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.aqua == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.aqua == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE)) 


#(7) Tree monocrop/ Plantation	
names(master)[68]<- "prev.mono"

master %>%
  filter(prev.mono == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.mono == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.mono == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE)) 

#(8) Deforested (clear cut)	
names(master)[69]<- "prev.clear"

master %>%
  filter(prev.clear == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.clear == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.clear == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(9) Degraded (selective logging)	
names(master)[70]<- "prev.degrad"

master %>%
  filter(prev.degrad == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.degrad == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.degrad == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(10) Land used for extractive activities (e.g. mining, oil & gas)	
names(master)[71]<- "prev.extract"

master %>%
  filter(prev.extract == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.extract == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.extract == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(11) Fire	
names(master)[72]<- "prev.fire"

master %>%
  filter(prev.fire == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.fire == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.fire == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(12) Non-fire disturbance (e.g. landslide, hurricane)	(
names(master)[73]<- "prev.non"

master %>%
  filter(prev.non == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.non == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.non == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(13) Marginal land and/or abandoned land	
names(master)[74]<- "prev.margin"

master %>%
  filter(prev.margin == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.margin == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.margin == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))

#(14) Other
names(master)[75]<- "prev.other"

master %>%
  filter(prev.other == 1) %>%
  filter(begin.year < 2011) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.other == 1) %>%
  filter(begin.year >= 2011 & begin.year < 2014) %>%
  summarize(sum(area, na.rm = TRUE))

master %>%
  filter(prev.other == 1) %>%
  filter(begin.year >= 2014) %>%
  summarize(sum(area, na.rm = TRUE))
