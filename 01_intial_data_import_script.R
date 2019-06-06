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

#
unique(master$World.region)

# Restoration/Reforestation by region
master %>%
  select(World.region, Total.Area..hectares., Ending.Year) -> df
  
# change variable names because they are long
names(df)[1] <- paste("region")
names(df)[2] <- paste("area")
names(df)[3] <- paste("end.year")

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
  select(area, region.code, end.year) %>%
  filter(end.year < 2011) %>%
  group_by(region.code) %>%
  summarize(total_area = sum(area, na.rm = TRUE)) -> df.sums.10

df.sums.10$time.period <- "2000-2010"
df.sums.10$x <- as.factor("a")
print(df.sums.10)


######
# 2011 to 2019
# making summary stats
df %>%
  select(area, region.code, end.year) %>%
  filter(end.year >= 2011 & end.year < 2014) %>%
  group_by(region.code) %>%
  summarize(total_area = sum(area, na.rm = TRUE)) -> df.sums.11

df.sums.11$time.period <- "2011-2013"
df.sums.11$x <- as.factor("a")
print(df.sums.11)

# 2014-2019
# making summary stats
df %>%
  select(area, region.code, end.year) %>%
  filter(end.year >= 2014) %>%
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


  
