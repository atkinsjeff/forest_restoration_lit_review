# plots

df.sums <- read.csv("./data/restoration_total_area_clean.csv")
df.sums$time.period <- factor(df.sums$time.period, levels = c("2000-2010", "2011-2013", "2014-2019", "2000-2019"))
df.sums$total.area <- df.sums$total.area / 1000000

df.sums$area <- round(df.sums$total.area, digits = 2)



# bar plot
x11()
ggplot(df.sums, aes(x = region.code, y = total.area))+
  geom_bar(stat = "identity", color = "black", fill = "dodgerblue")+
  theme_classic()+
  xlab("")+
  ylab("Increase in Forest Area (Ha) since 2000")

#########
# bar plot
x11(height = 6, width = 10)
ggplot(df.sums, aes(x = region.code, y = area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  theme_bw()+
  xlab("")+
  ylab("Increase in Forest Area (Millions of Ha)")+
  scale_y_continuous(breaks=seq(0, 80, 10))+
  theme(legend.position="none")+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust=1, size = 10),
        axis.text.y = element_text(size = 12))+
  facet_grid(. ~ time.period)

theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))


df.sums %>%
  filter(time.period == "2011-2013") -> df.sums2

x11(width = 6, height = 6)
ggplot(df.sums2, aes(x = region.code, y = total.area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  geom_text(aes(label = area), hjust = -0.5) +
  theme_bw()+
  xlab("")+
  coord_flip(ylim = c(0, 2))+
  ylab("Increase in Forest Area (Millions of Ha)")+
  theme(axis.text.x  = element_text(angle = 90, vjust=0.5, hjust=1, size = 10),
        axis.text.y = element_text(size = 12))+
  theme(legend.position="none")

df.sums %>%
  filter(time.period == "2014-2019") -> df.sums3

x11(width = 6, height = 6)
ggplot(df.sums3, aes(x = region.code, y = area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  geom_text(aes(label = area), hjust = -0.5) +
  theme_bw()+
  xlab("")+
  coord_flip(ylim = c(0, 2))+
  # ylim(0,2)+
  ylab("Increase in Forest Area (Millions of Ha)")+
  theme(axis.text.x  = element_text(angle = 90, hjust=1, vjust=0.5, size = 10),
        axis.text.y = element_text(size = 12),
        axis.ticks = element_blank())+
  theme(legend.position="none")

df.sums %>%
  filter(time.period == "2000-2019") -> df.sums4



######
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
  theme(axis.text.x  = element_text(angle=90, hjust=1, vjust=0.5,  size = 10),
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

#################
## bringing in area by ECOSYSTEM
df.eco <- read.csv("./data/area_totals_by_ecosystem.csv")

df.eco$time.period <- factor(df.eco$time.period, levels = c("2000-2010", "2011-2013", "2014-2019", "2000-2019"))
df.eco$area <- df.eco$total.area / 1000000

df.eco$area <- round(df.eco$area, digits = 2)
# bar plot
x11(height = 6, width = 10)
ggplot(df.eco, aes(x = ecosystem, y = area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  theme_bw()+
  xlab("")+
  ylab("Increase in Forest Area (Millions of Ha)")+
  scale_y_continuous(breaks=seq(0, 20, 5))+
  ylim(c(0,20))+
  theme(legend.position="none")+
  theme(axis.text.x  = element_text(angle=90, hjust=1, vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 12))+
  facet_grid(. ~ time.period)

# REFORESTED
x11(height = 6, width = 10)
ggplot(df.eco, aes(x = ecosystem, y = reforestation / 1000000, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  theme_bw()+
  xlab("")+
  ylab("Increase in Forest Area (Millions of Ha) from Reforestation")+
  scale_y_continuous(breaks=seq(0, 20, 5))+
  ylim(c(0,20))+
  theme(legend.position="none")+
  theme(axis.text.x  = element_text(angle=90, hjust=1, vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 12))+
  facet_grid(. ~ time.period)

# AFFORESTATION
x11(height = 6, width = 10)
ggplot(df.eco, aes(x = ecosystem, y = afforestation.area / 1000000, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  theme_bw()+
  xlab("")+
  ylab("Increase in Forest Area (Millions of Ha) from Afforestation")+
  scale_y_continuous(breaks=seq(0, 20, 5))+
  ylim(c(0,20))+
  theme(legend.position="none")+
  theme(axis.text.x  = element_text(angle=90, hjust=1, vjust = 0.5, size = 10),
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
  theme(axis.text.x  = element_text(angle = 90, hjust=1, vjust = 0.5, size = 10),
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
  theme(axis.text.x  = element_text(angle = 90, hjust=1, vjust = 0.5,  size = 10),
        axis.text.y = element_text(size = 12),
        axis.ticks = element_blank())+
  theme(legend.position="none")



##########################
# MOTIVE
#
##########################
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
  scale_y_continuous(breaks=seq(0, 20, 5))+
  ylim(c(0,20))+
  theme(legend.position="none")+
  theme(axis.text.x  = element_text(angle=90, hjust=1, vjust = 0.5, size = 10),
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
  theme(axis.text.x  = element_text(angle = 90, hjust=1, vjust = 0.5, size = 10),
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
  theme(axis.text.x  = element_text(angle = 90, hjust=1, vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 12),
        axis.ticks = element_blank())+
  theme(legend.position="none")


####################
# prior disturbance
#
################
#### REFORESTATION
############################################
## bringing in area by prior
df.pr <- read.csv("./data/area_totals_by_prior.csv")

df.pr$time.period <- factor(df.pr$time.period, levels = c("2000-2010", "2011-2013", "2014-2019", "2000-2019"))
df.pr$area <- df.pr$total.area / 1000000

df.pr$area <- round(df.pr$area, digits = 2)
df.pr$a.area <- round( (df.pr$afforest.area/1000000), digits = 2)
df.pr$r.area <- round( (df.pr$reforest.area/1000000), digits = 2)

# bar plot
x11(height = 6, width = 10)
ggplot(df.pr, aes(x = prior, y = area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  theme_bw()+
  xlab("")+
  ylab("Increase in Forest Area (Millions of Ha)")+
  scale_y_continuous(breaks=seq(0, 20, 5))+
  ylim(c(0,20))+
  theme(legend.position="none")+
  theme(axis.text.x  = element_text(angle=90, hjust=1, vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 12))+
  facet_grid(. ~ time.period)

#### subsetting to "2011-2013" and 2014-2019
### REFORESTATION

# bar plot
x11(height = 6, width = 10)
ggplot(df.pr, aes(x = prior, y = r.area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  theme_bw()+
  xlab("")+
  ylab("Increase in Forest Area (Millions of Ha) from Reforestation")+
  scale_y_continuous(breaks=seq(0, 20, 5))+
  ylim(c(0,20))+
  theme(legend.position="none")+
  theme(axis.text.x  = element_text(angle=90, hjust=1, vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 12))+
  facet_grid(. ~ time.period)

df.pr %>%
  filter(time.period == "2011-2013") -> df.pr2

x11(width = 6, height = 6)
ggplot(df.pr2, aes(x = prior, y = r.area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  geom_text(aes(label = r.area), hjust = -0.5) +
  theme_bw()+
  xlab("")+
  coord_flip(ylim = c(0, 2))+
  ylab("Increase in Forest Area (Millions of Ha) from Reforestation")+
  theme(axis.text.x  = element_text(angle = 90, hjust=1, vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 12))+
  theme(legend.position="none")

df.pr %>%
  filter(time.period == "2014-2019") -> df.pr3

x11(width = 6, height = 6)
ggplot(df.pr3, aes(x = prior, y = r.area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  geom_text(aes(label = r.area), hjust = -0.5) +
  theme_bw()+
  xlab("")+
  coord_flip(ylim = c(0, 2))+
  # ylim(0,2)+
  ylab("Increase in Forest Area (Millions of Ha)")+
  theme(axis.text.x  = element_text(angle = 90, hjust=1, vjust = 0.5,  size = 10),
        axis.text.y = element_text(size = 12),
        axis.ticks = element_blank())+
  theme(legend.position="none")


#### AFFORESATION
############################################

# bar plot
x11(height = 6, width = 10)
ggplot(df.pr, aes(x = prior, y = a.area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  theme_bw()+
  xlab("")+
  ylab("Increase in Forest Area (Millions of Ha) from Afforestation")+
  scale_y_continuous(breaks=seq(0, 20, 5))+
  ylim(c(0,20))+
  theme(legend.position="none")+
  theme(axis.text.x  = element_text(angle=90, hjust=1, vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 12))+
  facet_grid(. ~ time.period)


#### subsetting to "2011-2013" and 2014-2019
### REFORESTATION
df.pr %>%
  filter(time.period == "2011-2013") -> df.pr2

x11(width = 6, height = 6)
ggplot(df.pr2, aes(x = prior, y = a.area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  geom_text(aes(label = a.area), hjust = -0.5) +
  theme_bw()+
  xlab("")+
  coord_flip(ylim = c(0, 2))+
  ylab("Increase in Forest Area (Millions of Ha) from Afforestation")+
  theme(axis.text.x  = element_text(angle = 90, hjust=1, vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 12))+
  theme(legend.position="none")

df.pr %>%
  filter(time.period == "2014-2019") -> df.pr3

x11(width = 6, height = 6)
ggplot(df.pr3, aes(x = prior, y = a.area, fill = x))+
  geom_bar(stat = "identity", color = "black", size = 0.5)+
  geom_text(aes(label = a.area), hjust = -0.5) +
  theme_bw()+
  xlab("")+
  coord_flip(ylim = c(0, 2))+
  # ylim(0,2)+
  ylab("Increase in Forest Area (Millions of Ha) from Afforestation")+
  theme(axis.text.x  = element_text(angle = 90, hjust=1, vjust = 0.5,  size = 10),
        axis.text.y = element_text(size = 12),
        axis.ticks = element_blank())+
  theme(legend.position="none")

