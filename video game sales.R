library(tidyverse)

dat <- read.csv("Video_Game_Sales_as_of_Jan_2017.csv", stringsAsFactors = F)

# Select only NA_sales from the sales
dat <- select(dat, -EU_Sales, -JP_Sales, -Other_Sales, -Global_Sales)

# Remove observations played on very rare platforms
dat <- dat[!(dat$Platform %in% c("SCD", "WS","NG","TG16","3DO","GG","PCFX","GEN", "NES", "SNES")), ]


dat2 <- dat %>%
  mutate(
    ConsoleCompany = case_when(
      Platform %in% c("PS", "PS2", "PS3", "PS4","PSP", "PSV") ~ "Sony",
      Platform %in%  c("XB", "X360", "XOne") ~ "Xbox",
      Platform %in% c("Wii", "WiiU","GB", "GBA", "GC", "DS", "3DS", "G") ~ "Nintendo",
      Platform %in% c("PC") ~ "PC"
    )
)

# Filter out observations with null values in Crtic_Score & User_Score
dat3 <- dat2[!is.na(dat2$User_Score),]
dat3 <- dat3[!is.na(dat3$Critic_Score),]

dat3$Sales <- dat3$NA_Sales
dat3 <- select(dat3, -NA_Sales)
dat4 <- dat3[2:nrow(dat3),]
hist(dat4$Sales, clim = c(0, 16), breaks = 1000)
boxplot(dat4$Sales)

# Continue cleaning
dat4 <- select(dat4, -Name)
cooklm <- lm(Sales~., data = dat4)
plot(cooklm, which = 4)

dat4 <- filter(dat4, Sales > 0.1)
