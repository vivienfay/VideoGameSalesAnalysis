library(tidyverse)
library(MASS)
rm(list = ls())

dat <- read.csv("Video_Game_Sales_as_of_Jan_2017.csv", stringsAsFactors = F)

# Select only NA_sales from the sales
dat <- dat %>% 
  dplyr::select(-EU_Sales, -JP_Sales, -Other_Sales, -Global_Sales)
dat <- dat %>% 
  dplyr::filter(Rating != "AO" | Rating != "K-A")

###############################################################

# Remove observations played on very rare platforms
dat <- dat[!(dat$Platform %in% c("SCD", "WS","NG","TG16","3DO","GG","PCFX","GEN", "NES", "SNES", "DC")), ]

# Group Platforms into ConsoleCompany
dat2 <- dat %>%
  mutate(
    ConsoleCompany = case_when(
      Platform %in% c("PS", "PS2", "PS3", "PS4","PSP", "PSV") ~ "Sony",
      Platform %in%  c("X360", "XOne", "X") ~ "Xbox",
      Platform %in% c("Wii", "WiiU","GB", "GBA", "GC", "DS", "3DS", "G") ~ "Nintendo",
      Platform %in% c("PC") ~ "PC"
    )
)

# Group Rating into two categories
dat2 <- dat2 %>% 
  mutate(
    TargetAudience = case_when(
      Rating %in% c("E", "E10+") ~ "Children",
      Rating %in% c("T", "M") ~ "Teen & Up"
    )
  )

# Group Genre into bigger Genre
dat2 <- dat2 %>% 
  mutate(
    Category = case_when(
      Genre %in% c("Action", "Shooter", "Fighting", "Platform", "Adventure") ~ "Action",
      Genre %in% c("Role-Playing", "Puzzle", "Strategy", "Simulation") ~ "Big-Brain",
      Genre %in% c("Sports", "Racing") ~ "Sports",
      Genre %in% c("Misc") ~ "Misc"
    )
  )

# Filter out observations with null values in Crtic_Score & User_Score
dat3 <- dat2[!is.na(dat2$User_Score),]
dat3 <- dat3[!is.na(dat3$Critic_Score),]
dat3 <- dat3 %>% dplyr::select(-Genre, -Rating)

dat3$Sales <- dat3$NA_Sales
dat3 <- dplyr::select(dat3, -NA_Sales)
dat4 <- dat3[2:nrow(dat3),]

# Continue cleaning, by looking at residuals as well
dat4 <- dplyr::select(dat4, -Name)
dat4 <- filter(dat4, Sales > 0.1) # to match description

# Fix any collinearity issues
dat4 <- dat4 %>% dplyr::select(-Publisher)
dat4$Platform <- as.factor(dat4$Platform)
dat4$Year_of_Release <- as.factor(dat4$Year_of_Release)
dat4$Category <- as.factor(dat4$Category)
dat4$TargetAudience <- as.factor(dat4$TargetAudience)
dat4$ConsoleCompany <- as.factor(dat4$ConsoleCompany)
cooklm <- lm(Sales~., data = dat4)

# Boxcox transformation, ignored shifting
# Used lambda = -1/2
boxcox(cooklm)

# dat 5 with new transformed Sales
dat5 <- dat4
dat5$Sales <- sqrt(dat4$Sales)^(-1)
plot(dat5$Critic_Count, dat5$Sales)
plot(dat5$Critic_Score, dat5$Sales)
lm.2 <- lm(Sales~., data = dat5)
plot(lm.2, which = 2)
