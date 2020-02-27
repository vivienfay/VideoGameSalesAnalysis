library(tidyverse)
library(MASS)
rm(list = ls())

dat <- read.csv("Video_Game_Sales_as_of_Jan_2017.csv", stringsAsFactors = F)

# Select only NA_sales from the sales
dat <- dat %>% 
  dplyr::select(-EU_Sales, -JP_Sales, -Other_Sales, -Global_Sales) %>% 
  dplyr::filter(Rating != "AO" & Rating != "K-A" & Rating != "" & Rating != "EC") %>% 
  dplyr::filter(Name != "Wii Sports")  %>% # Sold with console
  dplyr::filter(NA_Sales > 0.1) # to match description)

###############################################################

# Remove observations played on very rare platforms
dat <- dat[!(dat$Platform %in% c("SCD", "WS","NG","TG16","3DO","GG","PCFX","GEN", 
                                 "NES", "SNES", "DC")), ]

# Group Platforms into Brand
# Group Rating into two categories
# Group Genre into bigger Genre
dat2 <- dat %>%
  mutate(
    Brand = case_when(
      Platform %in% c("PS", "PS2", "PS3", "PS4","PSP", "PSV") ~ "Sony",
      Platform %in%  c("X360", "XOne", "X") ~ "Xbox",
      Platform %in% c("Wii", "WiiU","GB", "GBA", "GC", "DS", "3DS", "G") ~ "Nintendo",
      Platform %in% c("PC") ~ "PC"
    ) 
  ) %>%
  mutate(
    Audience = case_when(
      Rating %in% c("E", "E10+") ~ "Family",
      Rating %in% c("T", "M") ~ "Mature"
    ) 
  ) %>% 
  mutate(
    Category = case_when(
      Genre %in% c("Action", "Shooter", "Fighting", "Platform", 
                   "Adventure", "Sports", "Racing") ~ "Action",
      Genre %in% c("Role-Playing", "Puzzle", "Strategy", "Simulation", "Misc") ~ "Other"
    )
  )


# Filter out observations with null values in Crtic_Score & User_Score
dat3 <- dat2[!is.na(dat2$User_Score),]
dat3 <- dat3[!is.na(dat3$Critic_Score),]

# Remove unnecessary columns
dat3 <- dat3 %>% dplyr::select(-Genre, -Rating)

# Remove the name column
namez <- paste(dat3$Name, dat3$Platform, dat3$Year_of_Release, sep = " ")  
dat4 <- dat3
rownames(dat4) <- namez
dat4[,1:3] <- NULL


# Reint
dat4 <- dat4 %>% dplyr::select(-Publisher)
dat4$Category <- as.factor(dat4$Category)
dat4$Audience <- as.factor(dat4$Audience)
dat4$Brand <- as.factor(dat4$Brand)

# Boxcox transformation, ignored shifting
# Used lambda = -1/2
fit.swag <- lm(NA_Sales~User_Count+Critic_Count+Critic_Score+User_Score+
                 Audience+Category+Brand, data = dat4)
boxcox(fit.swag)
mean(dat4$NA_Sales)^2
var(dat4$NA_Sales)
plot(fit.swag, which = 1)


# dat 5 with new transformed Sales
# Create dummy variables for collinearity checks
library(fastDummies)
dat5 <- fastDummies::dummy_cols(dat4, 
            select_columns = c("Category", "Audience", "Brand")) %>% 
  dplyr::select(-Category, -Audience, -Brand, 
                -Audience_Family, -Brand_Xbox, -Category_Other)
lm.2 <- lm(log(NA_Sales)~., data = dat5)
vif(lm.2)
ggplot(data = dat4) +
  geom_point(aes(x = Brand, y = log(NA_Sales), color = Category))
plot(lm.2, which = 2)
plot(lm.2x, which = 1)
plot(lm.2, which = 4)
print(cor(dat5),3)

# No Category after VIF and use interaction terms to fix variance 
# Removed 5-way interaction because of alias
lm.4 <-  lm(log(NA_Sales) ~ Critic_Score*User_Score*Critic_Count*Audience*
              Brand - Critic_Score:User_Score:Critic_Count:Audience:Brand,
            data = dat4)
al2 <- alias(lm.4)
plot(lm.4, which = 1)
plot(lm.4, which = 2)
summary(lm.4)

# Model Selection
n <- nrow(dat4)
fit.e <- lm(log(NA_Sales)~1, data = dat4)
scp <- list(lower = ~ 1, upper = ~ Critic_Score*User_Score*Critic_Count*Audience*
              Brand - Critic_Score:User_Score:Critic_Count:Audience:Brand)
print("backwards")
fit.back <- step(lm.4, scope = scp, direction = "backward", k = log(n))
print("forwards")
fit.forward <- step(fit.e, scope = scp, direction = "forward", k = log(n))
