library(tidyverse)
library(MASS)
library(corrplot)
library(tree)

rm(list = ls())

dat5 <- read.csv("dat_cleaned.csv", stringsAsFactors = T)
dat5$Year_of_Release <- as.factor(dat5$Year_of_Release)

lm.2 <- lm(Sales~., data = dat5)

# year is not a significant predictor at the 0.05 level
summary(lm.2)

# remove year and platform predictors
dat5 <- dat5 %>% 
  dplyr::select(-Year_of_Release, -Platform)

# remove observations with null values for TargetAudience
dat6 <- dat5[!is.na(dat5$TargetAudience), ]

lm.3 <- lm(Sales~., data = dat6)
# vif(lm.3)

## additional model diagnostics

# scatterplot and correlation matrix for continuous predictors
plot(dat6[, c(1, 2, 3, 4)])

# user and critic score appear to be moderately corelated
corrplot(cor(dat6[, c(1, 2, 3, 4)]))

# linear regression on continuous predictors only
lm.4 <- lm(Sales~ Critic_Score + User_Score + 
     Critic_Count + User_Count, data = dat5)
# variance inflation factors all less than 0.5
vif(lm.4)

# histograms of continuous predictors
hist(dat6$Critic_Score)
hist(dat6$User_Score)

hist(dat6$Critic_Count)
# user count significantly right skewed
hist(dat6$User_Count)

# consider log transformation of User_Count
plot(Sales ~ User_Count, dat = dat6)
plot(Sales ~ log(User_Count), dat = dat6)


## Regression Tree
tree.sales <- tree(Sales ~., data = dat6)
plot(tree.sales)
text(tree.sales, pretty = 0)