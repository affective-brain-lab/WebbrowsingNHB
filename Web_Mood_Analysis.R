library(ggplot2)
library(lme4)
library(lmerTest)
library(readxl)

########### Load Data ###########
data1 <- read_excel("NHB_Study1_Long_Mood_Data.xlsx")
data1 <- subset(data1, Filter_5_Days_of_Data == 1) # 5 days of data submitted & Min Valence values per day
data1 <- subset(data1, Browse_in_Session_Check == 2) # Participants were included in mood analysis if they engaged in web searches during the experiment, allowing alignment with mood ratings.

data2 <- read_excel("NHB_Study_2.xlsx")
data2 <- subset(data2, Filter_3_min == 1) # Min 3 valence values.
data2 <- subset(data2, BROWSE_IN_SESSION_Filter == 1) # Participants were included in mood analysis if they engaged in web searches during the experiment, allowing alignment with mood ratings.

########### Study 1 Prep ###########
data1$Z_NRC_Neg    <- scale(data1$NRC_Neg) #  Mean daily negative emotion score of webpages browsed (z-scored)
data1$Z_NRC_Pos    <- scale(data1$NRC_Pos) #  Mean daily negative emotion score of webpages browsed (z-scored)
data1$Z_Happy_Pre  <- scale(data1$Happy_Pre) # Happiness rating before web-browsing (z-scored)
data1$Z_Happy_Post <- scale(data1$Happy_Post) # Happiness rating after web-browsing (z-scored)
data1$Z_Age        <- scale(data1$Age)

########### Study 1 Analysis ###########

model1 <- lmer(
  Z_NRC_Neg ~ Z_Happy_Pre + Z_Age + Gender +
    (1 + Z_Happy_Pre | ID),
  data = data1,
  REML = TRUE,
  control = lmerControl(optimizer = "bobyqa",
                        optCtrl = list(maxfun = 2e5))
)
summary(model1)

model2 <- lmer(
  Z_Happy_Post ~ Z_NRC_Neg + Z_Happy_Pre + Z_Age + Gender +
    (1 + Z_NRC_Neg + Z_Happy_Pre | ID),
  data = data1,
  REML = TRUE,
  control = lmerControl(optimizer = "bobyqa",
                        optCtrl = list(maxfun = 2e5))
)
summary(model2)

model3 <- lmer(
  Z_NRC_Pos ~ Z_Happy_Pre + Z_Age + Gender +
    (1 + Z_Happy_Pre | ID),
  data = data1,
  REML = TRUE,
  control = lmerControl(optimizer = "bobyqa",
                        optCtrl = list(maxfun = 2e5))
)
summary(model3)

model4 <- lmer(
  Z_Happy_Post ~ Z_NRC_Pos + Z_Happy_Pre + Z_Age + Gender +
    (1 + Z_NRC_Pos + Z_Happy_Pre | ID),
  data = data1,
  REML = TRUE,
  control = lmerControl(optimizer = "bobyqa",
                        optCtrl = list(maxfun = 2e5))
)
summary(model4)


########### Study 2 Prep ###########
# Z-score the variables
data2$happy_pre_z   <- scale(data2$Happy_Pre) # Happiness rating before web-browsing (z-scored)
data2$happy_post_z  <- scale(data2$Happy_Post)  # Happiness rating after web-browsing (z-scored)
data2$NRC_NEG_VAL_z <- scale(data2$NRC_NEG_VAL) # Mean negative emotion of webpages browsed (z-scored)
data2$NRC_POS_VAL_z <- scale(data2$NRC_POS_VAL) # Mean positive emotion of webpages browsed (z-scored)
data2$age_z         <- scale(data2$Age)

# Fit the linear models using the z-scored variables
model5 <- lm(NRC_NEG_VAL_z ~ happy_pre_z + Age + Gender, data = data2)
summary(model5)

model6 <- lm(happy_post_z ~ NRC_NEG_VAL_z + happy_pre_z + Age + Gender, data = data2)
summary(model6)

model7 <- lm(NRC_POS_VAL_z ~ happy_pre_z + Age + Gender, data = data2)
summary(model7)

model8 <- lm(happy_post_z ~ NRC_POS_VAL_z + happy_pre_z + Age + Gender, data = data2)
summary(model8)
