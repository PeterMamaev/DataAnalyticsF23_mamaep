library(modeest)

epi_data <- read.csv("EPI_data.csv")
head(epi_data)
attach(epi_data)
View(epi_data)
# Do with NA and without NA values, see what happens

modernepi_data <- read.csv("2010EPI_data.csv", skip=1)
head(modernepi_data)
attach(modernepi_data)
View(modernepi_data)
#nrow(modernepi_data)

# SLIDE 6

#Central tendency tells about how the group of data is clustered around
#the center value of the distribution.
#I check the class of every data to make sure it is numeric.
class(modernepi_data$AIR_E)
class(modernepi_data$WATER_E)

mean(modernepi_data$AIR_E, na.rm = TRUE)
mean(modernepi_data$WATER_E, na.rm = TRUE)

median(modernepi_data$AIR_E, na.rm = TRUE)
median(modernepi_data$WATER_E, na.rm = TRUE)

hist(modernepi_data$AIR_E)
hist(modernepi_data$AIR_E, breaks=10)
hist(modernepi_data$WATER_E)

mode = mfv(modernepi_data$AIR_E, na_rm = TRUE)
print(mode)
mode = mfv(modernepi_data$WATER_E, na_rm = TRUE)
print(mode)

#?dplyr::filter
#?stats::filter

boxplot(modernepi_data$AIR_E, na.rm = TRUE)
boxplot(modernepi_data$WATER_E, na.rm = TRUE)

## SLIDE 7
# Again - this is just a preliminary check to make sure I can make a boxplot with them
class(modernepi_data$NOX_pt)
class(modernepi_data$SO2_pt)

mean(modernepi_data$NOX_pt, na.rm = TRUE)
mean(modernepi_data$SO2_pt, na.rm = TRUE)

median(modernepi_data$NOX_pt, na.rm = TRUE)
median(modernepi_data$SO2_pt, na.rm = TRUE)

hist(modernepi_data$NOX_pt)
hist(modernepi_data$SO2_pt)

mode = mfv(modernepi_data$AIR_E, na_rm = TRUE)
print(mode)
mode = mfv(modernepi_data$WATER_E, na_rm = TRUE)
print(mode)

boxplotOZONE = boxplot(modernepi_data$OZONE_pt)
print(boxplotOZONE)

class(WQI_pt)
modernepi_data$WQI_pt <- as.numeric(WQI_pt)

class(modernepi_data$WQI_pt)

boxplotWQI = boxplot(modernepi_data$WQI_pt)
print(boxplotWQI)

modernepi_data$WQI_pt <- as.numeric(modernepi_data$WQI_pt)
##class(modernepi_data$WQI_pt)
##modernepi_data$WQI_pt <- data.frame(WQI_pt)
#View(modernepi_data$WQI_pt)
##WQI_pt <- as.numeric(modernepi_data$WQI_pt)

#  data.frame(DALYNEW, AIR_HNEW, WATER_HNEW)
#modernepi_data$WQI_pt <- filter(modernepi_data, modernepi_data$WQI_pt !="..")
#modernepi_datawqi <- as.numeric(as.character(modernepi_datawqi))
#modernepi_data$WQI_pt <- as.numeric(as.character(modernepi_data$WQI_pt))
#class(modernepi_datawqi)
#View(modernepi_datawqi)
#boxplot(modernepi_datawqi)

#SLIDE 8

# This is done to check the numerical nature of the values
class(modernepi_data$CLIMATE)
class(modernepi_data$AGRICULTURE)

mean(modernepi_data$CLIMATE, na.rm = TRUE)
mean(modernepi_data$AGRICULTURE, na.rm = TRUE)

median(modernepi_data$CLIMATE, na.rm = TRUE)
median(modernepi_data$AGRICULTURE, na.rm = TRUE)

hist(modernepi_data$CLIMATE)
hist(modernepi_data$AGRICULTURE)

# Multiple modes because a single number can appear multiple times as the mode
mode = mfv(modernepi_data$CLIMATE, na_rm = TRUE)
print(mode)
# Here, on the other hand, using the same method, there is only one most frequent number.
mode = mfv(modernepi_data$AGRICULTURE, na_rm = TRUE)
print(mode)

modernepi_fisheries <- filter(modernepi_data,FISHERIES !="..")
boxplot(modernepi_fisheries)
#boxplot(modernepi_data$FISHERIES_pt)
boxplot(modernepi_data$NMVOC_pt)

#SLIDE 9
boxplot(modernepi_data$ENVHEALTH, modernepi_data$ECOSYSTEM)

#SLIDE 10
qqplot(modernepi_data$ENVHEALTH, modernepi_data$ECOSYSTEM)

#SLIDE 12
boxplot(epi_data$ENVHEALTH, epi_data$DALY, epi_data$AIR_H, epi_data$WATER_H)
lmENVH <- lm(epi_data$ENVHEALTH ~ epi_data$DALY + epi_data$AIR_H + epi_data$WATER_H)
lmENVH
summary(lmENVH)
cENVH <- coef(lmENVH)

#SLIDE 13
DALYNEW <- c(seq(5, 95, 5))
# seq generates regular sequences
AIR_HNEW <- c(seq(5, 95, 5))
WATER_HNEW <- c(seq(5, 95, 5))
NEW <- data.frame(DALYNEW, AIR_HNEW, WATER_HNEW)
help(predict)
pENV <- predict(lmENVH, NEW, interval="prediction")
pENV
cENV <- predict(lmENVH, NEW, interval="confidence")
cENV

#SLIDE 14
boxplot(ENVHEALTH,DALY,AIR_E,WATER_H)
Model1 <- lm(epi_data$AIR_E ~ epi_data$DALY + epi_data$AIR_H + epi_data$WATER_H)
Model1
summary(Model1)
cModel1 <- coef(Model1)
AIR_ENEW <- c(seq(5, 95, 5))
help(seq)
NEW_AIRE <- data.frame(DALYNEW, AIR_ENEW, WATER_HNEW)
pENV <- predict(Model1, NEW_AIRE, interval = "prediction")
pENV
cENV <- predict(Model1, NEW_AIRE, interval = "confidence")
cENV

boxplot(ENVHEALTH,DALY,CLIMATE,WATER_H)
Model2 <- lm(epi_data$CLIMATE ~ epi_data$DALY + epi_data$AIR_H + epi_data$WATER_H)
Model2
summary(Model2)
cModel2 <- coef(Model2)
CLIMATE_NEW <- c(seq(5, 95, 5))
NEW_CLIMATE <- data.frame(DALYNEW, CLIMATE_NEW, WATER_HNEW)
pENV <- predict(Model2, NEW_CLIMATE, interval = "prediction")
pENV
cENV <- predict(Model2, NEW_CLIMATE, interval = "confidence")
cENV

# While the Shapiro-Wilks test is possible, the number of rows for
# 2010 EPI data is too high.
# As such we wouldn't receive a good assessment for its values.
nrow(modernepi_data)
# The dataset for epi_data is within acceptable size to do Shapiro-Wilks test on.
nrow(epi_data)

library(dplyr)

# data_sl <- sample(modernepi_data))
data_sl <- na.omit(modernepi_data$ENVHEALTH)
shapiro.test(data_sl)

# Omitting the NA values from the 2010 EPI data to compress it to a size where the Shapiro-Wilks can be performed.
data_sl2 <- na.omit(modernepi_data$ECOSYSTEM)
shapiro.test(data_sl2)



#data_sl <- sample()
#data_sl <- modernepi_data[sample(2:nrow(epi_data), 2000)]


shapiro.test(epi_data$ENVHEALTH)
shapiro.test(epi_data$ECOSYSTEM)
#View(modernepi_data$ENVHEALTH)

modernepi_data <- read.csv("2010EPI_data.csv", skip=1)
nrow(modernepi_data)
modernepi_data <- filter(modernepi_data, !is.na(EPI))
nrow(modernepi_data)
shapiro.test(modernepi_data$ENVHEALTH)
shapiro.test(modernepi_data$ECOSYSTEM)

