library(tidyverse)
library(readxl)
# df <- read.csv("DAFinalMegasheet.csv", sheet= 'Avg_Income_and_Tax_Liability')

df_tax = read_excel("DAFinalMegasheet.xlsx", 
           sheet = "Avg_Income_and_Tax_Liability")
df_crime = read_excel("DAFinalMegasheet.xlsx", 
                sheet = "Violent_Property_Firearm_Rates")

names(df_tax)[1] = 'Year'

df_merge <- merge(df_tax,df_crime,by=c("FIPS Code",'Year')) 
head(df_merge)

sum(is.na(df_merge['FIPS Code']))
sum(is.na(df_merge['Year']))
sum(is.na(df_merge['Place of Residence']))
sum(is.na(df_merge['Average NY AGI of All Returns']))
sum(is.na(df_merge['Average Tax of All Returns']))
sum(is.na(df_merge['Average NY AGI of Taxable Returns']))
sum(is.na(df_merge['Average Tax of Taxable Returns']))
sum(is.na(df_merge['Average NY AGI of Nontaxable Returns']))
sum(is.na(df_merge['County Sort Order']))
sum(is.na(df_merge['County.x']))
sum(is.na(df_merge['County.y']))
sum(is.na(df_merge['Population']))
sum(is.na(df_merge['Index Count']))
sum(is.na(df_merge['Index Rate']))
sum(is.na(df_merge['Violent Count']))
sum(is.na(df_merge['Violent Rate']))
sum(is.na(df_merge['Property Count']))
sum(is.na(df_merge['Property Rate']))
sum(is.na(df_merge['Firearm Count']))
sum(is.na(df_merge['Firearm Rate']))
# Only attributes with missing values are Firearm Count and Firearm Rate.
# There is a more efficient way to do this I'm sure, I just want to be thorough.
# The more efficient way in question:
null_sum_per_column <- colSums(is.na(df_merge))
null_sum_per_column

# Imputing missing values with the median of the column - not using the mean due to possibility of outliers.
df_merge$`Firearm Count` <- ifelse(is.na(df_merge$`Firearm Count`), median(df_merge$`Firearm Count`, na.rm = TRUE), df_merge$`Firearm Count`)
df_merge$`Firearm Rate` <- ifelse(is.na(df_merge$`Firearm Rate`), median(df_merge$`Firearm Rate`, na.rm = TRUE), df_merge$`Firearm Rate`)


dfmerge_column_types <- sapply(df_merge, class)
dfmerge_column_types
# All values except Place of Residence are "numeric", meaning we don't need to do further class conversion.

# Since df_merge is merged from two dataframes, there's potentially a few redundant variables we want to clear.
# The columns 'County.x' and 'County.y' are redundant as they show the county name, same as 'Place of Residence'.
# For convenience, I also want to rename 'Place of Residence' to 'County'.
diff_county <- setdiff(df_merge$`County.y`, df_merge$`County.x`)
diff_county2 <- setdiff(df_merge$`Place of Residence`, df_merge$`County.x`)
diff_county3 <- setdiff(df_merge$`Place of Residence`, df_merge$`County.y`)
# County.y has 'St Lawrence' which County.x and Place of Residence call "St. Lawrence".
# No major disparities aside from that, we can drop the two redundant County.x and County.y columns.
column_to_drop <- "County.x"
df_merge <- select(df_merge, -one_of(column_to_drop))
colnames(df_merge)[colnames(df_merge) == "Place of Residence"] ="County"
colnames(df_merge)[colnames(df_merge) == "FIPS Code"] ="FIPS"
#head(df_merge)


# Using Cook's Distance and IQR to identify outlier variables in violent crime and taxable income statistics.
# Extremely mega-rich places and a select few criminal hotspots should not skew the data.
# "Average NY AGI of All Returns" is my target variable for taxes.
# "Index Count" is the target variable for the Count of crimes.
# The rate of crime is measured per 100,000 people. 
indexcount <- df_merge$`Index Count`
indexrate <- df_merge$`Index Rate`
violentcount <- df_merge$`Violent Count`
violentrate <- df_merge$`Violent Rate`
propertycount <- df_merge$`Property Count`
propertyrate <- df_merge$`Property Rate`
firearmcount <- df_merge$`Firearm Count`
firearmrate <- df_merge$`Firearm Rate`
model_crimecount <- lm(indexcount ~ violentcount + propertycount + firearmcount, data=df_merge)
model_crimecount
#Coefficients:
#  (Intercept)   violentcount  propertycount   firearmcount  
#    2.148e-11      1.000e+00      1.000e+00     -8.507e-16 
cooksdistance_crimecount <- cooks.distance(model_crimecount)
influential <- cooksdistance[(cooksdistance_crimecount > (3 * mean(cooksdistance_crimecount, na.rm=TRUE)))]
influential_names <- names(influential)
outliers <- df_merge[influential_names,]
units_without_outliers <- df_merge %>% anti_join(outliers)
summary(units_without_outliers)
model_no_outliers <- lm(indexcount ~ violentcount + propertycount + firearmcount, data= units_without_outliers)
summary(model_no_outliers)

agi_all <- df_merge$`Average NY AGI of All Returns`
agi_tax <- df_merge$`Average NY AGI of Taxable Returns`
agi_nontax <- df_merge$`Average NY AGI of Nontaxable Returns`
model_agi <- lm(agi_all ~ agi_tax + agi_nontax + firearmcount, data=df_merge)
model_agi
#Coefficients:
#  (Intercept)       agi_tax    agi_nontax  firearmcount  
#  -1452.5749        0.7342        0.1279        0.5331   
cooksdistance_modelagi <- cooks.distance(model_agi)
influential_agi <- cooksdistance[(cooksdistance_modelagi > (3 * mean(cooksdistance_modelagi, na.rm=TRUE)))]
influential_agi_names <- names(influential_agi)
outliers_agi <- df_merge[influential_agi_names,]
units_without_outliers_agi <- df_merge %>% anti_join(outliers_agi)
model_no_outliers_agi <- lm(agi_all ~ agi_tax + agi_nontax + firearmcount, data= units_without_outliers_agi)
summary(model_no_outliers_agi)

iqr_index <- IQR(df_merge$`Index Count`)
print(iqr_index)
# 5225.25
iqr_agiall <- IQR(df_merge$`Average NY AGI of All Returns`)
print(iqr_agiall)
# 10969.5

#head(df_merge)

outlier_border <- 1.5

# Removing them for independent variables as I don't want to too severely skew my models by removing all anomaly from independent variables.
# I'm using indexcount since indexrate is a proportion of the same number. Extreme high outliers in indexcount could be extremely populous areas (i.e. big cities) skewing our distribution.
q1indexcount <- quantile(indexcount, 0.25)
q3indexcount <- quantile(indexcount, 0.75)
iqr_indexcount <- IQR(indexcount, na.rm = TRUE)
cleaned_data <- subset(df_merge, indexcount > (q1indexcount - 1.5*iqr_indexcount) & indexcount < (q3indexcount + 1.5*iqr_indexcount))
#view(cleaned_data)
nrow(cleaned_data)
nrow(df_merge)
# 840 rows in cleaned_data, 992 rows in df_merge, removing 152 outliers.
df_merge <- cleaned_data

# I'm choosing average NY AGI as it gives us the clearest metric for overall income - hence why I'm trying to remove mega-wealthy data.
agi_all <- df_merge$`Average NY AGI of All Returns`

q1agi_all <- quantile(agi_all, 0.25)
q3agi_all <- quantile(agi_all, 0.75)
iqr_agi_all <- IQR(agi_all, na.rm = TRUE)
cleaned_data_agi <- subset(df_merge, agi_all > (q1agi_all - 1.5*iqr_agi_all) & agi_all < (q3agi_all + 1.5*iqr_agi_all))
nrow(cleaned_data_agi)
nrow(df_merge)
# 786 rows in cleaned_data, 840 rows in df_merge, removing 54 outliers.
df_merge <- cleaned_data_agi


# Data visualization

# Re-incorporating the values after removing the outliers in dependent variables.
indexcount <- df_merge$`Index Count`
indexrate <- df_merge$`Index Rate`
violentcount <- df_merge$`Violent Count`
violentrate <- df_merge$`Violent Rate`
propertycount <- df_merge$`Property Count`
propertyrate <- df_merge$`Property Rate`
firearmcount <- df_merge$`Firearm Count`
firearmrate <- df_merge$`Firearm Rate`
agi_all <- df_merge$`Average NY AGI of All Returns`
agi_tax <- df_merge$`Average NY AGI of Taxable Returns`
tax_all <- df_merge$`Average Tax of All Returns`
tax_tax <- df_merge$`Average Tax of Taxable Returns`
agi_non <- df_merge$`Average NY AGI of Nontaxable Returns`
head(df_merge)

# qqplot
# scatterplot regression
# boxplot comparing spread of different crime rates and types of taxable income.
boxplot(agi_all, agi_tax, agi_non, data=df_merge, names=c("agi_all", "agi_taxable", "agi_nontax"), main="Box Plot", xlab="Variables", ylab="Distribution")

boxplot(tax_all, tax_tax,names=c("tax_all_returns", "tax_taxable"), main="Box Plot", xlab="Types of Taxed Income", ylab="Distribution")

# I prefer to compare the rates of crime rather than their counts because the rates are all measured per the 100,000 people. Hence we don't have to worry about massive outliers as much.
boxplot(violentrate, propertyrate, firearmrate, data=df_merge, names=c("violentrate", "propertyrate", "firearmrate"), main="Box Plot", xlab="Crime Rates", ylab="Distribution")
# The property crime rate is significantly higher than both the violent and firearm rate - makes sense, it's a lower-risk crime.
# One weakness that now comes to mind in this dataset, it does not bring up whether or not these crime rates overlap, or can happen to the same person.

# Crime counts for good measure.
boxplot(violentcount, propertycount, firearmcount, data=df_merge, names=c("violentcount", "propertycount", "firearmcount"), main="Box Plot", xlab="Crime Counts", ylab="Distribution")

boxplot(indexcount ~ violentcount, data=df_merge, main="Box Plot", xlab="Violent Crime Count", ylab="Crime Index Count")
boxplot(indexcount ~ propertycount, data=df_merge, main="Box Plot", xlab="Property Crime Count", ylab="Crime Index Count")
boxplot(indexcount ~ firearmcount, data=df_merge, main="Box Plot", xlab="Firearm Crime Count", ylab="Crime Index Count")



# Quantile-Quantile, ggplots and ECDF Plots correlating various variables.
# Their nature (hopefully) allows me to finally start correlating some of the variables between the two datasets.
library(ggplot2)

variable1 <- "Average NY AGI of All Returns"
variable2 <- "Index Count"

ggplot(df_merge, aes(x = .data[[variable1]], y = .data[[variable2]])) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear trendline
  labs(title = paste("Scatter Plot between", variable1, "and", variable2),
       x = variable1,
       y = variable2)

qqplot(df_merge[[variable1]], df_merge[[variable2]])
# Unsurprisingly, the qqplot's non-linear pattern predicts the data is not distributed in a normal manner.

# More elaborate syntax for an ECDF plot with coloring and labels taken from the internet.
ggplot(df_merge, aes(x = .data[[variable1]])) +
  stat_ecdf(geom = "step", color = "blue", size = 1) +
  geom_line(aes(x = .data[[variable2]], y = ..y..), stat = "ecdf", color = "green", size = 1) +
  labs(title = paste("ECDF Plot between", variable1, "and", variable2),
       x = variable1,
       y = "Cumulative Probability")




variable1 <- "Average NY AGI of All Returns"
variable2 <- "Index Rate"
ggplot(df_merge, aes(x = .data[[variable1]], y = .data[[variable2]])) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear trendline
  labs(title = paste("Scatter Plot between", variable1, "and", variable2),
       x = variable1,
       y = variable2)
# Much lesser linearity in the gg scatterplot.
qqplot(df_merge[[variable1]], df_merge[[variable2]])

ggplot(df_merge, aes(x = .data[[variable1]])) +
  stat_ecdf(geom = "step", color = "blue", size = 1) +
  geom_line(aes(x = .data[[variable2]], y = ..y..), stat = "ecdf", color = "green", size = 1) +
  labs(title = paste("ECDF Plot between", variable1, "and", variable2),
       x = variable1,
       y = "Cumulative Probability")


# Now just picking two random variables
variable1 <- "Average Tax of All Returns"
variable2 <- "Firearm Rate"
ggplot(df_merge, aes(x = .data[[variable1]], y = .data[[variable2]])) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear trendline
  labs(title = paste("Scatter Plot between", variable1, "and", variable2),
       x = variable1,
       y = variable2)
# Much lesser linearity in the gg scatterplot.
qqplot(df_merge[[variable1]], df_merge[[variable2]])
ggplot(df_merge, aes(x = .data[[variable1]])) +
  stat_ecdf(geom = "step", color = "blue", size = 1) +
  geom_line(aes(x = .data[[variable2]], y = ..y..), stat = "ecdf", color = "green", size = 1) +
  labs(title = paste("ECDF Plot between", variable1, "and", variable2),
       x = variable1,
       y = "Cumulative Probability")

variable1 <- "Average Tax of All Returns"
variable2 <- "Firearm Count"
ggplot(df_merge, aes(x = .data[[variable1]], y = .data[[variable2]])) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear trendline
  labs(title = paste("Scatter Plot between", variable1, "and", variable2),
       x = variable1,
       y = variable2)
# Much lesser linearity in the gg scatterplot.
qqplot(df_merge[[variable1]], df_merge[[variable2]])
ggplot(df_merge, aes(x = .data[[variable1]])) +
  stat_ecdf(geom = "step", color = "blue", size = 1) +
  geom_line(aes(x = .data[[variable2]], y = ..y..), stat = "ecdf", color = "green", size = 1) +
  labs(title = paste("ECDF Plot between", variable1, "and", variable2),
       x = variable1,
       y = "Cumulative Probability")

variable1 <- "Average Tax of All Returns"
variable2 <- "Property Rate"
ggplot(df_merge, aes(x = .data[[variable1]], y = .data[[variable2]])) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear trendline
  labs(title = paste("Scatter Plot between", variable1, "and", variable2),
       x = variable1,
       y = variable2)
# Much lesser linearity in the gg scatterplot.
qqplot(df_merge[[variable1]], df_merge[[variable2]])
ggplot(df_merge, aes(x = .data[[variable1]])) +
  stat_ecdf(geom = "step", color = "blue", size = 1) +
  geom_line(aes(x = .data[[variable2]], y = ..y..), stat = "ecdf", color = "green", size = 1) +
  labs(title = paste("ECDF Plot between", variable1, "and", variable2),
       x = variable1,
       y = "Cumulative Probability")

variable1 <- "Average Tax of All Returns"
variable2 <- "Property Count"
ggplot(df_merge, aes(x = .data[[variable1]], y = .data[[variable2]])) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear trendline
  labs(title = paste("Scatter Plot between", variable1, "and", variable2),
       x = variable1,
       y = variable2)
# Much lesser linearity in the gg scatterplot.
qqplot(df_merge[[variable1]], df_merge[[variable2]])
ggplot(df_merge, aes(x = .data[[variable1]])) +
  stat_ecdf(geom = "step", color = "blue", size = 1) +
  geom_line(aes(x = .data[[variable2]], y = ..y..), stat = "ecdf", color = "green", size = 1) +
  labs(title = paste("ECDF Plot between", variable1, "and", variable2),
       x = variable1,
       y = "Cumulative Probability")

variable1 <- "Average Tax of All Returns"
variable2 <- "Violent Rate"
ggplot(df_merge, aes(x = .data[[variable1]], y = .data[[variable2]])) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear trendline
  labs(title = paste("Scatter Plot between", variable1, "and", variable2),
       x = variable1,
       y = variable2)
# Much lesser linearity in the gg scatterplot.
qqplot(df_merge[[variable1]], df_merge[[variable2]])
ggplot(df_merge, aes(x = .data[[variable1]])) +
  stat_ecdf(geom = "step", color = "blue", size = 1) +
  geom_line(aes(x = .data[[variable2]], y = ..y..), stat = "ecdf", color = "green", size = 1) +
  labs(title = paste("ECDF Plot between", variable1, "and", variable2),
       x = variable1,
       y = "Cumulative Probability")

variable1 <- "Average Tax of All Returns"
variable2 <- "Violent Count"
ggplot(df_merge, aes(x = .data[[variable1]], y = .data[[variable2]])) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear trendline
  labs(title = paste("Scatter Plot between", variable1, "and", variable2),
       x = variable1,
       y = variable2)
# Much lesser linearity in the gg scatterplot.
qqplot(df_merge[[variable1]], df_merge[[variable2]])
ggplot(df_merge, aes(x = .data[[variable1]])) +
  stat_ecdf(geom = "step", color = "blue", size = 1) +
  geom_line(aes(x = .data[[variable2]], y = ..y..), stat = "ecdf", color = "green", size = 1) +
  labs(title = paste("ECDF Plot between", variable1, "and", variable2),
       x = variable1,
       y = "Cumulative Probability")


# The sample is well below 5000 rows, and hence the normality of its distribution can be tested with a Shapiro-Wilks Test.
shapiro.test(df_merge$`Index Count`)
shapiro.test(df_merge$`Index Rate`)
shapiro.test(df_merge$`Violent Count`)
shapiro.test(df_merge$`Violent Rate`)
shapiro.test(df_merge$`Property Count`)
shapiro.test(df_merge$`Property Rate`)
shapiro.test(df_merge$`Firearm Count`)
shapiro.test(df_merge$`Firearm Rate`)
shapiro.test(df_merge$`Average NY AGI of All Returns`)
shapiro.test(df_merge$`Average NY AGI of Taxable Returns`)
shapiro.test(df_merge$`Average Tax of All Returns`)
shapiro.test(df_merge$`Average Tax of Taxable Returns`)
shapiro.test(df_merge$`Average NY AGI of Nontaxable Returns`)
# Extremely low p-values for every single variable.
# Predictably, this non-synthetic data collected from the NY state government database is not normally distributed.

# Comparing correlations of various crime rates to the AGI of all returns target variable.
variable1 <- "Average NY AGI of All Returns"
variable2 <- "Property Rate"
ggplot(df_merge, aes(x = .data[[variable1]], y = .data[[variable2]])) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear trendline
  labs(title = paste("Scatter Plot between", variable1, "and", variable2),
       x = variable1,
       y = variable2)
# Poor linear correlation with a relatively flat trendline, unlikely there is much correlation.
qqplot(df_merge[[variable1]], df_merge[[variable2]])
ggplot(df_merge, aes(x = .data[[variable1]])) +
  stat_ecdf(geom = "step", color = "blue", size = 1) +
  geom_line(aes(x = .data[[variable2]], y = ..y..), stat = "ecdf", color = "green", size = 1) +
  labs(title = paste("ECDF Plot between", variable1, "and", variable2),
       x = variable1,
       y = "Cumulative Probability")

variable1 <- "Average NY AGI of All Returns"
variable2 <- "Violent Rate"
ggplot(df_merge, aes(x = .data[[variable1]], y = .data[[variable2]])) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear trendline
  labs(title = paste("Scatter Plot between", variable1, "and", variable2),
       x = variable1,
       y = variable2)
# Very very similar distribution to the correlation with property rate.

variable1 <- "Average NY AGI of All Returns"
variable2 <- "Firearm Rate"
ggplot(df_merge, aes(x = .data[[variable1]], y = .data[[variable2]])) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear trendline
  labs(title = paste("Scatter Plot between", variable1, "and", variable2),
       x = variable1,
       y = variable2)
# Stronger linear correlation but still relatively weak.
# Predictably there's multiple areas - predominantly on the lower end of AGI - with 0 firearm rates, or 0 violence.
# This may be the first hint that relatively impoverished neighborhoods aren't immediately the most susceptible to (at least firearm-related) crime.

# Model 1
# Multivariate Regression to determine the correlation between target variable and multiple independent variables.

# Performing the train-test split on the data.
set.seed(123)
train_indices <- sample(1:nrow(df_merge), 0.7 * nrow(df_merge))
train_data <- df_merge[train_indices, ]
test_data <- df_merge[-train_indices, ]

print(nrow(train_data))

#head(df_merge)

# dplyr package
# sample_frac
# sample_n

multivar_model <- lm(indexcount ~ agi_tax + agi_non, data = train_data)
#multivar_model
summary(multivar_model)
# Adjusted R-squared: 0.1997
mse <- mean(multivar_model$residuals^2)
print(mse)
# 2816475

multivar_model <- lm(indexcount ~ agi_tax + agi_non + violentrate, data = train_data)
summary(multivar_model)
# Adjusted R-squared: 0.5851
mse <- mean(multivar_model$residuals^2)
print(mse)
# 2816475

# Removing the non-numeric variable.
#view(df_merge) # County column needs to be dropped for the correlation matrix to work.

df_merge_num <- subset(df_merge, select = -County)
df_merge_num <- subset(df_merge_num, select = -County.y)
print(sapply(df_merge_num, class))
cormatrix <- cor(df_merge_num)

heatmap(cormatrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100), 
        main = "Correlation Heatmap",
        margins = c(10, 10),
        cexRow = 0.75, cexCol = 0.75,
        key.title = NA)

# Near-perfect 1 fit, of course.
multivar_modeltemp <- lm(indexcount ~ agi_all + agi_tax + tax_all + tax_tax + agi_non + violentcount + indexrate + violentrate + propertycount + propertyrate + firearmcount + firearmrate, data = train_data)
summary(multivar_modeltemp)
mse <- mean(multivar_modeltemp$residuals^2)
#print(mse)
multivar_modeltemp <- lm(indexrate ~ agi_all + agi_tax + tax_all + tax_tax + agi_non + violentcount + indexcount + violentrate + propertycount + propertyrate + firearmcount + firearmrate, data = train_data)
summary(multivar_modeltemp)
mse <- mean(multivar_modeltemp$residuals^2)
#print(mse)

# I'm interested in seeing the correlation of financial variables with a given rate of criminality.

multivar_modeltemp <- lm(indexrate ~ agi_all + agi_tax + tax_all + tax_tax + agi_non, data = train_data)
summary(multivar_modeltemp)
mse <- mean(multivar_modeltemp$residuals^2)


# Assessing the fit using only the AGI financial data.
multivar_modeltemp1 <- lm(indexcount ~ agi_tax + agi_non + tax_all + tax_tax, data = train_data)
summary(multivar_modeltemp1)
mse <- mean(multivar_modeltemp1$residuals^2)
print(mse)
# low correlation of these variables, Adjusted R-Squared at about 0.3198

multivar_modeltemp2 <- lm(indexcount ~ agi_tax + agi_non + tax_all + tax_tax + violentcount, data = train_data)
summary(multivar_modeltemp2)
print(mean(multivar_modeltemp2$residuals^2))
# I found the variable that turns the Adjusted R-Squared all the way up to 0.9056

multivar_modeltemp <- lm(indexcount ~ agi_all + agi_tax + agi_non + tax_all + tax_tax, data = train_data)
summary(multivar_modeltemp)
mean(multivar_modeltemp1$residuals^2)

multivar_modeltemp <- lm(indexcount ~ agi_all + agi_tax + agi_non + tax_all, data = train_data)
summary(multivar_modeltemp)
mean(multivar_modeltemp1$residuals^2)

multivar_modeltemp <- lm(indexcount ~ agi_all + agi_tax + tax_all, data = train_data)
summary(multivar_modeltemp)
mean(multivar_modeltemp1$residuals^2)

multivar_modeltemp <- lm(indexcount ~ agi_tax + tax_all, data = train_data)
summary(multivar_modeltemp)

multivar_modeltemp2 <- lm(indexcount ~ agi_all + agi_tax + agi_non + tax_all + tax_tax, data = train_data)
summary(multivar_modeltemp2)
print(mean(multivar_modeltemp2$residuals^2))

multivar_modeltemp2 <- lm(indexcount ~ agi_all + agi_tax + agi_non + tax_all + tax_tax + violentrate, data = train_data)
summary(multivar_modeltemp2)
print(mean(multivar_modeltemp2$residuals^2))

multivar_modeltemp2 <- lm(indexcount ~ agi_all + agi_tax + agi_non + tax_all + tax_tax + violentcount, data = train_data)
summary(multivar_modeltemp2)
print(mean(multivar_modeltemp2$residuals^2))

multivar_modeltemp2 <- lm(indexcount ~ agi_all + agi_tax + agi_non + tax_all + tax_tax + propertycount, data = train_data)
summary(multivar_modeltemp2)
print(mean(multivar_modeltemp2$residuals^2))

multivar_modeltemp2 <- lm(indexcount ~ agi_all + agi_tax + agi_non + tax_all + tax_tax + propertyrate, data = train_data)
summary(multivar_modeltemp2)
print(mean(multivar_modeltemp2$residuals^2))

multivar_modeltemp2 <- lm(indexcount ~ agi_all + agi_tax + agi_non + tax_all + tax_tax + firearmcount, data = train_data)
summary(multivar_modeltemp2)
print(mean(multivar_modeltemp2$residuals^2))

multivar_modeltemp2 <- lm(indexcount ~ agi_all + agi_tax + agi_non + tax_all + tax_tax + firearmrate, data = train_data)
summary(multivar_modeltemp2)
print(mean(multivar_modeltemp2$residuals^2))

multivar_modeltemp2 <- lm(indexcount ~ agi_all + agi_tax + agi_non + tax_all + tax_tax + propertycount, data = train_data)
summary(multivar_modeltemp2)
print(mean(multivar_modeltemp2$residuals^2))

multivar_modeltemp2 <- lm(indexcount ~ agi_all + agi_tax + agi_non + tax_all + tax_tax + propertyrate, data = train_data)
summary(multivar_modeltemp2)
print(mean(multivar_modeltemp2$residuals^2))


multivar_modeltemp <- lm(indexrate ~ agi_all + agi_tax + agi_non + tax_all + tax_tax, data = train_data)
summary(multivar_modeltemp)
mean(multivar_modeltemp1$residuals^2)


multivar_modeltemp2 <- lm(indexrate ~ agi_all + agi_tax + agi_non + tax_all + tax_tax, data = train_data)
summary(multivar_modeltemp2)
print(mean(multivar_modeltemp2$residuals^2))

multivar_modeltemp2 <- lm(indexrate ~ agi_all + agi_tax + agi_non + tax_all + tax_tax + violentcount, data = train_data)
summary(multivar_modeltemp2)
print(mean(multivar_modeltemp2$residuals^2))

multivar_modeltemp2 <- lm(indexrate ~ agi_all + agi_tax + agi_non + tax_all + tax_tax + violentrate, data = train_data)
summary(multivar_modeltemp2)
print(mean(multivar_modeltemp2$residuals^2))

multivar_modeltemp2 <- lm(indexrate ~ agi_all + agi_tax + agi_non + tax_all + tax_tax + firearmcount, data = train_data)
summary(multivar_modeltemp2)
print(mean(multivar_modeltemp2$residuals^2))

multivar_modeltemp2 <- lm(indexrate ~ agi_all + agi_tax + agi_non + tax_all + tax_tax + firearmrate, data = train_data)
summary(multivar_modeltemp2)
print(mean(multivar_modeltemp2$residuals^2))

multivar_modeltemp2 <- lm(indexrate ~ agi_all + agi_tax + agi_non + tax_all + tax_tax + propertycount, data = train_data)
summary(multivar_modeltemp2)
print(mean(multivar_modeltemp2$residuals^2))

multivar_modeltemp2 <- lm(indexrate ~ agi_all + agi_tax + agi_non + tax_all + tax_tax + propertyrate, data = train_data)
summary(multivar_modeltemp2)
print(mean(multivar_modeltemp2$residuals^2))

step(multivar_modeltemp, direction = "backward")

# Support Vector Regressor
install.packages("e1071")
library(e1071)

help("svm")
# svm is used to train a support vector machine. It can be used to carry out general regression and classification (of nu and epsilon-type), as well as density-estimation.
svr_model <- svm(indexcount ~ agi_all + agi_tax + tax_all + tax_tax + agi_non + violentcount + indexrate + violentrate + propertycount + propertyrate + firearmcount + firearmrate, data = train_data)
svr_model
# 
# Call:
#   svm(formula = indexcount ~ agi_all + agi_tax + tax_all + tax_tax + 
#         agi_non + violentcount + indexrate + violentrate + propertycount + 
#         propertyrate + firearmcount + firearmrate, data = train_data)
# 
# 
# Parameters:
#   SVM-Type:  eps-regression 
# SVM-Kernel:  radial 
# cost:  1 
# gamma:  0.08333333 
# epsilon:  0.1 

# Number of Support Vectors:  95

# Radial Kernel: model is capable of capturing non-linear data relationships.

svr_predictions <- predict(svr_model, train_data)
summary(svr_predictions)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -8.078   763.647  1472.524  2438.069  2748.681 13721.238 

length(svr_predictions) <- length(svr_predictions) * 0.7
length(svr_predictions)

length(train_data$`Index Count`) #786
length(svr_predictions) #550

plot(train_data$`Index Count`, svr_predictions, main = "SVR Predictions vs Actual", xlab = "Actual", ylab = "Predicted")

plot(svr_model, data <- train_data, train_data$`Index Count`~train_data$`Average NY AGI of All Returns`, slice <- list(train_data$`Index Count` <- 3, train_data$`Average NY AGI of All Returns` <- 4))



# I can repeat this for other correlations. I want to make sure I can correctly interpret it.


library(randomForest)
df_merge$`Average NY AGI of All Returns`
df_merge_concat <- df_merge
names(df_merge_concat) = make.names(names(df_merge))
rf_model <- randomForest(indexcount~., data = df_merge_concat, ntree = 500)
print(rf_model)

set.seed(123)
train_indices <- sample(1:nrow(df_merge_concat), 0.7 * nrow(df_merge))
train_data_rf <- df_merge[train_indices, ]
test_data_rf <- df_merge[-train_indices, ]

importance(rf_model)
varImpPlot(rf_model)

rf_model2 <- randomForest(indexcount~., data = df_merge_concat, ntree = 500, mtry=3, importance=TRUE)
importance(rf_model2)
varImpPlot(rf_model2)
#                                        %IncMSE IncNodePurity
# FIPS                                  2.150038     898284400
# Year                                  7.365227     481088704
# County                                1.990285     702577569
# Average.NY.AGI.of.All.Returns         3.775841    2165170152
# Average.Tax.of.All.Returns            8.298821    2969168731
# Average.NY.AGI.of.Taxable.Returns     3.150368    2180360960
# Average.Tax.of.Taxable.Returns        7.041262    2550883100
# Average.NY.AGI.of.Nontaxable.Returns  4.373931     382985620
# County.Sort.Order                    11.754340   21849606829
# County.y                              6.026123     731531127
# Population                           12.487344   27982697443
# Index.Count                          14.756256   40270198555
# Index.Rate                            8.178963    2081817073
# Violent.Count                        12.663317   30953890180
# Violent.Rate                          9.496258   17263043709
# Property.Count                       15.348230   37566118304
# Property.Rate                         6.659076    1239549464
# Firearm.Count                        10.020730    8224855643
# Firearm.Rate                          6.167546    4649304293

rf_model2_inc <- randomForest(indexcount~ agi_all + agi_non + agi_tax + tax_all + tax_tax, data = df_merge_concat, ntree = 500, mtry=3, importance=TRUE)
importance(rf_model2_inc)
varImpPlot(rf_model2_inc)
# %IncMSE IncNodePurity
# agi_all 28.30081    1518413913
# agi_non 15.99727     781184090
# agi_tax 21.77527     642341291
# tax_all 23.31670    1396674284
# tax_tax 12.02971     755739646

rf_model3 <- randomForest(indexcount~., data = df_merge_concat, ntree = 500, mtry=4, importance=TRUE)
importance(rf_model3)
varImpPlot(rf_model3)
# %IncMSE IncNodePurity
# FIPS                                  6.203502     401067185
# Year                                  6.925870     279157507
# County                                5.741616     508479185
# Average.NY.AGI.of.All.Returns         6.879237     899164984
# Average.Tax.of.All.Returns            5.916147    1511509440
# Average.NY.AGI.of.Taxable.Returns     6.316662    1188952044
# Average.Tax.of.Taxable.Returns        6.264447    2083100704
# Average.NY.AGI.of.Nontaxable.Returns  4.752179     308973952
# County.Sort.Order                    10.401915   19603912613
# County.y                              6.277743     608279286
# Population                           12.777767   32064187815
# Index.Count                          16.567479   47734091407
# Index.Rate                            6.234880    1619238468
# Violent.Count                        10.896502   24306276916
# Violent.Rate                          8.508253   15733984043
# Property.Count                       15.967506   44866688651
# Property.Rate                         6.240498     573476474
# Firearm.Count                         9.519322    6985614515
# Firearm.Rate                          5.484642    3700712838

rf_model3_inc <- randomForest(indexcount~ agi_all + agi_non + agi_tax + tax_all + tax_tax, data = df_merge_concat, ntree = 500, mtry=4, importance=TRUE)
importance(rf_model3_inc)
varImpPlot(rf_model3_inc)
# %IncMSE IncNodePurity
# agi_all 29.346815    1637099440
# agi_non 16.980615     764394354
# agi_tax 21.425450     599051277
# tax_all 25.104289    1486172172
# tax_tax  9.695326     635044127

rf_model4 <- randomForest(indexcount~., data = df_merge_concat, ntree = 500, mtry=5, importance=TRUE)
importance(rf_model4)
varImpPlot(rf_model4)
# %IncMSE IncNodePurity
# FIPS                                  4.808265     274896626
# Year                                  5.667016     324670318
# County                                4.127368     193943324
# Average.NY.AGI.of.All.Returns         5.515665     548452076
# Average.Tax.of.All.Returns            6.681013    1151404423
# Average.NY.AGI.of.Taxable.Returns     5.071408     634584765
# Average.Tax.of.Taxable.Returns        5.833874    1571559769
# Average.NY.AGI.of.Nontaxable.Returns  4.819090     137317426
# County.Sort.Order                     8.851765   13251890831
# County.y                              4.818110     328710524
# Population                           12.778210   31301407609
# Index.Count                          18.951618   57768501988
# Index.Rate                            5.434620     965495925
# Violent.Count                        11.379941   29592168242
# Violent.Rate                          7.902442   12241555681
# Property.Count                       16.104981   45449684467
# Property.Rate                         4.280423     507131961
# Firearm.Count                         8.519271    5853626119
# Firearm.Rate                          4.029661    2173914847

rf_model4_inc <- randomForest(indexcount~ agi_all + agi_non + agi_tax + tax_all + tax_tax, data = df_merge_concat, ntree = 500, mtry=5, importance=TRUE)
importance(rf_model4_inc)
varImpPlot(rf_model4_inc)


rf_model5 <- randomForest(indexcount~., data = df_merge_concat, ntree = 500, mtry=6, importance=TRUE)
importance(rf_model5)
varImpPlot(rf_model5)
# %IncMSE IncNodePurity
# FIPS                                  4.495793     162021469
# Year                                  4.903325     375275132
# County                                4.116231     200181271
# Average.NY.AGI.of.All.Returns         6.967699     359028294
# Average.Tax.of.All.Returns            7.564564     870547401
# Average.NY.AGI.of.Taxable.Returns     3.505543     604099528
# Average.Tax.of.Taxable.Returns        5.446943     735904666
# Average.NY.AGI.of.Nontaxable.Returns  4.019272     135199293
# County.Sort.Order                     8.278224   14619034555
# County.y                              3.336322     168560726
# Population                           10.860075   24629269200
# Index.Count                          19.762576   62440142505
# Index.Rate                            6.974657     667027906
# Violent.Count                        11.288532   30793682354
# Violent.Rate                          7.134635    7524651935
# Property.Count                       18.076686   55781229713
# Property.Rate                         7.030056     284144089
# Firearm.Count                         7.766897    4535793751
# Firearm.Rate                          4.044331     873560385

rf_model5 <- randomForest(indexcount~., data = df_merge_concat, ntree = 500, mtry=6, importance=TRUE)
importance(rf_model5)
varImpPlot(rf_model5)


rf_model5_inc <- randomForest(indexcount~ agi_all + agi_non + agi_tax + tax_all + tax_tax, data = df_merge_concat, ntree = 500, mtry=6, importance=TRUE)
importance(rf_model5_inc)
varImpPlot(rf_model5_inc)

rf_model6 <- randomForest(indexcount~., data = df_merge_concat, ntree = 500, mtry=7, importance=TRUE)
importance(rf_model6)
varImpPlot(rf_model6)
# %IncMSE IncNodePurity
# FIPS                                  5.414131    12783559.9
# Year                                  4.899748      825991.6
# County                                4.923028     9700646.1
# Average.NY.AGI.of.All.Returns         3.190038    13218457.5
# Average.Tax.of.All.Returns            3.920810     8887929.8
# Average.NY.AGI.of.Taxable.Returns     2.922896     2931155.1
# Average.Tax.of.Taxable.Returns        2.467860     4707017.1
# Average.NY.AGI.of.Nontaxable.Returns  3.066564      830222.5
# County.Sort.Order                     7.577996    19036508.0
# County.y                              5.350450     8343544.0
# Population                           10.895471   520626604.4
# Index.Count                          21.875878  1757515803.9
# Index.Rate                            7.587487    54431629.7
# Violent.Count                        12.986322   942293180.1
# Violent.Rate                          3.848858    25108031.5
# Property.Count                       19.619748  1575491927.4
# Property.Rate                         7.026092    21346191.8
# Firearm.Count                         7.912309   291041292.7
# Firearm.Rate                          5.631600    80515054.

rf_model6_inc <- randomForest(indexcount~ agi_all + agi_non + agi_tax + tax_all + tax_tax, data = df_merge_concat, ntree = 500, mtry=7, importance=TRUE)
importance(rf_model6_inc)
varImpPlot(rf_model6_inc)

rf_model7 <- randomForest(indexcount~., data = df_merge_concat, ntree = 500, mtry=8, importance=TRUE)
importance(rf_model7)
varImpPlot(rf_model7)
# %IncMSE IncNodePurity
# FIPS                                  5.552906     4987314.0
# Year                                  5.210668      676688.4
# County                                5.060857     5185087.2
# Average.NY.AGI.of.All.Returns         2.625373     7189745.6
# Average.Tax.of.All.Returns            2.965391    13014062.5
# Average.NY.AGI.of.Taxable.Returns     4.026029     1616446.8
# Average.Tax.of.Taxable.Returns        1.874491     1846634.5
# Average.NY.AGI.of.Nontaxable.Returns  1.262529      863645.9
# County.Sort.Order                     5.614649    15393462.4
# County.y                              2.719548     5284729.0
# Population                            9.634524   405107372.8
# Index.Count                          23.917521  1926693910.9
# Index.Rate                            6.955317    27913445.4
# Violent.Count                        11.413747   744385252.5
# Violent.Rate                          3.187997    12921120.9
# Property.Count                       21.279767  1833547277.8
# Property.Rate                         6.677152    14235266.1
# Firearm.Count                         6.363640   233234640.0
# Firearm.Rate                          5.700982    94680547.1

rf_model7_inc <- randomForest(indexcount~ agi_all + agi_non + agi_tax + tax_all + tax_tax, data = df_merge_concat, ntree = 500, mtry=8, importance=TRUE)
importance(rf_model7_inc)
varImpPlot(rf_model7_inc)
# %IncMSE IncNodePurity
# agi_all 29.52665    1620079154
# agi_non 14.54039     798398007
# agi_tax 22.31936     567914464
# tax_all 23.76389    1492226370
# tax_tax 11.96218     610733949



