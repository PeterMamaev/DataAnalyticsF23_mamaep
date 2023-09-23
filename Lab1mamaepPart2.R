library(ggplot2)

multivariate <- read.csv("multivariate.csv")
head(multivariate)
attach(multivariate)
View(multivariate)
help(lm) # lm is used to carry out regression and covariance analysis

mm <- lm(Homeowners~Immigrant) # Notably the immigrants are presented as a float
mm

summary(mm)$coef # The output above shows the estimate of the regression beta coefficients (column Estimate)

column Pr(>|t|)
plot(Homeowners~Immigrant)
help(abline) # Adds straight line through a current plot
abline(mm, col=2, lwd=3)
#abline(mm, col=2, lwd=4)

# Pass immigrant values 
newimmigrantdata <- data.frame(Immigrant = c(0, 20))
mm %>% predict(newimmigrantdata)
help(predict) # General predictions from model fitting functions

abline(mm, col=3, lwd=3)
attributes(mm)


# Creating the actual plots
plot(mtcars$wt, mtcars$mpg)
library(ggplot2)
qqplot(mtcars$wt, mtcars$mpg)
qplot(mtcars$wt, mpg, data = mtcars)
ggplot(mtcars, aes(x=wt,y=mpg)) + geom_point() # Dotplot
plot(pressure$teperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure)

# Color coding the lines
lines(pressure$temperate, pressure$pressure/2, col="red")
points(pressure$temperature, pressure$pressure/2, col="green")
library(ggplot2)

qplot(pressure$temperature, pressure$pressure, geom="line")
qplot(temperature, pressure, data = pressure, geom = "line") # relationship line between temperature and pressure variables
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line() + geom_point() # adds points to relationship line

# Bar graphs
barplot(BOD$demand, names.arg = BOD$Time) # bar graph
table(mtcars$cyl)
barplot(table(mtcars$cyl)) # table of contents
qplot(mtcars$cyl) # bar graph
qplot(factor(mtcars$cyl))
qplot(factor(cyl), data = mtcars)
ggplot(mtcars, aes(x=factor(cyl))) + geom_bar()

hist(mtcars$mpg)
hist(mtcars$mpg, breaks=10) # specify approximate number of bins with breaks
hist(mtcars$mpg, breaks=5)
hist(mtcars$mpg, breaks=11)
qplot(mpg, data = mtcars, binwidth = 4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth = 4)

# Comparative box plot
plot(ToothGrowth$supp, ToothGrowth$len) # passing plot() function a factor of x-values and vector of y-values
boxplot(len - supp, data = ToothGrowth) # comparative boxplot formula syntax, only used if values are in the same dataframe
boxplot(len ~ supp + dose, data = ToothGrowth)
library(ggplot2)
qplot(ToothGrowth$supp, ToothGrowth$len, geom = "boxplot")

# syntax for boxplot if two vectors are in the same dataframe
qplot(supp, len, data = ToothGrowth, geom = "boxplot")
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
# comparative boxplot using three separate vectors
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom = "boxplot")
qplot(interaction(supp, dose), len, data = ToothGrowth, geom = "boxplot")
# same operation performed with ggplot() function
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len)) + geom_boxplot()
