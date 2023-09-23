# library(XLS)
library(readxl)

data <- read.csv("GPW3_GRUMP_SummaryInformation_2010.csv")

epidata <- read.csv("2010EPI_data.csv", skip = 1)
#View(epidata)

head(epidata, 2)
help(data)
#epidata = read.csv(file.choose(), header=T)

names(epidata) <- as.matrix(epidata[1,])
epidata <- epidata[-1,]
View(epidata)

EPI


epidata[-1,]

attach(epidata) # Sets a 'default' object
fix(epidata) # Launches a simple data editor
epidata # Prints out values of epidata

tf <- is.na(epidata) # Records true values for when the data value is missing
tf
#E <- epidata(!tf) # Filters out T/F values

summary(epidata) # stats

epidata$EPI <- as.integer(epidata$EPI)

fivenum(epidata$EPI, na.rm=TRUE)
stem(epidata$EPI)

hist(epidata$EPI)
rug(epidata$EPI)


epidata$PopulationDensity07 <- as.integer(epidata$PopulationDensity07)
epidata$BIODIVERSITY <- as.integer(epidata$DALY)

fivenum(epidata$PopulationDensity07, na.rm=TRUE) # Five number test
stem(epidata$PopulationDensity07) # Stem and leaf plot
stem(epidata$DALY)
hist(epidata$PopulationDensity07, seq(30., 95., 1.0), prob=TRUE) # Histogram
hist(epidata$DALY)

lines(density(epidata$PopulationDensity07, na.rm = TRUE, bw=1.))

rug(epidata$PopulationDensity07, epidata$DALY)
help(rug)

plot(ecdf(epidata$EPI), do.points = FALSE, verticals = TRUE) # Cumulative density graph
par(pty = "s")
qqnorm(epidata$EPI); qqline(epidata$EPI) # Quantile-quantile graph

boxplot(epidata$EPI, epidata$BIODIVERSITY)
boxplot(epidata$EPI, epidata$DALY)
qqplot(epidata$EPI, epidata$DALY)

# intercomparing more values
boxplot(epidata$EPI, epidata$ENVHEALTH)
boxplot(epidata$ECOSYSTEM, epidata$WATER_H)
qqplot(epidata$AIR_H, epidata$WATER_H)
plot(ecdf(epidata$AIR_EWATER_E), do.points = FALSE, verticals = TRUE)

# What is the x here?
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for DSN") # Quantile-quantile plot
#qqline(x)
qqplot(epidata$EPI, epidata$AIR_H, xlab="EPI / Air Pollution")

help(distributions)

EPILand <- epidata$EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(EPILand)
#hist(ELand, seq(30., 95., 1.0), prob=TRUE)
