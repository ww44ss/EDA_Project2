## EDA Project 2

## Plot2.R
## Creates a plot of total PM2.5 emissions for Question 1.

## Get package
require(RDS)

## Create file paths and read data
d <- getwd()
data25r <- readRDS(paste0(d, "/exdata-data-NEI_data/summarySCC_PM25.rds"))
dataclass <- readRDS(paste0(d, "/exdata-data-NEI_data/Source_Classification_Code.rds"))

## DATA CLEANING

## Get rid of incomplete cases

data25 <- data25r[complete.cases(data25r),]

        ## Create sample to test program changes wit shorter processing times
        ## Use this expression to create a sample of 20000 random elements of the data to reduce processing time
        ## data25 <- data25[c(1, sample(1:16497651,20000,replace=T)),]

## DATA ANALYSIS

## keep only the baltimore cases

databaltimore <- data25[data25$fips == 24510,]

## Create aggregation of sum of PM2.5

aggpol <- aggregate(databaltimore$Emissions, by =list(year = databaltimore$year), sum)

## PLOTTING

## Plot to screen
plot(aggpol, type="o", col=2, lty=1,ylim=c(0,4000), lwd=2,ylab=expression('Total PM'[2.5]))
abline(lm(aggpol$x~aggpol$year),col="blue", lty=2, lwd=1)
title(main = expression("Total PM"[2.5]*" (Baltimore) by year"))


## Store plot to .png file

png(filename=paste0(d, "/Plot2.png"))

plot(aggpol, type="o", col=2, lty=1,ylim=c(0,4000), lwd=2,ylab=expression('Total PM'[2.5]))
abline(lm(aggpol$x~aggpol$year),col="blue", lty=2, lwd=1)
title(main = expression("Total PM"[2.5]*" (Baltimore) by year"))

dev.off()

## End