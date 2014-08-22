## EDA Project 2
## WAS
## August 2014

## Plot4.R
## Creates a plot of total PM2.5 emissions for Question 1.

## Get packages
require(RDS)
require(ggplot2)

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

#merge data sets (PM2.5 and class)
b <- merge(data25, dataclass, by = "SCC")

## find lines that have the "Coal"
c <- b[grepl("Coal", b$EI.Sector) == TRUE, ]

## sum
aggpol <- aggregate(c$Emissions, list(year = c$year), sum)

## PLOTS

## plot on screen
plot(aggpol, type="o", col=2, lty=1, ylim=c(0, 8e5), lwd = 2, xlim= c(1998, 2008), ylab=expression('Total U.S. Coal PM'[2.5]))
abline(lm(aggpol$x~aggpol$year),col="blue", lty=2, lwd=1)
title(main = expression("Total Coal PM"[2.5]*" by year"))


## Store plot to .png file
png(filename=paste0(d, "/Plot4.png"))
plot(aggpol, type="o", col=2, lty=1, ylim=c(0, 8e5), lwd = 2, xlim= c(1998, 2008), ylab=expression('Total U.S. Coal PM'[2.5]))
abline(lm(aggpol$x~aggpol$year),col="blue", lty=2, lwd=1)
title(main = expression("Total Coal PM"[2.5]*" by year"))
dev.off()

## End