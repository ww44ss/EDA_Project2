## EDA Project 2
## WAS
## August 2014

## Plot5.R
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

## Select "ON-ROAD" sources
c <- b[grepl("ON-ROAD", b$type) == TRUE, ]
## select Baltimore fips
e <- c[grepl("24510", c$fips) == TRUE, ]

## sum
aggpol <- aggregate(e$Emissions, list(year = e$year), sum)

## PLOTS

## plot on screen
plot(aggpol, type="o", col=2, lty=1, ylim=c(0, 1000), lwd = 2, xlim= c(1998, 2008), ylab=expression('Total Baltimore ON-ROAD PM'[2.5]))
abline(lm(aggpol$x~aggpol$year),col="blue", lty=2, lwd=1)
title(main = expression("Total Baltimore ON-ROAD PM"[2.5]*" by year"))


## Store plot to .png file
png(filename=paste0(d, "/Plot5.png"))
plot(aggpol, type="o", col=2, lty=1, ylim=c(0, 1000), lwd = 2, xlim= c(1998, 2008), ylab=expression('Total Baltimore ON-ROAD PM'[2.5]))
abline(lm(aggpol$x~aggpol$year),col="blue", lty=2, lwd=1)
title(main = expression("Total Baltimore ON-ROAD PM"[2.5]*" by year"))
dev.off()

##

## End