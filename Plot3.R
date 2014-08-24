## EDA Project 2
## WAS
## August 2014

## Plot3.R
## Creates a plot of total PM2.5 emission trends for different sources for Question 3.

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

## Create sample to test program changes with shorter processing times
## Use this expression to create a sample of 20000 random elements of the data to reduce processing time
## data25 <- data25[c(1, sample(1:16497651,20000,replace=T)),]

## DATA ANALYSIS

databaltimore <- data25[data25$fips == 24510,]

## PLOTS

## Plot to screen
a<-qplot(year, Emissions, data=databaltimore, facets = . ~ type, geom=c("point", "smooth"), method="lm", ylim=c(0, NA))+ coord_cartesian(ylim=c(0,150))
print(a)
## Store plot to .png file

png(filename=paste0(d, "/Plot3.png"))
a<-qplot(year, Emissions, data=databaltimore, facets = . ~ type, geom=c("point", "smooth"), method="lm", ylim=c(0, NA))+ coord_cartesian(ylim=c(0,150))
print(a)
dev.off()

## End