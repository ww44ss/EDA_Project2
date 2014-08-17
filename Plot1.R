## EDA Project 1

#Plot1.R
## Creates a plot of total PM2.5 emissions for Question 1.

##get package
require(RDS)

##create file paths and read data
d <- getwd()
data25 <- readRDS(paste0(d, "/exdata-data-NEI_data/summarySCC_PM25.rds"))
dataclass <- readRDS(paste0(d, "/exdata-data-NEI_data/Source_Classification_Code.rds"))

##create aggregation of sum of PM2.5
aggpol <- aggregate(data25$Emissions, list(year = data25$year), sum)

##plot to screen
plot(aggpol, type="o", col=2, lty=2, ylim=c(0, 8e6),ylab=expression('Total PM'[2.5]))
title(main = expression("Total PM"[2.5]*" by year"))

##Create sample to test program changes
##Use this expression to create a sample of 200 random elements of the data to reduce processing time
##data25 <- data25[c(1, sample(1:200,16497651,replace=T)),]


##store plot to .png file
png(filename=paste0(d, "/Plot1.png"))
plot(aggpol, type="o", col=2, lty=2, ylim=c(0, 8e6),ylab=expression('Total PM'[2.5]))
title(main = expression("Total PM"[2.5]*" by year"))
dev.off()

##end