## EDA Project 2
## WAS
## August 2014

## Plot6.R
## compares PM2.5 for different counties

## Get packages
require(RDS)
require(ggplot2)
require(maps)
library(maps)
data(county.fips)

## GET DATA
## this simple instruction just checks to see if the data are already in memory and if not then read them in. 
## it turns out this has been a huge timesaver FYI.

if(data25[1,1]=="09001") {print("data appears to be loaded")} else {
        
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
    
       
        #merge data sets (PM2.5 and class)
        combo <- merge(data25, dataclass, by = "SCC")

}

if(onroadflag == "setp6") {print("onroad appears good for p6")} else {
        ## Select "ON-ROAD" sources
        onroad <- combo[grepl("ON-ROAD", combo$type) == TRUE, ]
        
        #merge with county.fips data
        onroad$fips<-as.integer(onroad$fips)
        onroad <- merge(onroad, county.fips, by = "fips")
        
        onroad <- onroad[c("fips", "Emissions", "type", "year", "polyname")]
        
        onroadflag<-"setp6"
}
        
## DEFINE FUNCTIONS FOR SIMPLIFIED ANALYSIS

## sum polution levels function
sumpollution <- function(x) aggregate(x$Emissions, list(year = x$year), sum)
## mean pollution level function
meanpollution <- function(x) aggregate(x$Emissions, list(year = x$year), mean)

## utility function to apply proper names to data frames
renamecolumns <- function(y) {
        colnames(y) <- c("year", "totalPM25")
        y
}

## Roll all the above into a single function
countypollutiontrend<- function(fips) renamecolumns(sumpollution(onroad[grepl(as.character(fips), onroad$fips) == TRUE, ]))

## note the tis mean is optional and I just added for interest sake
meanpollutiontrend<- function(fips) renamecolumns(meanpollution(onroad[grepl(as.character(fips), onroad$fips) == TRUE, ]))

## ANALYZE DATA

## build data sets for select cities
## note this is more than required, but I had some extra time so decided to play around a bit
## I started to automate the city names above with county.fips data but ran out of time so the below part is still manual
##

## create totals for select cities
## integrate into on data frame

balt25 <- countypollutiontrend(24510)
        city<-rep("Baltimore", 4)
        PM25totals<-cbind(balt25, city)
        PM25totals2<-cbind(balt25, city)
la25 <- countypollutiontrend(06037)
        city<-rep("Los Angeles", 4)
        la25<-cbind(la25, city)
        PM25totals<-rbind(PM25totals, la25)
        PM25totals2<-rbind(PM25totals, la25)
dallas25 <- countypollutiontrend(48113)
        city<-rep("Dallas", 4)
        dallas25<-cbind(dallas25, city)
        PM25totals<-rbind(PM25totals, dallas25)
pdx25 <- countypollutiontrend(41051)
        city<-rep("PortlandOR", 4)
        pdx25<-cbind(pdx25, city)
        PM25totals<-rbind(PM25totals, pdx25)
seattle25 <- countypollutiontrend(53033)
        city<-rep("Seattle", 4)
        seattle25<-cbind(seattle25, city)
        PM25totals<-rbind(PM25totals, seattle25)
denver25 <- countypollutiontrend(08031)
        city<-rep("Denver", 4)
        denver25<-cbind(denver25, city)
        PM25totals<-rbind(PM25totals, denver25)
chicago25 <- countypollutiontrend(17031)
        city<-rep("Chicago", 4)
        chicago25<-cbind(chicago25, city)
        PM25totals<-rbind(PM25totals, chicago25)
louisville25 <- countypollutiontrend(21111)
        city<-rep("Louisville", 4)
        louisville25<-cbind(louisville25, city)
        PM25totals<-rbind(PM25totals, louisville25)
nyc25<-countypollutiontrend(36061)
        city<-rep("New York", 4)
        nyc25<-cbind(nyc25, city)
        PM25totals<-rbind(PM25totals, nyc25)



## plot on screen

a1<-qplot(year, totalPM25, data=PM25totals, facets = . ~ city, geom=c("point", "smooth"), method="lm", main = expression("Total PM"[2.5]*" by year") ) 
print(a1)
a2<-qplot(year, totalPM25, data=PM25totals2, facets = . ~ city, geom=c("point", "smooth"), method="lm", main = expression("Total PM"[2.5]*" by year") ) 
print(a2)

## Store plot to .png file
png(filename=paste0(d, "/Plot6.png"))
a1 <-qplot(year, totalPM25, data=PM25totals2, facets = . ~ city, geom=c("point", "smooth"), method="lm", main = expression("Total PM"[2.5]*" by year") ) 
print(a1)
dev.off()

png(filename=paste0(d, "/Plot6alt.png"))
a2 <-qplot(year, totalPM25, data=PM25totals, facets = . ~ city, geom=c("point", "smooth"), method="lm", main = expression("Total PM"[2.5]*" by year") ) 
print(a2)
dev.off()
##

## End