
climate <- read.csv(file="ClimateData.csv")    
head(climate)
climate2 <- subset(climate, Name == "BisonLake" | Name == "NorthLostTrail")
climate3 <- subset(climate2, Year == "2010" | Year == "2011" | Year == "2012")
climate3$recno <- NULL
climate3$Sta <- NULL
climate3$Date <- NULL
climate3$MaxSnowDepth <- NULL
climate3$AvgSnowDepth <- NULL
climate3$GDD_calc_fill <- NULL
climate3$water_year <- NULL
climate3$MinAirTempfill <- NULL
climate3$MaxAirTempfill <- NULL
climate3$AvgAirTempfill <- NULL
head(climate3)
climate3 <- na.omit(climate3)
write.csv(climate3, "ClimateData2.csv")

climate <- read.csv(file="ClimateData2.csv")
head(climate)
climate$Month <- as.factor(climate$Month)
climate$Year <- as.factor(climate$Year)



library(plyr)

# simple example - ddply function
ddply(climate, # input data frame
      "Month", # variable that you'd like to group by
      function(x){ # function you'd like to run on each group 
        mean(x$AvgAirTemp, na.rm=TRUE)
      }
)

# reformatted
monthlyData <- ddply(climate,
                     "Month",
                     function(x){ 
                       MeanAirTemp <- mean(x$AvgAirTemp, na.rm=TRUE)
                       data.frame(MeanAirTemp=MeanAirTemp)
                     }
)
print(monthlyData)

# calculating multiple things for each group
monthlyData <- ddply(climate,
                     "Month",
                     function(x){ 
                       
                       meanAirTemp <- mean(x$AvgAirTemp, na.rm=TRUE)
                       sdAirTemp <- sd(x$AvgAirTemp, na.rm=TRUE)
                       meanPrecip <- mean(x$Precip, na.rm=TRUE)
                       sdPrecip <- sd(x$Precip, na.rm=TRUE)
                       
                       data.frame(meanAirTemp=meanAirTemp,sdAirTemp=sdAirTemp,
                                  meanPrecip=meanPrecip,sdPrecip=sdPrecip)
                     }
)
print(monthlyData)


# splitting data into groups based on multiple factors
monthlyData <- ddply(climate,
                     c("Month","Year"),
                     function(x){ 
                       MeanAirTemp <- mean(x$AvgAirTemp, na.rm=TRUE)
                       data.frame(MeanAirTemp=MeanAirTemp)
                     }
)
print(monthlyData)



# summarise -- simple example
x <- ddply(climate, # input data frame 
           "Month", # variable that you'd like to group by
           summarise, # "helper function" you'd like to run
           MeanAirTemp = mean(AvgAirTemp, na.rm = TRUE)) # function you'd like to apply to each group
print(x)


# two factors for calculations
x <- ddply(climate, 
           "Month", 
           summarise, 
           AirTempMean=mean(AvgAirTemp, na.rm = TRUE), 
           AirTempSD=sd(AvgAirTemp, na.rm=TRUE))
print(x)

# 2 factors to split by
y <- ddply(climate, 
           c("Month","Year"), 
           summarise, 
           MeanAirTemp = mean(AvgAirTemp, na.rm = TRUE))
print(y)


# transform 
x <- ddply(climate, 
           "Month", 
           transform, 
           MonthlyMeanTemp = mean(AvgAirTemp, na.rm = TRUE))
head(x)

# mutate
x <- ddply(climate, 
           "Month", 
           mutate, 
           MeanMaxTemp = mean(MaxAirTemp, na.rm = TRUE),
           MeanMinTemp = mean(MinAirTemp, na.rm = TRUE),
           MeanDiff = MeanMaxTemp - MeanMinTemp)
head(x)

# plotting
par(mfrow = c(1, 3))
d_ply(climate, "Year", summarise, boxplot(AvgAirTemp, xlab="Year", ylab="Mean Air Temperature (degrees C)"))


# linear model (more complicated example)

modelCoeffs <- ddply(climate, 
                     "Year", 
                     function(x) {
                       model <- lm(AvgAirTemp ~ Elevation, data=x)
                       model$coefficients
                       setNames(coef(model), c("Intercept", "Slope"))
})

