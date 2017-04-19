### 2017-04-12
### Spatial Analysis Lab
### MRK

sites<-read.csv(file="SamplingSites.csv")    
head(sites)

# latitude and longitude - assign points to be plotted
points<-sites[,4:5]

## Let's make a map with a background   
library(ggmap)
map<-qmap(location=c(lon = -72.897343, lat = 44.351406),zoom=9, maptype="toner-lite",source="stamen")

map + geom_point(data = points, aes(x = Longitude, y = Latitude), color="red", size=3, alpha=0.5)

## Another simpler map
library(maps)
map(database="state", region='vermont', interior=T, fill=T, col="lightgreen")
library(sp)
coordinates(points)<- ~ Longitude + Latitude
points(x=points, pch=17, col="red")

### The following line usually works, maybe something is wrong with the website?
#bioclim <- getData('worldclim', var='bio', res=10)

### Reading in climate and landcover data from tif file
library(raster)
bioclimLC<-stack("bioclim-landuse1.tif") # we need to 'stack' the layers on top of each other so that it is one file
names(bioclimLC)<-c("landcover","bio1","bio2","bio3","bio4","bio5","bio6","bio7","bio8","bio9","bio10","bio11","bio12","bio13","bio14","bio15","bio16","bio17","bio18","bio19") #renaming
bioclimLC1<-crop(x=bioclimLC, extent(-74,-71, 42.5, 45.5)) # cropping it to Vermont's extent
plot(bioclimLC1)

# add in some fake data
set.seed(44) 
points<-sites[,4:5] # subset only the latitudes and longitudes
fakePres<-sample(c(rep("Y", 9),rep("N",9))) ## creating fake presence data to compare sites
points<-cbind(points, fakePres)
climData <- extract(x=bioclimLC1, y=points[, c("Longitude", "Latitude")]) # extracting values of each layer (19 bioclim, 1 landcover) at each of the sites
dbio1 <- cbind(points, climData) 

summary(aov(formula=bio1~fakePres, data=dbio1)) # comparing the annual mean temperature (bio1) of the sites 'with disease' and without

boxplot(bio1~fakePres, data=dbio1, col=c("blue","red"))

mean(subset(dbio1, fakePres=="Y")$bio1)
mean(subset(dbio1, fakePres=="N")$bio1)

# logistic regression
fakeP <- ifelse(dbio1$fakePres=="Y", 1, 0) # assigning presence as '1' and absence as '0' for the model
dbio1$fakeP<-fakeP # adding our new column of 0's and 1's to the original climate dataset
MyModel <- glm(fakeP~bio1, family=binomial("logit"),data=dbio1)
summary(MyModel) 

plot(x=dbio1$bio1,y=fakeP,xlab="Annual Mean Temperature (C)",ylab="P(Extant)", type='n', ylim=c(0,1))
curve(predict(MyModel,data.frame(bio1=x),type="resp"),add=T)
points(x=dbio1$bio1,y=fakeP,cex=2,pch=21,bg="lightblue")

# creating maps with abundance data pie charts
library(plyr)
#siteAbundance<-ddply(data,.(Species, Site_Letter),summarize,total=length(Species))
#head(siteAbundance)
#save(siteAbundance,file="sitebundance.Rdata")
load(file="siteabundance.Rdata")

library(reshape2)
wSiteAbund<-dcast(siteAbundance,Site_Letter~Species) # manipulating so species abundance numbers are listed per site ('wide' format)
head(wSiteAbund)


wSiteAbund$lat<-sites$Latitude # adding longitude and latitude data to site abundance
wSiteAbund$lon<-sites$Longitude
wSiteAbund[is.na(wSiteAbund)]<-0 # replacing NAs with zeros
Names<-names(wSiteAbund)[2:12] # leaving out C. picta because there was only one occurrence


library(rworldmap)
plot(bioclimLC1$bio2, xlim=c(-73.5,-72.4), ylim=c(43,45),col="white", axes=TRUE, legend=F,main="",box=FALSE) # plotting an empty map to get axes
map("state", c('vermont'), interior=T, fill=T, col="lightgreen",add=T)
pies<-mapPies(dF=wSiteAbund[,-4],nameX="lon",nameY="lat",nameZs=Names[-3],zColours=c("blue","purple","orange","red","darkred","darkorchid1","darksalmon","goldenrod","cadetblue","chocolate","coral4"),xlim=c(-72,-74), ylim=c(43,44.8),landCol="gray50",addCatLegend = T,add=T,font=6,symbolSize=2.5)


# predicting distributions
bioclimLC1<-crop(x=bioclimLC, extent(-75, -67, 40, 46)) # cropping to New England extent
plot(bioclimLC1)

library(maptools)
data("wrld_simpl")
ws1<-rasterize(wrld_simpl,bioclimLC1) #Transfer values associated with 'object' type spatial data (points, lines, polygons) to raster cells
values(bioclimLC1)[is.na(values(bioclimLC1))]<-0 # replacing NAs with zeros
layers1<-mask(x=bioclimLC1,mask=ws1) #Create a new Raster* object that has the same values as x, except for the cells that are NA
plot(layers1)

library(dismo)
PresPoints<-subset(dbio1, dbio1$fakePres=="Y")
presPoints<-data.frame(lon=PresPoints[,2],lat=PresPoints[,1]) # need to switch order for Maxent
group <- dismo::kfold(presPoints, 5)
pres_train <- presPoints[group != 1, ]
pres_test <- presPoints[group == 1, ]

### absence points
AbsPoints<-subset(dbio1, dbio1$fakePres=="N")
absPoints<-data.frame(lon=AbsPoints[,2],lat=AbsPoints[,1])
group <- dismo::kfold(absPoints, 5)
abs_train <- absPoints[group != 1, ]
abs_test <- absPoints[group == 1, ]


#run MAXENT
# train the model
xm <- maxent(x=layers1, removeDuplicates=T, p=pres_train,a=abs_train, args=c("-J","-P"))
# xm #opens browser and reveals more stats (importance of variables)

#evaluate (test the model)
e2 <- evaluate(pres_test, abs_test, xm, layers1) 
e2@auc # gives AUC value (1 is perfect prediction)

## keep in mind were are using very low numbers - only using 4 points to test the model  

#predict with all points
xmFull <- maxent(x=layers1, removeDuplicates=T, p=presPoints,a=absPoints, args=c("-J","-P"))
px <- predict(layers1, xmFull, progress="")

#plot maxent
plot(px, main="Predicted distribution with absence data", xlim=c(-75,-67),ylim=c(40,46))
map("state", 'vermont', add=T)
map('usa',add=T)

#### create background set with 500 random points as "pseudo-absence"
backg <- dismo::randomPoints(layers1, n=500, warn=0)
colnames(backg) = c('lon', 'lat')
group <- dismo::kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]

xm2 <- dismo::maxent(x=layers1, removeDuplicates=T, p=pres_train,a=backg_train, args=c("-J","-P"))

e2 <- evaluate(pres_test, backg_test, xm2, layers1)
e2@auc 

#predict
xm2FULL <- maxent(x=layers1, removeDuplicates=T, p=presPoints,a=backg, args=c("-J","-P"))
px2 <- predict(layers1, xm2FULL, progress="")

#plot maxent
plot(px2, main="Predicted distribution with pseudo-absence",xlim=c(-75,-67),ylim=c(40,46))
map("state", 'vermont', add=T)
map('usa',add=T)
coordinates(points)<- ~ Longitude + Latitude
points(points, pch=17, col="red")



