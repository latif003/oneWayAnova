# Latifah thesis 14 february 2015
# calculate anova value between Landcover. we interested in 8 indices: ndvi, evi, ndmi, nbr, rsr, tcw, tcg, tcw
# F critical value: http://homepages.wmich.edu/~hillenbr/619/AnovaTable.pdf
# http://www.itl.nist.gov/div898/handbook/eda/section3/eda3673.htm
# http://link.springer.com/chapter/10.1007/978-1-4419-0052-4_7#page-1

# always clean the workspace before we start, its a good habit :)
rm(list=ls())
ls()

# load libraries
library(tidyr)
library(dplyr)
library(ggplot2)

# read data
data <- read.csv(file = 'data/all_indices.csv', header = TRUE, sep=",") # this data contain all indices value in the column
data <- as.data.frame(data)
head(data) #inspect the data

getwd()

# subsetting raster May 2013
data2 <- subset(data, select = c(current_la, Sample_num, date_sampl, Age_years_, ndvi130529, evi130529, nbr130529, ndmi130529, rsr130529, tcb130529, tcg130529, tcw130529))
colnames(data2) <- c("Landcover", "ID", "sample_date", "age", "ndvi", "evi", "nbr", "ndmi", "rsr", "tcb", "tcg", "tcw")
##data2$tcw <- data2$tcw/10000 # re-scale

##subset(data2, ID!="P11") : to remove rows

# Calc one way ANOVA for each Vis 
model <- lm(tcw ~ Landcover, data = data2) # change the Vis we want to calc
anova(model)
summary(model)

## investigate boxplot
bo <- boxplot(tcw ~ Landcover, data = data2)
bo
# extract outlier if necessary
outliers <- boxplot(ndvi ~ Landcover, data = data2, plot=FALSE)$out
outid <- data2[data2$ndvi %in% outliers,] # find the id of outliers

# calc paralel comparison/ pairways comparison (post doc analysis when F value fron one way ANOVA is significant)
## data$Landuse=factor(data$Landuse, labels=c("fo","maf","sf")) #we can abreviate landuse by factors
anov=aov(tcw ~ Landcover, data=data2)
summary(anov)
post <-TukeyHSD(anov)
post
plot(post)

# modify data in order to have multiple boxplots in one graph be plotted (put column to row)
str(data2)
colnames(data2)[5:12] <- c("NDVI", "EVI", "NBR", "NDMI", "RSR","TC Brightness", "TC Greenness", "TC Wetness")
##data2$Landcover=factor(data2$Landcover, labels=c("fo","manioc","sf")) #we can abreviate Landcover by factors
head(data2)
data2mod <- data2%>%
  gather(Landcover, ID, 5:12) # turn column to row
colnames(data2mod) <- c("Landcover", "ID","sample_date", "age","indices", "value")  

write.csv(data2mod, file="outcsv/indices_modif.csv")

# subsetting data for visualization
df1 <- subset (data2mod, indices %in% c("ndvi", "evi", "nbr", "ndmi")) # contain only four Vis
df2 <- subset (data2mod, indices %in% c("rsr", "tc brightness", "tc greenness", "tc wetness")) #

# plot the data
p <- ggplot(data = data2mod, aes(x=Landcover, y=value))  
p <- p + geom_boxplot(aes(fill=Landcover), outlier.colour = NA)#"blue", outlier.size = 1.5)#NA) #outlier.colour=NA(remove outlier in visualization) "blue", outlier.size = 1
#p <- p + geom_boxplot(outlier.colour = NA)
p <- p + facet_wrap( ~ indices, scales="free", ncol=4)
p <- p + xlab("Landcover") + ylab("Indices value")
p <- p + theme (axis.text.x=element_blank())
p <- p + theme (axis.ticks=element_blank())
p

head(data) # inspect data
#===============================================================================================================================================
#===============================================================================================================================================
# TO KML
library(sp)
library(rgdal)
library(maptools)
# define the coordinates
coords <- cbind(data$x, data$y)
# fill in the original coordinat
coords_utm <- SpatialPointsDataFrame(coords, data = data, proj4string = CRS('+proj=utm +zone=20 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'))
# transform to langlot(because kml is in langlot)
to_kml <- spTransform(coords_utm, CRSobj = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
kmlPoints(obj = to_kml, kmlfile = 'to_kml.kml')
codf <- as.data.frame(coords_utm) # to identify the site number



