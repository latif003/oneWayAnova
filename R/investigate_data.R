# Latifah thesis 15 february 2015
# find out data distribution (min, max mean). we interested in 8 indices: ndvi, evi, ndmi, nbr, rsr, tcw, tcg, tcw

# always clean the workspace before we start, its a good habit :)
rm(list=ls())
ls()

# read data
data <- read.csv(file = 'data/all_indices.csv', header = TRUE, sep=",") 
data <- as.data.frame(data)
head(data) #inspect the data

getwd()

# subsetting raster May 2013
data2 <- subset(data, select = c(current_la, Sample_num, date_sampl, Age_years_, ndvi130529, evi130529, nbr130529, ndmi130529, rsr130529, tcb130529, tcg130529, tcw130529))
colnames(data2) <- c("Landcover", "ID", "sample_date", "age", "ndvi", "evi", "nbr", "ndmi", "rsr", "tcb", "tcg", "tcw")
##subset(data2, ID!="P11") : remove rows


# subset data based on land cover
fo <- subset(data2, Landcover =="forest")
maf <- subset(data2, Landcover =="manioc field")
sf <- subset(data2, Landcover =="secondary forest")

# count observation number
NROW(na.omit(fo$ndvi))

# investigate the data
head(fo) # inspect data
# ========================================================================
# ========================================================================
fun_min <- function(x){
  min <- round(min(x, na.rm=TRUE), 2)
}
fun_max <- function(x){
  max <- round(max(x, na.rm=TRUE), 2)
}
fun_mean <- function(x){
  mean <- round(mean(x, na.rm=TRUE), 2)
}
fun_sd <- function(x){
  mean <- round(sd(x, na.rm=TRUE), 2)
}
# min, max and mean value
fo1 <- apply(fo[,5:12],2,FUN=fun_sd) 
fodf <- as.data.frame(fo1)
maf1 <- apply(maf[,5:12],2,FUN=fun_mean) 
mafdf <- as.data.frame(maf1)
sf1 <- apply(sf[,5:12],2,FUN=fun_mean) 
sfdf <- as.data.frame(sf1)
fo$min <- apply(fo[,5:12],2,FUN=fun_min)

rm(fo1, maf1, sf1, fodf, mafdf, sfdf) 

  
# for crosscheck
data2$ndmi <- data2$ndmi/10000 # change the scale
round(quantile(data2$ndvi, na.rm=TRUE), 2) # calc quantile
round(mean(data2$ndvi, na.rm=TRUE), 2)
round(sd(data2$ndvi, na.rm=TRUE), 2)
max(maf$ndvi, na.rm=T)

tcg_new <- read.csv(file = 'D:/CenterPointNew/outcsv/all_tc.csv', header = TRUE, sep=",") 
head(tcg_new)
fog <- subset(tcg_new, current_la =="forest", select=tcg130529)
mag <- subset(tcg_new, current_la =="manioc field", select=tcg130529)
sfg <- subset(tcg_new, current_la =="secondary forest", select=tcg130529)
max(sfg$tcg130529, na.rm=TRUE)
fog <- fog/10000
