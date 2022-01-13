memory.limit(180000)
options(scipen=9999)
install.packages("sp")
install.packages("sf")
install.packages("raster")
install.packages("rgdal")
install.packages("tidyverse")
install.packages("rmapshaper")
install.packages("matrixStats")
install.packages("SpatialAcc")

library(sp)
library(sf)
library(raster)
library(rgdal)
library(tidyverse)
library(rmapshaper)
library(matrixStats)
library(SpatialAcc)
library(rgdal)
library(dplyr)
library(tmap)
library(reshape)
library(ggplot2)
library(rgdal)

##population
##oa_temp

setwd("C:/Users/Yeorim/Desktop/advanced/oa_temp")
list<-list.files()
df1<-data.frame()

for(i in list){
  # read *.csv file
  df <- read.csv(i, skip = 1, header = FALSE, stringsAsFactors = FALSE)
  # [1] "YMD" "TT" "dongCode" "censusCode" "TotalPop"
  # [6] "China" "NonChina"
  df <- df[, c(1:2, 4:7)]
  # [1] "YMD" "TT" "censusCode" "TotalPop" "China"
  # [6] "NonChina"
  # define missing value
  for(j in 4:6){
    a <- which(df[, j] == "*")
    df[a, j] <- NA
  }
  # bind dataframe
  df1 <- rbind.data.frame(df1, df)
  
}

##oa_long
setwd("C:/Users/Yeorim/Desktop/advanced/oa_long")
list2<-list.files()
df2<-data.frame()

for(i in list2){
  # read *.csv file
  df <- read.csv(i, skip = 1, header = FALSE, stringsAsFactors = FALSE)
  # [1] "YMD" "TT" "dongCode" "censusCode" "TotalPop"
  # [6] "China" "NonChina"
  df <- df[, c(1:2, 4:7)]
  # [1] "YMD" "TT" "censusCode" "TotalPop" "China"
  # [6] "NonChina"
  # define missing value
  for(j in 4:6){
    a <- which(df[, j] == "*")
    df[a, j] <- NA
  }
  # bind dataframe
  df2 <- rbind.data.frame(df2, df)
  
}

pop<-rbind.data.frame(df1,df2)
names(pop)<- c("YMD","TT", "census_code","TotalPop","China","NonChina")

popmean <- pop %>%
  group_by(census_code) %>%
  summarize(mean_pop = mean(TotalPop))

#load shp
oa_area<-readOGR("C:/Users/Yeorim/Desktop/advanced/data/oa.shp",encoding="UTF-8")
names(oa_area@data)[1]<-"census_code"
oa_area<- merge(oa_area, popmean, by = "census_code")

#load hospital
hospital<-readOGR("C:/Users/Yeorim/Desktop/advanced/data/hospital_foreign.shp",encoding="UTF-8",use_iconv=T)

#centroids
#########st to sf
oa.sf<-st_as_sf(oa_area)
oa.centroid <- st_centroid(oa.sf)
hospital_sf <- st_as_sf(hospital)
hosp.dist <- st_distance(oa.centroid, hospital_sf)


#calculatre 2SFCA
oa_area$mean_pop[is.na(oa_area$mean_pop)]<-0
oa_area$mean_pop
nrow(as.data.frame(hospital@data))
n<-as.vector(rep(1, times = 1491))
hospital@data$n<-n

TSFCA <- ac(p = oa_area$mean_pop, n = hospital$n, 
            D = hosp.dist, d0 = 600, family = "2SFCA")

oa.sf <- oa.sf %>%
  mutate(TSFCA = TSFCA)

#mapificate
tm_shape(oa.sp, unit = "m") +
  tm_polygons(col = "TSFCA", style = "jenks", palette = "Reds", 
              border.alpha = 0, title = "2SFCA") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.outside = TRUE, legend.outside.position = "right")


tm_shape(oa.sp, unit = "m") +
  tm_polygons(col = "mean_pop", style = "jenks", palette = "Reds", 
              border.alpha = 0, title = "mean of de facto population") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.outside = TRUE, legend.outside.position = "right")


oa.sp<-as(oa.sf, Class= "Spatial")
oa.sp@proj4string
crs(oa.sp)<-"+proj=longlat +datum=WGS84"

writeOGR(oa.sp, dsn = "C:/Users/Yeorim/Desktop/advanced/data", layer = 'oatsfca',
         driver = 'ESRI Shapefile')


###########단계구분도
oa.groups <- classIntervals(oa.sp$TSFCA, 5, style = "jenks")
oa.colors <- findColours(oa.groups, brewer.pal(5, "Reds"))
plot(oa.sp, col = oa.colors, border = NA)

legend.text <- vector()
for (i in 1:(length(oa.groups$brks)-1)) {
  legend.text[i] <-
    paste(as.integer(oa.groups$brks)[i:(i+1)], collapse = " - ")
}
legend("bottomleft", fill = attr(oa.colors, "palette"),
       legend = legend.text, border = NA, bg = NA,
       box.col = "gray",cex=0.8)

num<-read.csv("C:/Users/Yeorim/Desktop/advanced/data/num.csv")
View(aa)
aaa<-aa$NUMPOINTS
hist(aaa, seq=)
View(as.data.frame(num))

#loacalG
##shp: seoul
##df:seoul.df, value:total

library(rgdal)
library(classInt)
library(RColorBrewer)
library(spdep)

lw <- poly2nb(oa_area, queen = TRUE)
seoul.nblist <- nb2listw(lw, zero.policy = TRUE )
seoul.localG <- localG(TSFCA, seoul.nblist, zero.policy = TRUE)

oa.sf <- oa.sf %>%
  mutate(seoul.localG = seoul.localG)

oa.sp2<-as(oa.sf, Class= "Spatial")
crs(oa.sp2)<-"+proj=longlat +datum=WGS84"

writeOGR(oa.sp2, dsn = "C:/Users/Yeorim/Desktop/advanced/data", layer = 'oalocalg',
         driver = 'ESRI Shapefile')

aa<-which(oa.sf$seoul.localG<= -1.96)
b<-which(oa.sf$seoul.localG>= 1.96)
View(oa_area@data[aa,])
View(oa_area@data[b,])
View(as.data.frame(unique(oa_area@data[b,]$ADM_NM)))
View(as.data.frame(unique(oa_area@data[a,]$ADM_NM)))
quantile(oa_area$mean_pop)
a
plot(oa_area, col = "Grey 90", border = NA)
plot(oa_area[a,], col = "red", border = NA, add = TRUE)

median(oa_area$mean_pop)
nrow(as.data.frame(oa_area[a,]))
nrow(as.data.frame(oa_area[b,]))

View(as.data.frame(oa_area[a,]))

View(as.data.frame(oa.sf$seoul.localG))

oa.sf$seoul.localG
G<-as.numeric(oa.sf$seoul.localG)     
View(as.data.frame(G))

oa.sf$seoul.localG<-G
temp<-as.data.frame(oa_area[a,])
temp$num<-as.vector(rep(0, times = 12))
View(temp)
