install.packages("ggmap")
library(ggmap)
map <- get_map(location =c(121.5155386,25.0457962), zoom = 12)
plot(map)
mrt<-read.csv("C:/Users/dr476825/Desktop/taipeimertro/mrt_pop_holi.csv")
attach(mrt)
ggmap(map, darken = c(0.5, "black")) + geom_point(aes(x = lat, y = lon, size = V12,color=V12f,alpha=0.8), data = mrt)+scale_color_gradient2(name="out-in",low="blue",high="red",mid="white")+scale_size(name="假日23時捷運人潮量",range = c(0, 20))
tpebnd<-read.csv("C:/Users/dr476825/Desktop/file/106_2_spatial_analysis/classdata/tpe_sqr_bnd.csv")
plot(tpebnd)
library(splancs)
as.points(tpebnd[,2])