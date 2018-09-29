library(rvest)
tmpdocs<-read_html("https://www.imdb.com/chart/top")
tit<-tmpdocs %>% html_nodes("strong , .secondaryInfo")%>% html_text()
year<-c()
rate<-c()
for (i in 5:length(tit)){
if (i%%2==1){
year[(i-5)/2+1]<-tit[i]
}
else{
rate[(i-6)/2+1]<-tit[i]
}
}
year<-as.numeric(as.character(substr(year,2,5)))
rate<-as.numeric(as.character(rate))
yr<-data.frame(year,rate)
summary(rate)
year2<-year^2
reg<-lm(rate~year)
summary(reg)
plot(year,rate)
abline(reg)