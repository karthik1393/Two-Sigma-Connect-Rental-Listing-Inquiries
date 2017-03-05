library(wordcloud)
library(ggmap)
library(ggplot2)
high<-filter(rent,interest_level=="high")
low<-filter(rent,interest_level=="low")
medium<-filter(rent,interest_level=="medium")
nrow(high)
nrow(low)
nrow(medium)
plot_hig<-ggplot(high,aes(bedrooms,price))+geom_point()+ylim(0,9000)
plot_hig
plot_medi<-ggplot(medium,aes(bedrooms,price))+geom_point()+ylim(0,9000)
plot_medi
plot_low<-ggplot(low,aes(bedrooms,price))+geom_point()+ylim(0,15000)
plot_low
hist(num_vars$price)
?hist
####word cloud for features
fe<-rent$features
fe<-Corpus(VectorSource(fe))
# fe<-tm_map(fe,tolower)
# fe<-tm_map(fe,PlainTextDocument)
# 
# fe<-tm_map(fe,removeWords,stopwords("english"))
fe_c<-DocumentTermMatrix(fe)
fe_c<-as.data.frame(as.matrix(fe_c))
fe_wordcloud<-wordcloud(colnames(fe_c),colSums(fe_c),scale=c(3,0.50),colors = brewer.pal(9,"Blues")[c(5,6,7,8,9)],min.freq = 50)
#fe_wordcloud<-wordcloud(colnames(fe_c),colSums(fe_c),scale=c(3,0.25),random.color = TRUE)

#Worldcloud display address
fe1<-rent$display_address
fe1<-Corpus(VectorSource(fe1))
fe<-tm_map(fe,tolower)
fe<-tm_map(fe,PlainTextDocument)
fe<-tm_map(fe,removeWords,stopwords("english"))
fe_c1<-DocumentTermMatrix(fe1)
fe_c1<-as.data.frame(as.matrix(fe_c1))
fe_wordcloud1<-wordcloud(colnames(fe_c),colSums(fe_c),scale=c(3,0.50),colors = brewer.pal(9,"Blues")[c(5,6,7,8,9)],min.freq = 50)

##united states

?geocode

map<-get_map(location = c(lon = -95.3632715, lat = 29.7632836),zoom=4,maptype="roadmap",source="google")
ggmap(map)
main<-ggmap(map)+geom_point(size=1,data=rent,aes(x=rent$longitude,y=rent$latitude,color=rent$interest_level))+scale_color_brewer(palette="Set1")
##
geocode("newyork")
map1<-get_map(location = c(lon = -74.00594, lat =  40.71278),zoom=15,maptype="roadmap",source="google")
ggmap(map1)
main1<-ggmap(map1)+geom_point(size=2,data=rent,aes(x=rent$longitude,y=rent$latitude,color=rent$interest_level))+scale_color_brewer(palette="Set1")
main1

