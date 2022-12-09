install.packages("spatstat")
library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)

LondonBoroughs <- st_read("///Users/lwj/Desktop/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")
library(stringr)
BoroughMap <- LondonBoroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700)

qtm(BoroughMap)
summary(BoroughMap)
BluePlaques <- st_read("///Users/lwj/Desktop/open-plaques-london-2018-04-08.geojson")%>%
  st_transform(.,27700)
summary(BluePlaques)
#plot the blue plaques in the city
tmap_mode("plot")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(col = "blue")


#Data cleaning
#remove duplicates,删除在伦敦范围外的建筑（蓝点）
library(tidyverse)
library(sf)
BluePlaques <- distinct(BluePlaques)
BluePlaquesSub <- BluePlaques[BoroughMap,]
#check to see that they've been removed
tmap_mode("plot")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")
# add sparse=false to get the complete matrix.查看有无相交
intersect_indices <-st_intersects(BoroughMap, BluePlaques)

# Spatial joining
Londonborough <- st_read("///Users/lwj/Desktop/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")%>%
  st_transform(., 27700)
OSM <- st_read("///Users/lwj/Desktop/greater-london-latest-free/gis_osm_pois_a_free_1.shp")%>%
  st_transform(., 27700) %>%
  #select hotels only
  filter(fclass == 'hotel')
join_example <-  st_join(OSM, Londonborough)
nrow(join_example)


# read in the .csv
# and make it into spatial data
Airbnb <- read_csv("///Users/lwj/Desktop/listings.csv")%>%
  st_as_sf(.,coords = c("longitude", "latitude"),crs = 4326) %>%  #longitude和latitude分别为x，y，表示geometry； crs设置坐标系
  st_transform(.,27700)%>%
  #select entire places that are available all year
  filter(room_type == 'Entire home/apt' & availability_365 =='365')

# make a function for the join
# functions are covered in practical 7
# but see if you can work out what is going on
# hint all you have to do is replace data1 and data2
# with the data you want to use

Joinfun <- function(data1, data2){
  
  output<- data1%>%
    st_join(Londonborough,.) %>%
    add_count(GSS_CODE, name="hotels_in_borough") 
  
  return(output)
}
# use the function for hotels
Hotels <- Joinfun(OSM, Londonborough)
# then for airbnb
Airbnb <- Joinfun(Airbnb, Londonborough)

Hotels <- Hotels %>%
  #at the moment each hotel is a row for the borough
  #we just one one row that has number of airbnbs
  group_by(., GSS_CODE, NAME)%>%
  summarise(`Accomodation count` = unique(hotels_in_borough))
Airbnb <- Airbnb %>%
  group_by(., GSS_CODE, NAME)%>%
  summarise(`Accomodation count` = unique(hotels_in_borough))
#At this point I have
#Airbnb sf object = airbnbs per borough
#Hotels sf object = hotels per borough
#join them
all_accomodation <- st_join(Hotels, Airbnb)#错误的
head(all_accomodation)
#x中某几何要素能与几个y中的几何要素相连接，输出结果中该几何要素就占据几行
#每行对应一个y中几何要素的属性数据，这样输出结果的行数可能会大于x的行数

# returns the data based on the same geometries (or polygons)
all_accomodation <- st_join(Hotels, Airbnb, join = st_equals)
head(all_accomodation)

#Study Area
#extract the borough
# select by attribute
Harrow <- BoroughMap %>%
  filter(., NAME=="Harrow")

#Check to see that the correct borough has been pulled out
tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5)
#clip the data to our single borough
BluePlaquesSub <- BluePlaques[Harrow,]
#check that it's worked
tmap_mode("plot")
tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

#now set a window as the borough boundary
window <- as.owin(Harrow)
plot(window)
#For point pattern analysis, we need to create a point pattern (ppp) object
#create a sp object
BluePlaquesSub<- BluePlaquesSub %>%
  as(., 'Spatial')
#create a ppp object
BluePlaquesSub.ppp <- ppp(x=BluePlaquesSub@coords[,1],
                          y=BluePlaquesSub@coords[,2],
                          window=window)
BluePlaquesSub@coords[,1]#.ppp中的x
BluePlaquesSub@coords[,2]#.ppp中的y
BluePlaquesSub.ppp %>%
  plot(.,pch=16,cex=0.5, 
       main="Blue Plaques Harrow")

BluePlaquesSub.ppp %>%
  density(., sigma=500)%>% #The sigma value sets the diameter of the Kernel 
  plot()


#Quadrat Analysis:面积被划分为大小相同的单元，统计分析单元内部的关注点数与假设的差距
#First plot the points
plot(BluePlaquesSub.ppp,
     pch=16,
     cex=0.5, 
     main="Blue Plaques in Harrow")

#now count the points in that fall in a 6 x 6
#grid overlaid across the windowBluePlaquesSub.ppp2<-BluePlaquesSub.ppp %>%
BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 6, ny = 6)%>%
  plot(., add=T, col="red")#not recommend to do

#run the quadrat count
Qcount <- BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 6, ny = 6)%>%
  as.data.frame() %>%
  dplyr::count(Var1=Freq)%>%#Var1代表该区域酒店数量，即频率
  dplyr::rename(Freqquadratcount=n)#Freq代表酒店数量为Var1的区域数量
Qcount %>% #Check the first column data type,if it is factor, onvert it to numeric, as.numeric(as.character())
  summarise_all(class)

sums <- Qcount %>%
  #calculate the total blue plaques (Var * Freq)
  mutate(total = Var1 * Freqquadratcount) %>%
  dplyr::summarise(across(everything(), sum))%>%#前者代表应用到整个表格，后者求和
  dplyr::select(-Var1) #select选定保留该列，负号代表删除该选定列

lambda<- Qcount%>%  
  #λ是单位时间(或单位面积)内随机事件的平均发生次数
  #calculate lambda
  mutate(total = Var1 * Freqquadratcount)%>%
  dplyr::summarise(across(everything(), sum)) %>%
  mutate(lambda=total/Freqquadratcount) %>%
  dplyr::select(lambda)%>%
  pull(lambda)#提取单列，类同$


#Calculate expected using the Poisson formula
QCountTable <- Qcount %>%
  mutate(Pr=((lambda^Var1)*exp(-lambda))/factorial(Var1))%>%  #计算泊松分布公式！
  #now calculate the expected counts based on our total number of plaques
  #and save them to the table
  mutate(Expected= (round(Pr * sums$Freqquadratcount, 0)))

#Compare the frequency distributions of the observed and expected point patterns
plot(c(1,5),c(0,14), type="n",
     xlab="Number of Blue Plaques (Red=Observed,Blue=Expected)", 
     ylab="Frequency of Occurances")
points(QCountTable$Freqquadratcount, 
       col="Red", 
       type="o", 
       lwd=3)
points(QCountTable$Expected, col="Blue", 
       type="o", 
       lwd=3)
#都在较低的端有较高的频率计数-联想到泊松分布
#quadat.test()使用Chi平方检验来比较每个象限的观测频率和期望频率
#卡方检验就是统计样本的实际观测值与理论推断值之间的偏离程度，如果卡方值x^2越大，p越小，二者偏差程度越大；
#反之，p越大，二者偏差越小；若两个值完全相等时，卡方值就为0，表明理论值完全符合。
teststats <- quadrat.test(BluePlaquesSub.ppp, nx = 6, ny = 6)
plot(BluePlaquesSub.ppp,pch=16,cex=0.5, main="Blue Plaques in Harrow")
plot(teststats, add=T, col = "red")


#Ripley’s K test:getting around the limitations of quadrat analysis
K <- BluePlaquesSub.ppp %>%
  Kest(., correction="border") %>% #Kest(., correction=c("Ripley","border"))
  plot()
#直到1300米左右的距离，蓝色斑块似乎聚集在哈罗
#在1500米左右，分布似乎是随机的，然后分散在1600米到2100米之间(点呈现规则分布)。
Kval <- as.data.frame(Kest(BluePlaquesSub.ppp, correction = "Ripley"))


#DBSCAN:点与点之间的聚类(to see if there are any clusters present)
install.packages("fpc")
library(raster)
library(fpc)
st_geometry(BoroughMap)
#Epsilon - this is the radius within which the algorithm with search for clusters
#MinPts - this is the minimum number of points that should be considered a cluster
#first extract the points from the spatial points data frame
BluePlaquesSubPoints <- BluePlaquesSub %>%
  coordinates(.)%>%
  as.data.frame()

#now run the dbscan analysis
db <- BluePlaquesSubPoints %>%
  fpc::dbscan(.,eps = 700, MinPts = 4)
#now plot the results
plot(db, BluePlaquesSubPoints, main = "DBSCAN Output", frame = F)
plot(BoroughMap$geometry, add=T)

# used to find suitable eps value based on the knee in plot
# k is no of nearest neighbours used, use min points
install.packages("dbscan")
library(dbscan)
BluePlaquesSubPoints%>%
  dbscan::kNNdistplot(.,k=4)

db
db$cluster
BluePlaquesSubPoints<- BluePlaquesSubPoints %>%
  mutate(dbcluster=db$cluster)

#create some convex hull polygons to wrap around the points in our clusters
chulls <- BluePlaquesSubPoints %>%
  group_by(dbcluster) %>%
  dplyr::mutate(hull = 1:n(),
                hull = factor(hull, chull(coords.x1, coords.x2)))%>%
  arrange(hull)
#chulls2 <- ddply(BluePlaquesSubPoints, .(dbcluster), 
#  function(df) df[chull(df$coords.x1, df$coords.x2), ])
chulls <- chulls %>%
  filter(dbcluster >=1)

dbplot <- ggplot(data=BluePlaquesSubPoints, 
                 aes(coords.x1,coords.x2, colour=dbcluster, fill=dbcluster)) 
#add the points in
dbplot <- dbplot + geom_point()
#now the convex hulls
dbplot <- dbplot + geom_polygon(data = chulls, 
                                aes(coords.x1,coords.x2, group=dbcluster), 
                                alpha = 0.5) 
#now plot, setting the coordinates to scale correctly and as a black and white plot 
#(just for the hell of it)...
dbplot + theme_bw() + coord_equal()

###add a basemap
##First get the bbox in lat long for Harrow
HarrowWGSbb <- Harrow %>%
  st_transform(., 4326)%>%
  st_bbox()


library(OpenStreetMap)

basemap <- OpenStreetMap::openmap(c(51.5549876,-0.4040502),c(51.6405356,-0.2671315),
                                  zoom=NULL,
                                  "stamen-toner")

# convert the basemap to British National Grid
basemap_bng <- openproj(basemap, projection="+init=epsg:27700")

#autoplot(basemap_bng) sometimes works
autoplot.OpenStreetMap(basemap_bng)+ 
  geom_point(data=BluePlaquesSubPoints, 
             aes(coords.x1,coords.x2, 
                 colour=dbcluster, 
                 fill=dbcluster)) + 
  geom_polygon(data = chulls, 
               aes(coords.x1,coords.x2, 
                   group=dbcluster,
                   fill=dbcluster), 
               alpha = 0.5)  

































































