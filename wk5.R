library(sf)
Londonborough <- st_read("///Users/lwj/Desktop/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")%>%
  st_transform(., 27700)#转化为英国坐标系，ESPG code为27700
OSM <- st_read("///Users/lwj/Desktop/greater-london-latest-free/gis_osm_pois_a_free_1.shp")%>%
  st_transform(.,27700)%>%
  dplyr::filter(fclass == 'hotel')#在fclass列只保留hotel的数据
join_example <-  st_join(Londonborough, OSM)#st_join=left jion,保留左侧数据集数据
head(join_example)
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(here)
Worldcities <- st_read("///Users/lwj/Desktop/World_Cities/World_Cities.shp")%>%
  st_transform(., 27700)
UK_outline <- st_read("///Users/lwj/Desktop/gadm41_GBR_shp/gadm41_GBR_0.shp")%>%
  st_transform(., 27700)
Airbnb <- read_csv("///Users/lwj/Desktop/listings.csv")%>%
  st_as_sf(.,coords = c("longitude", "latitude"),crs = 4326) %>%  #longitude和latitude分别为x，y，表示geometry； crs设置坐标系
  st_transform(.,27700)%>%
  filter(room_type == 'Entire home/apt' & availability_365 =='365')
# make a function for the join

Joinfun <- function(data1, data2){
  
  output<- data1%>%
    st_join(data2,.) %>%#sf包中的st_join()使用空间位置关系在两个矢量对象之间传递属性信息,x和y均需是sf格式
    add_count(GSS_CODE, name="hotels_in_borough") 
  
  return(output)
}
#R语言通过function()指令来命名和创建函数。
#首先要给函数赋值，也就是命名，然后在小括号中写入参数，最后再大括号中写入函数要执行的语句
Hotels <- Joinfun(OSM,Londonborough)
Airbnb <- Joinfun(Airbnb,Londonborough)

Worldcities2 <- Worldcities %>%
  filter(CNTRY_NAME=='United Kingdom'&
           Worldcities$CITY_NAME=='Birmingham'|
           Worldcities$CITY_NAME=='London'|
           Worldcities$CITY_NAME=='Edinburgh')
newbb <- c(xmin=-296000, ymin=5408, xmax=655696, ymax=1000000)
UK_outlinecrop <- UK_outline$geometry %>%
  st_crop(., newbb)
#st_crop:用指定地理或投影坐标确定的矩形范围截取矢量对象
Hotels <- Hotels %>%
  #at the moment each hotel is a row for the borough
  #we just one one row that has number of airbnbs
  group_by(., GSS_CODE, NAME)%>%
  summarise(`Accomodation count` = unique(hotels_in_borough))

Airbnb <- Airbnb %>%
  group_by(., GSS_CODE, NAME)%>%
summarise(`Accomodation count` = unique(hotels_in_borough))

#Make the map
tmap_mode("plot")
# set the breaks
# for our mapped data
breaks = c(0, 5, 12, 26, 57, 286) 

# plot each map
tm1 <- tm_shape(Hotels) + 
  tm_polygons("Accomodation count", 
              breaks=breaks,
              palette="PuBu")+
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(a)", position=c(0,0.85), size=1.5)

tm2 <- tm_shape(Airbnb) + 
  tm_polygons("Accomodation count",
              breaks=breaks, 
              palette="PuBu") + 
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(b)", position=c(0,0.85), size=1.5)

tm3 <- tm_shape(UK_outlinecrop)+ 
  tm_polygons(col="darkslategray1")+
  tm_layout(frame=FALSE)+
  tm_shape(Worldcities2) +
  tm_symbols(col = "red", scale = .5)+
  tm_text("CITY_NAME", xmod=-1, ymod=-0.5)

legend <- tm_shape(Hotels) +
  tm_polygons("Accomodation count",
              palette="PuBu") +
  tm_scale_bar(position=c(0.2,0.04), text.size=0.6)+
  tm_compass(north=0, position=c(0.65,0.6))+
  tm_layout(legend.only = TRUE, legend.position=c(0.2,0.25),asp=0.1)+
  tm_credits("(c) OpenStreetMap contrbutors and Air b n b", position=c(0.0,0.0))

t=tmap_arrange(tm1, tm2, tm3, legend, ncol=2)
t

#or arrange our maps using the grid package
library(grid)
# erases the current device or moves to a new page 
# probably not needed but makes sure you are plotting on a new page.
grid.newpage()

pushViewport(viewport(layout=grid.layout(2,2)))
print(tm1, vp=viewport(layout.pos.col=1, layout.pos.row=1, height=5))
print(tm2, vp=viewport(layout.pos.col=2, layout.pos.row=1, height=5))
print(tm3, vp=viewport(layout.pos.col=1, layout.pos.row=2, height=5))
print(legend, vp=viewport(layout.pos.col=2, layout.pos.row=2, height=5))


#INSET MAP
Londonbb <- st_bbox(Airbnb,crs = st_crs(Airbnb))%>%
  st_as_sfc()#转换为sf内，sfc针对几何

main <- tm_shape(Airbnb, bbbox = Londonbb) + 
  tm_polygons("Accomodation count",
              breaks=breaks, 
              palette="PuBu")+
  tm_scale_bar(position = c("left", "bottom"), text.size = .75)+
  tm_layout(legend.position = c("right","top"), 
            legend.text.size=.75, 
            legend.title.size = 1.1,
            frame=FALSE)+
  tm_credits("(c) OpenStreetMap contrbutors and Air b n b", position=c(0.0,0.0))+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.06, 0.1)) +
  
  #bottom left top right
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.2))

inset = tm_shape(UK_outlinecrop) + tm_polygons() +
  tm_shape(Londonbb)+ 
  tm_borders(col = "grey40", lwd = 3)+
  tm_layout(frame=FALSE,
            bg.color = "transparent")+
  tm_shape(Worldcities2) +
  tm_symbols(col = "red", scale = .5)+
  tm_text("CITY_NAME", xmod=-1.5, ymod=-0.5)

library(grid)
main
print(inset, vp = viewport(0.86, 0.29, width = 0.5, height = 0.55))

tmap_save(t, 'hotelsandairbnbR.png')

library(grid)
tmap_save(main,insets_tm = inset,insets_vp=viewport(x=0.86, y=0.29, width=.5, height=.55), filename="test.pdf", dpi=600)


#Interactive map
tmap_mode("view")
tm_shape(Airbnb) + 
  tm_polygons("Accomodation count", breaks=breaks) 


#Advanced interactive map
# library for pop up boxes
install.packages("leafpop")
install.packages("leaflet")
library(leafpop)
library(leaflet)

#join data
Joined <- Airbnb%>%
  st_join(., Hotels, join = st_equals)%>%
  dplyr::select(GSS_CODE.x, NAME.x, `Accomodation count.x`, `Accomodation count.y`)%>%
  dplyr::rename(`GSS code` =`GSS_CODE.x`,
                `Borough` = `NAME.x`,
                `Airbnb count` = `Accomodation count.x`,
                `Hotel count`= `Accomodation count.y`)%>%
  st_transform(., 4326)


#remove the geometry for our pop up boxes to avoid
popupairbnb <-Joined %>%
  st_drop_geometry()%>%
  dplyr::select(`Airbnb count`, Borough)%>%
  popupTable()

popuphotel <-Joined %>%
  st_drop_geometry()%>%
  dplyr::select(`Hotel count`, Borough)%>%
  popupTable()

tmap_mode("view")

# set the colour palettes using our previously defined breaks


pal1 <- Joined %>%
  colorBin(palette = "YlOrRd", domain=.$`Airbnb count`, bins=breaks)

pal1 <-colorBin(palette = "YlOrRd", domain=Joined$`Airbnb count`, bins=breaks)

pal2 <- Joined %>%
  colorBin(palette = "YlOrRd", domain=.$`Hotel count`, bins=breaks)


map<- leaflet(Joined) %>%
  # add basemap options
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB")%>%
  
  #add our polygons, linking to the tables we just made
  addPolygons(color="white", 
              weight = 2,
              opacity = 1,
              dashArray = "3",
              popup = popupairbnb,
              fillOpacity = 0.7,
              fillColor = ~pal2(`Airbnb count`),
              group = "Airbnb")%>%
  
  addPolygons(fillColor = ~pal2(`Hotel count`), 
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              popup = popupairbnb,
              fillOpacity = 0.7,group = "Hotels")%>%
  # add a legend
  addLegend(pal = pal2, values = ~`Hotel count`, group = c("Airbnb","Hotel"), 
            position ="bottomleft", title = "Accomodation count") %>%
  # specify layers control
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Toner Lite", "CartoDB"),
    overlayGroups = c("Airbnb", "Hotels"),
    options = layersControlOptions(collapsed = FALSE)
  )

# plot the map
map























