library(janitor)
library(sf)
library(tidyverse)
LondonWards <- st_read("///Users/lwj/Desktop/statistical-gis-boundaries-london/ESRI/London_Ward.shp")
LondonWardsMerged <- st_read("///Users/lwj/Desktop/statistical-gis-boundaries-london/ESRI/London_Ward_CityMerged.shp")%>%
  st_transform(.,27700)
WardData <- read_csv("///Users/lwj/Desktop/ward-profiles-excel-version.csv",
                     locale = locale(encoding = "latin1"),
                     na = c("NA", "n/a")) %>% 
  clean_names() #clean_names对列名称做清洗，空格将会变成下划线，转化为小写字母

LondonWardsMerged <- LondonWardsMerged %>% 
  left_join(WardData, 
            by = c("GSS_CODE" = "new_code"))%>%
  dplyr::distinct(GSS_CODE, .keep_all = T)%>% #去重处理,删除重复行
  dplyr::select(GSS_CODE, ward_name, average_gcse_capped_point_scores_2014) #仅保留选中列

#have a look to check that it's 
#in the right projection
st_crs(LondonWardsMerged)

#Always plot the data to check it looks reasonable
library(tmap)
BluePlaques <- st_read(here::here("///Users/lwj/Desktop/open-plaques-london-2018-04-08.geojson")) %>%
  st_transform(.,27700)
tmap_mode("plot")
tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(col = "blue")

summary(BluePlaques)
#lose the blue plaques that fall outside of London
BluePlaquesSub <- BluePlaques[LondonWardsMerged,]
tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")


# count all of the blue plaques that fall within each Ward in the City
example<-st_intersects(LondonWardsMerged, BluePlaquesSub)
example

check_example <- LondonWardsMerged%>%
  st_join(BluePlaquesSub)%>%
  filter(ward_name=="Kingston upon Thames - Coombe Hill")


library(sf)
points_sf_joined <- LondonWardsMerged%>%
  mutate(n = lengths(st_intersects(., BluePlaquesSub)))%>%
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per ward
  mutate(density=n/area)%>%
  #select density and some other variables 
  dplyr::select(density, ward_name, gss_code, n, average_gcse_capped_point_scores_2014)


points_sf_joined<- points_sf_joined %>%                    
  group_by(gss_code) %>%         
  summarise(density = first(density),
            wardname= first(ward_name),
            plaquecount= first(n))

tm_shape(points_sf_joined) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("wardname", "density"),
              title="Blue Plaque Density")


install.packages("spdep")
library(spdep)
#calculate the centroids of all Wards in London
coordsW <- points_sf_joined%>%
  st_centroid()%>%
  st_geometry()
plot(coordsW,axes=TRUE)


#generate a spatial weights matrix 
#create a neighbours list
#create a neighbours list
LWard_nb <- points_sf_joined %>%
  poly2nb(., queen=T)
summary(LWard_nb)

#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")
#add a map underneath
plot(points_sf_joined$geometry, add=T)


# Matrix style
#create a spatial weights matrix from these weights
Lward.lw <- LWard_nb %>%
  nb2mat(., style="B")
sum(Lward.lw) 

# global standardisation  
#dividing our 625 wards by the total number of neighbours
#meaning each spatial weight has a value of 0.169

# row standardisation
sum(Lward.lw[1,]) #row 1 here sums to 6


#Autocorrelation
Lward.lw <- LWard_nb %>%
  nb2listw(., style="C") #transform matrix to list
#Moran’s I test 
I_LWard_Global_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  moran.test(., Lward.lw)
I_LWard_Global_Density



#Geary’s C
C_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  geary.test(., Lward.lw)
C_LWard_Global_Density



#Getis Ord 热点分析
G_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)
G_LWard_Global_Density




#use the localmoran function to generate I for each ward in the city
I_LWard_Local_count <- points_sf_joined %>%
  pull(plaquecount) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

I_LWard_Local_Density <- points_sf_joined %>% #主要使用
  pull(density) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

#what does the output (the localMoran object) look like?
slice_head(I_LWard_Local_Density, n=5)

points_sf_joined <- points_sf_joined %>%
  mutate(plaque_count_I = as.numeric(I_LWard_Local_count$Ii))%>%
  mutate(plaque_count_Iz =as.numeric(I_LWard_Local_count$Z.Ii))%>%
  mutate(density_I =as.numeric(I_LWard_Local_Density$Ii))%>%
  mutate(density_Iz =as.numeric(I_LWard_Local_Density$Z.Ii))

#Mapping
#set the breaks manually
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
library(RColorBrewer)
MoranColours<- rev(brewer.pal(8, "RdGy"))

#Plot on an interactive map
tm_shape(points_sf_joined) +
  tm_polygons("plaque_count_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Blue Plaques in London")




#Local Getis Ord Gi
Gi_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localG(., Lward.lw)

head(Gi_LWard_Local_Density)

points_sf_joined <- points_sf_joined %>%
  mutate(density_G = as.numeric(Gi_LWard_Local_Density))

library(RColorBrewer)

GIColours<- rev(brewer.pal(8, "RdBu"))

#now plot on an interactive map
tm_shape(points_sf_joined) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Blue Plaques in London")



#Average GSCE scores
#use head to see what other variables are in the data file

slice_head(points_sf_joined, n=2)
#need to drop the geometry
Datatypelist <- LondonWardsMerged %>% 
  st_drop_geometry()%>%
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

I_LWard_Local_GCSE <- LondonWardsMerged %>%
  arrange(GSS_CODE)%>%
  pull(average_gcse_capped_point_scores_2014) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

points_sf_joined <- points_sf_joined %>%
  arrange(gss_code)%>%
  mutate(GCSE_LocIz = as.numeric(I_LWard_Local_GCSE$Z.Ii))


tm_shape(points_sf_joined) +
  tm_polygons("GCSE_LocIz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, GCSE Scores")



G_LWard_Local_GCSE <- LondonWardsMerged %>%
  dplyr::arrange(GSS_CODE)%>%
  dplyr::pull(average_gcse_capped_point_scores_2014) %>%
  as.vector()%>%
  localG(., Lward.lw)

points_sf_joined <- points_sf_joined %>%
  dplyr::arrange(gss_code)%>%
  dplyr::mutate(GCSE_LocGiz = as.numeric(G_LWard_Local_GCSE))

tm_shape(points_sf_joined) +
  tm_polygons("GCSE_LocGiz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, GCSE Scores")




























