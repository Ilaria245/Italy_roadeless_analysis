library(dplyr)
library(terra)
library(sf)
library(tidyverse)
library(stars)
library(units)
library(ggplot2)
library(usdm)


setwd('project_path')

#loading data: street and geographical region

###in my case (Italy) the data from OMS were divided in sub-regions, so i downloaded the different Italian regions and combine them in order to match OMS's sub-regions,
### this doesn't necessary have to be your case, 
###if so here there's the code to select the region and combine them together 

#geographical regions

# load all the Italian's regions
gadm_regions<-st_read('data_path/gadm/gadm41_ITA_1.shp')

### select, combine  and change the coordinates. here an e.g with the north-eastern region ( one out of five) 
first_region_gadm<-gadm_regions[gadm_regions$GID_1%in%c('ITA.7_1','ITA.17_1','ITA.20_1','ITA.6_1'),]%>%
                      st_union()%>%
                      st_boundary()%>%   # with this line I created the new polygon's edge
                      st_transform('EPSG:3035')


regions<-list(first_region_gadm,second_region_gadm,third_region_gadm,fourth_region_gadm,fifth_region_gadm)

### lines to polygons 
regions <- lapply(regions, "st_polygonize")


#road data

#load the OSM data
###(remember, for Italy the data are divided in 5 sub-regions. Each step must be repeated for all of them. If you're lucky, the data might be available for the entire country at once)

first_region_OSM_street<- st_read('data_path/roads.shp')   
first_region_OSM_railway<- st_read('data_path/railways.shp')

# remove the tunnel from the dataset. remove the columns that aren't the identificative  code and the geometry 
first_region_OSM_street<-filter(first_region_OSM_street,tunnel=='F')%>%
                          select(code,geometry)
first_region_OSM_railway<-filter(first_region_OSM_railway, tunnel == 'F')%>%
                          select(code,geometry)

# combine the two dataset
first_region_road<-bind_rows(first_region_OSM_railway,first_region_OSM_street) 


#filter the road of interest by code (select the code of your interest ) ###I used a loop to speed of everything because my data were divided

###crate a single list with all my sub-regions
road<- list(first_region_road,second_region_road,third_region_road,fourth_region_road,fifth_region_road )
### output list
road_fitered <- list() 

  for(i in 1:length(road)){
     result <- road[[i]] 
     result<- result[ result$code %in% c('5111','5112','5113','5114','5115','5121','5122','5123','5124','5125','5131','5132','5133','5134','5135','5141','5142','5143','6101','6102','6105','6106','6107','6108','6109'),]
     result<- st_transform( result,crs='EPSG:3035')
     road_fitered <- c(road_fitered,list(result))
     print(i)
   }

###########################


### now we have our sub-region shape and our road of interest
# Let's proceed with the application of 1km buffer to the roads, followed by the selection of the common area between the buffer and the regions 
# by doing these steps we'll create a  "mask" that we'll later subtract to the regions to obtain the non-roaded surfaces.

###since my data were divided in sub regions I needed to create a function and a loop 


# define a function to apply a 1 km buffer around the roads 
buff_selection_function<-function(road_filtered,regions){
  road_buffer<-road_filtered%>%
    st_buffer(dist=1000)%>%
    st_union()%>%    #
    st_intersection(regions)%>%  
    st_collection_extract() %>% 
    st_cast("POLYGON")
  return(road_buffer)
}

#output vector
result_buffered <-vector('list',length(road_filtered))  # it is more efficient to create a vector in term of memory usage cos

for (i in 1:length(road_filtered)) {
  result <- buff_selection_function(road_filtered[[i]], regions[[i]])    #make sure that your dataset matches correctly (first_sub_reg_road with fist_region, second with second etc.)
  result_buffered[[i]] <- result
}

#let's subtract the just obtained masks to the regions

# define the function to obtain the roadless surface (regions - buffer --> roadless areas)

roadless_function<-function(regions,road_buffer){
  roadless_patch<-regions%>%
    st_difference(st_union(road_buffer))%>%
    st_cast("POLYGON")
  return(roadless_patch)
}

### apply the function to the regions
roadless_results <- vector('list', length(result_buffered))
for (i in 1:length(result_buffered)) {
  result <- roadless_function(regions[[i]], result_buffered[[i]])
  roadless_results[[i]] <- result
}


###########################

#In my internship, I considered only those surfaces that were at least 1 km² large.


#set the coordinates
crs <- st_crs('EPSG:3035')  


# crate the function to filter the surfaces
filter_function <- function(unfiltered_polygons) {
  filtered_polygons <- lapply(unfiltered_polygons, function(polygon) {
    # calculating the area
    area <- st_area(polygon)
    
    # keep only those that are >  km²
    if (area > 1e6) {
      return(polygon)
    } else {
      return(NULL)
    }
  })
  
  # filter out the null polygon
  filtered_polygons <- filtered_polygons[!sapply(filtered_polygons, is.null)]
  
  
  # create a now sfc object which only contain filtered polygons
  filtered_polygon_sfc <- st_sfc(filtered_polygons, crs = crs)
  return(filtered_polygon_sfc)
}


filtered_polygons_sfc <- lapply(roadless_results, filter_function) # onece again, my data were divided 

  
#now we have our polygon of interest

###########################

#then i applied 1km internal buffer and right after a 1km external buffer to the same elements.
#This process excludes narrow and/or irregular surfaces, resulting in a reduced edge effect.

buffers <- function(polygons_list) {
  buffered_polygons <- lapply(polygons_list, function(polygon) {
    # internal buffer
    internal_buffer <- st_buffer(polygon, dist = -1000)
    
    # external buffer
    external_buffer <- st_buffer(internal_buffer, dist = 1000)
    return(external_buffer)
  })
  
  # keeping the object crs and class 
  buffered_polygons_sfc <- st_sfc(buffered_polygons, crs = crs)
  return(buffered_polygons_sfc)
}

# apply the function to the previous filtered polygons 
buffered_roadless_areas <- lapply(filtered_polygons_sfc, buffers) # divided data


###########################


# let's subtract the geometries the contain more the 70% of water 



eea_waw<-rast('path_raster_internal_water')  # this raster were a tiff containing all type of internal waters  
eea_waw_df<-as.data.frame(eea_waw,xy=TRUE)  

# since i only needed the lake i made the lake selection

#select only the permanent waters
permanent_water2_df<-eea_waw_df[eea_waw_df$WAW_2018_100m_eu_03035_V1_0 == 2,]## in my case permanent waters were assigned to the number 2  

# convert my df into a raster
permanent_water_raster <- rasterFromXYZ(permanent_water2_df)

#convert to a polygon and than to an sf object. this allows me to work with the previous created polygons
permanent_water_as_polygon <- terra::as.polygons(permanent_water_raster)
permanent_water_sf <- sf::st_as_sf(permanent_water_as_polygon)%>%st_cast('POLYGON')%>%st_set_crs(crs)

# select only the geometry column 
permanent_water<-permanent_water_sf$geometry



### Since my data were divided, I worked on each sub-region separately (without creating a loop, which should not be done).
#Here I provide the code used for just one sub-region

#create a list that will contain the final result for one region (or the entire nation)
no_water_first_subreg <- list()

#  clean the data from empty polygons. (1 stands for the fist sub-region. do the same for each regions or implement the loop by adding another loop )
for (i in 1:length(buffered_roadless_areas[[1]])) {
  selected_line <- buffered_roadless_areas[[1]][[i]]
  if (length(selected_line) > 0) {
    no_water_first_subreg <- append(no_water_first_subreg, list(selected_line))
  }
}

# here a for loop is need in order to ally these step to each geometry

for(i in 1:length(no_water_first_subreg)){
  
  no_water_first_subreg[[i]]<-no_water_first_subreg[[i]]%>%
    st_sfc(crs = st_crs(permanent_water) )
}

# create a empty sf object
first_subreg_no_water <- st_sf(geometry = st_sfc(), crs = st_crs('EPSG:3035'))

# set the desired threshold
water_threshold <- 0.7

#  select only those polygon that contain less than 70% of water
for (i in 1:length(no_water_first_subreg)) {
  polygon <- no_water_first_subreg[[i]]
  

  intersection <- st_intersection(polygon, permanent_water)  
  
  if (length(intersection) == 0) {
    intersection_percentage <- 0
  } else {
    
    polygon_area <- st_area(polygon)
    
    intersection_area <- sum(st_area(intersection))
    
    intersection_percentage <- unclass(intersection_area / polygon_area)
  }
  
  if (intersection_percentage <= water_threshold) {
    polygon_sf <- st_sf(polygon)%>%
                  st_transform( crs = st_crs(first_subreg_no_water))
    first_subreg_no_water <- rbind(first_subreg_no_water, polygon_sf)
  }
}


final_geometries <- rbind(first_subreg_no_water, 
                     second_subreg_no_water, 
                     third_subreg_no_water, 
                     fourth_subreg_no_water, 
                     fifth_subreg_no_water)



# now we have our final polygons :) (i hope it worked for you as well )

#########################################################
# The next step was to create a stack with all the rasters I wanted to analyze.
# In my case, these included elevation, water availability (expressed as the ptalpha coefficient), forest coverage, crop suitability, and biogeographical region.

# Import all the rasters of interest using the "rast" function. Some of the rasters may be divided into more than one part.
# If you need to download multiple rasters to cover the entire country of interest, you can merge them using the "merge" function.


#e.g 1
 wa<-rast('path/water_availability.tiff')

# e.g 2
dem30x0<-rast('analysis/input/eurostat elevation/10_DEM_y30x0/10_DEM_y30x0.tif')
dem30x10<-rast('analysis/input/eurostat elevation/10_dem_y30x10/10_DEM_y30x10.tif')
dem40x0<-rast('analysis/input/eurostat elevation/10_DEM_y40x0/10_DEM_y40x0.tif')
dem40x10<-rast('analysis/input/eurostat elevation/10_DEM_y40x10/10_DEM_y40x10.tif')
elevation<-merge(dem30x0,dem30x10,dem40x0,dem40x10)

# since the data are unlikely provided for a single country, you need a shapefile or raster file of the country in question
# in my case i downloaded the a shape file from the website : GADM 

shp_italy<-st_read('path/gadm41_ITA_0.shp')
shp_it<-shp_italy$geometry
shp_it_sf<-st_sf(shp_it)

# Then I cropped the rasters using the shapefile of Italy, which resulted in smaller rasters and faster analysis. (When you crop, you modify the extent; it will not necessarily match the shape of Italy.)
# This step only works if both elements have the same CRS. Converting worldwide rasters can be time-consuming, so first we assign the raster's CRS to the shapefile, then we crop it.
# Finally, we reassign the desired CRS.


raster_list<-list(crop_suit,elevation,forest,wa,bioge_regions)
output_path <- "path/" 
names(raster_list) <- c("crop_suit", "elevation", "forest", "bioge_regions")


for(i in seq_along (raster_list)){
  raster<-raster_list[[i]]
  shp_it_sf<-st_transform(shp_it_sf,crs(raster))
  raster <- crop(x=raster,y=shp_it_sf)
  
  
  nome_raster_cropped <- paste0("cropped_", names(raster_list)[i], ".tif")
  full_path <- file.path(output_path, nome_raster_cropped)
  
  
  writeRaster(raster,full_path)
  cat("Raster", names(raster_list)[i])
}


# Now that we have cropped the rasters, we need to reproject them so they can be stacked together.
#fist create a empty template

template <- terra::rast(shp_it_sf,
                        ext=ext(shp_it_sf), 
                        resolution = 1000, 
                        crs="epsg:3035")  

#e.g: how to reproject a single raster
wa_final <- terra::project(wa, template)

# if you re dealing with discrete variable (for example, in my case the biogeographical regions was coded as 5  7 an 9 ) when you reproject the raster new value could be created
# but they might not have any meaning so add method = 'near' to avoid the creation of new values 
biogeo_final <- terra::project(bioge_regions, template, method="near")


# it might be usefull to rename the elements inside our raster
#e.g
names(wa_final) <- "water_av"  

# we also have to rasterize the roadless geometries
# first create a vector
roadless_vect <- vect(final_geometries %>% 
                        mutate(is_roadless=1))
raster_roadless <- terra::rasterize(roadless_vect, template, field="is_roadless") 
raster_roadless<- subst(raster_roadless, NA, 0)  # chage the NA value to 0 value


# then we also need to rasterize the italian shp file 
raster_shape_italia<- terra::rasterize(shp_it_sf %>% mutate(is_italy=1), template, field="is_italy")     

#stack them together
mystack<-c(raster_shape_italia,bioge_regions,raster_roadless,wa,elevation,crop_suit,forest)


# crate a df

mydata <- as.data.frame(mystack) %>% 
  as_tibble() %>% 
  filter(!is.na(is_italy)) %>%    # this line allows us to keep only the pixels that actually represent Italy
  mutate(biogeo=factor(biogeo, levels=c(1,7, 9), labels=c("Alpine", "Continental", "Mediterranean"))) %>%  
  mutate(is_roadless=factor(is_roadless))



#NOW YOU SHOULD HAVE YOUR DF AND BE ABLE TO TO ALL THE STATISTIC YOU WANT 



# i would like to share something about my code
#I'm not showing every single step and analyses, just some examples.


# SOME ANALYSES 


# i calculated the total the surface of the total area per each biogeografical region

#if you aim to calcutate only the readless surface you just have to add filter (is_roadless=1)
result <- mydata %>%
  filter(is_italy==1)%>%
  group_by(biogeo) %>% 
  summarise(pixel = n()) %>% 
  mutate(`area m^2` = pixels * 30 * 30, #multiply by pixel area (mind the resolution)
         `area km^2` = round(`area m^2`/1e6,2))  # the function round  is used to round number, in my case i decided to keep 2 decimal place



# i calculated the mean for each variable within each bio region

result <- mydata %>% 
  group_by(biogeo, is_roadless) %>% 
  summarize(n=n(),
            wa=signif(mean(wa,na.rm=TRUE),digits = 4),    # round e signif do more or less the same thing. the second account for significant digits wherever  the decimal point is
            elevation=signif(mean(`elevation`),digits = 6),
            crop=signif(mean(agri_suit,na.rm=TRUE),digits=2),
            forest=signif(mean(forest,na.rm=TRUE),digits = 3))


# In order to test whether there were differences in the distribution of the variables between the road and roadless data, I performed a Mann-Whitney test.
# (It is suitable when the variable does not follow a normal distribution.)


bg<-list("Alpine","Continental","Mediterranean")
variable <- names(mydata)[4:7]


for( i in 1: length (bg)){
  
  for(j in 1: length(variable))
  { 
    bioreg<-bg[i]
    
    var_name<-variable[j]
    
    test_mannwhitney<- with(mydata, 
                            wilcox.test(
                              mydata[[var_name]][is_roadless == "0" & biogeo == bioreg],    # mydata[[var_name]] allows to access to the variable in the df ,
                              mydata[[var_name]][is_roadless == "1" & biogeo == bioreg]     #you should be alble to do the same staff using get(var_name) (not 100% sure)

                            ))
                                       
      }
}






#SOME GRAPH

# Some correlation graph
# this type of graph give you also the correlation test value

library(GGally)

# i divided the analysis between the different regions (Alpina, Continenal, Mediterranean)
#here an e.g for the Alpine region

alpine_data<-mydata%>% 
  filter(biogeo=="Alpine")%>%group_by( is_roadless) %>% slice_sample(n=1000)

alpine_data$is_roadless <- as.factor(alpine_data$is_roadless)


# different colors for road and roadless


corr_graph<-ggpairs(alpine_data,columns = 4:7,    #these are the columns of interest
                    columnLabels = c("Elevation (m)", "PTalpha coefficient", "Crop Suitability (%)", "Forest Coverage (%) "),
                    mapping=aes(colour=is_roadless,alpha=0.3),
                    upper = list(continuous = wrap("cor", method = "spearman")) )+
  scale_color_manual(values = c("road" = "#084594", "roadless" = "#ce1256")) +
  scale_fill_manual(values = c("road" = "#084594", "roadless" = "#ce1256")) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color = "black"),
    axis.text.y = element_text(vjust = 2) )

#same color

corr_graph_one_color<-ggpairs(
  alpine_data, 
  columns = 4:7,
  columnLabels = c("PTalpha coefficient", "Elevation (m)", "Crop Suitability (%)", "Forest Coverage (%)"),
  upper = list(continuous = "cor", method = "spearman"),
  diag = list(continuous = 'blankDiag'),
  mapping = aes(colour = "points", alpha = 0.3)
) +
  scale_color_manual(values = c(points = "#006400")) +  
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color = "black"),
    axis.text.y = element_text(vjust = 2)
  )





# i have done some graph and here is the palet i created for the elevetion graph, took me a while to find the "perfect" one, hope it might be useful
elevation_palette <- c("#4E7942", "#8EAB43", "#D1E05B", "#F4A460", "#E79D45", "#B85F21", "#8B3626", "#654321","#5D432C","#2E1D12","#1F1409")

# elevation plot
elevationplot <- ggplot(data = mydataplot) +
  geom_tile(aes(x = x, y = y, fill = elevation)) +
  geom_sf(data=shp_it_sf, fill=NA, col=gray(0.7),size=0.002) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(2, "cm"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  guides(fill = guide_colorbar(title="Elevation (m)",title.position = "top",title.hjust = 0.5))+
  scale_fill_gradientn(colors = elevation_palette)






#going forward with MY analyses i also investigate the actually protected areas against the roadless areas 
#here how i plotted them 

my_stack2<-c(raster_shape_italia,bioge_regions,raster_roadless,protected_italian_areas)

my_data2<-as.data.frame(my_stack2,xy=TRUE)%>%
  as_tibble()%>%
  filter(!is.na(is_italy))%>%
  mutate(biogeo=factor(biogeo, levels=c(1,7,9), labels=c("Alpine", "Continental", "Mediterranean"))) 


my_data2$is_protected[is.na(my_data2$is_protected)] <- 0

#create a new column
my_data2$result<- ifelse(my_data2$is_protected == 6 & my_data2$is_roadless == 0, 6,
                            ifelse(my_data2$is_protected == 6 & my_data2$is_roadless == 5, 7,
                                   ifelse(my_data2$is_protected== 0 & my_data2$is_roadless == 5, 5,
                                          ifelse(my_data2$is_protected== 0 & my__data2$is_roadless == 0, 1, NA)
                                   )
                            )
)

my_data2$result <- as.factor(my_data2$result)

#define the palette
PA_col <- c("1"="NA","5" = "yellow", "6" = "blue", "7" = "green")


ggplot(my_data2)+
  geom_sf(data=shp_it_sf, fill="lightgray", col=gray(1),size=0.002)+
  geom_tile(aes(x=x, y=y, fill= result)) +
  scale_fill_manual(values =PA_col)+
  theme_minimal()




# HOPE THIS MIGHT HELP :))



